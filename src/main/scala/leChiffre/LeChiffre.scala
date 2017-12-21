// Copyright 2017 IBM
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
package leChiffre

import chisel3._
import chisel3.util._
import rocket._
import cde._
import uncore.tilelink.{HasTileLinkParameters, Get, Put, GetBlock}

import perfect.util._
import perfect.random._

class LeChiffre(implicit p: Parameters) extends RoCC()(p) with UniformPrintfs
    with LeChiffreH with FletcherH with HasTileLinkParameters
    with ChiffreController {
  override lazy val io = new RoCCInterface
  override val printfSigil = "LeChiffre: "
  lazy val scanId = "main"

  io.busy := false.B
  io.interrupt := false.B
  io.mem.req.valid := false.B

  scan.clk := false.B

  val funct = io.cmd.bits.inst.funct
  val do_echo             = io.cmd.fire() & funct === f_ECHO.U
  val do_cycle            = io.cmd.fire() & funct === f_CYCLE.U
  val do_enable           = io.cmd.fire() & funct === f_ENABLE.U
  val do_unknown          = io.cmd.fire() & funct  >  f_ENABLE.U

  val s_ = Chisel.Enum(UInt(), List('WAIT,
    'CYCLE_TRANSLATE, 'CYCLE_READ, 'CYCLE_QUIESCE,
    'RESP, 'ERROR))
  val state = Reg(init = s_('WAIT))

  val checksum = Reg(UInt(checksumWidth.W))
  val Seq(cycle_count, read_count, cycles_to_scan) =
    Seq.fill(3)(Reg(UInt(cycleWidth.W))) // scalastyle:off
  val Seq(rd_d, rs1_d, rs2_d, resp_d) =
    Seq.fill(4)(Reg(UInt())) // scalastyle:off

  val fletcher = Module(new Fletcher(checksumWidth))

  io.cmd.ready := state === s_('WAIT)

  when (io.cmd.fire() && state === s_('WAIT)) {
    rd_d := io.cmd.bits.inst.rd
    rs1_d := io.cmd.bits.rs1
    rs2_d := io.cmd.bits.rs2
  }

  when (do_echo) {
    state := s_('RESP)
    resp_d := io.cmd.bits.rs1
  }

  when (do_cycle) {
    state := Mux(io.cmd.bits.status.vm === 0.U,
                 s_('CYCLE_READ), s_('CYCLE_TRANSLATE))
    cycle_count := 0.U
    read_count := 0.U
    cycles_to_scan := 0.U - 1.U
    printfInfo("Cycling: addr 0x%x\n", io.cmd.bits.rs1)
  }

  scan.en := do_enable
  when (do_enable) {
    state := s_('RESP)
    resp_d := 0.U
  }

  val fletcherWord = Reg(UInt((checksumWidth / 2).W))
  fletcher.io.data.bits.word := scan.out ## (fletcherWord >> 1)
  fletcher.io.data.valid := false.B

  when (state === s_('CYCLE_TRANSLATE)) {
    state := s_('ERROR)
    printfError("Address translation not implemented\n")
  }

  val acq = io.autl.acquire
  val gnt = io.autl.grant
  acq.valid := false.B
  gnt.ready := true.B

  // Parallel-in, Serial-out handling
  val piso = Module(new Piso(tlDataBits)).io
  val reqSent = Reg(init = false.B)
  piso.p.valid := reqSent & gnt.fire() & read_count =/= 0.U
  piso.p.bits.data := gnt.bits.data
  piso.p.bits.count := (tlDataBits - 1).U
  scan.clk := piso.s.valid
  scan.out := piso.s.bits
  val last = cycle_count === cycles_to_scan - 1.U
  when (piso.s.valid) {
    cycle_count := cycle_count + 1.U

    fletcherWord := scan.out ## (fletcherWord >> 1)
    when (cycle_count(log2Up(checksumWidth / 2) - 1, 0) ===
            (checksumWidth / 2 - 1).U || last) {
      fletcher.io.data.valid := true.B
      fletcher.io.data.bits.cmd := k_compute.U
    }
  }

  // AUTL Acq/Gnt handling
  val utlBlockOffset = tlBeatAddrBits + tlByteAddrBits
  val utlDataPutVec = Wire(Vec(tlDataBits / xLen, UInt(xLen.W)))
  val addr_d = rs1_d
  val addr_block = addr_d(coreMaxAddrBits - 1, utlBlockOffset)
  val addr_beat = addr_d(utlBlockOffset - 1, tlByteAddrBits)
  val addr_byte = addr_d(tlByteAddrBits - 1, 0)
  val addr_word = tlDataBits compare xLen match {
    case 1 => addr_d(tlByteAddrBits - 1, log2Up(xLen/8))
    case 0 => 0.U
    case -1 => throw new Exception("XLen > tlByteAddrBits (this doesn't make sense!)")
  }
  val get = Get(client_xact_id = 0.U,
    addr_block = addr_block,
    addr_beat = addr_beat,
    addr_byte = addr_byte,
    operand_size = MT_D,
    alloc = false.B)
  val getBlock = GetBlock(addr_block = addr_block, alloc = false.B)
  acq.bits := get

  def autlAcqGrant(ready: Bool = true.B): Bool = {
    val done = reqSent & gnt.fire()
    when (!reqSent & ready) {
      acq.valid := true.B
      reqSent := acq.fire()
    }
    when (done) {
      reqSent := false.B
    }
    done
  }

  /* This contains no logic handling for when the {checksum, length} is
   * not equal to one data unit (tlDataBits in length) coming back
   * over TileLink */
  require((checksumWidth + cycleWidth) == tlDataBits,
          "Header (checksum, cycles) of Chiffre Scan must be equal to tlDataBits")
  when (state === s_('CYCLE_READ)) {
    when (autlAcqGrant(piso.p.ready)) {
      read_count := read_count + xLen.U
      addr_d := addr_d + tlDataBytes.U
      when (read_count === 0.U) {
        val msbs = gnt.bits.data(cycleWidth + checksumWidth - 1, cycleWidth)
        val lsbs = gnt.bits.data(cycleWidth - 1, 0)
        checksum := msbs
        cycles_to_scan := lsbs
        printfInfo("Checksum: 0x%x\n", msbs)
        printfInfo("Bits to scan: 0x%x\n", lsbs)
      }
      when (read_count >= cycles_to_scan) {
        piso.p.bits.count := tlDataBits.U - read_count + cycles_to_scan - 1.U
        state := s_('CYCLE_QUIESCE)
      }
    }
  }

  when (state === s_('CYCLE_QUIESCE)) {
    when (piso.p.ready) { state := s_('RESP) }
    resp_d := checksum =/= fletcher.io.checksum
  }

  io.resp.valid := state === s_('RESP)
  when (state === s_('RESP)) {
    io.resp.bits.data := resp_d
    io.resp.bits.rd := rd_d
    state := s_('WAIT)

    fletcher.io.data.valid := true.B
    fletcher.io.data.bits.cmd := k_reset.U
  }

  when (state === s_('ERROR)) {
  }

  when (io.cmd.fire()) { printfInfo("cmd 0x%x, rs1 0x%x, rs2 0x%x\n",
    io.cmd.bits.inst.asUInt, io.cmd.bits.rs1, io.cmd.bits.rs2)
    printfInfo("  status 0x%x\n", io.cmd.bits.status.asUInt())
    printfInfo("   -> fs 0x%x\n", io.cmd.bits.status.fs)
    printfInfo("   -> xs 0x%x\n", io.cmd.bits.status.xs)
    printfInfo("   -> vm 0x%x\n", io.cmd.bits.status.vm)
  }
  when (io.resp.fire()) { printfInfo("Chiffre: resp rd 0x%x, data 0x%x\n",
    io.resp.bits.rd, io.resp.bits.data) }

  when (acq.fire()) {
    printfInfo("autl acq.%d | addr 0x%x, addr_block 0x%x, addr_beat 0x%x, addr_byte 0x%x\n",
      acq.bits.a_type, addr_d, acq.bits.addr_block, acq.bits.addr_beat,
      acq.bits.addr_byte())
  }

  when (reqSent && gnt.fire()) {
    printfDebug("autl.gnt | data 0x%x, beat 0x%x\n",
      gnt.bits.data, gnt.bits.addr_beat) }

  when (scan.clk) {
    printfInfo("Scan[%d]: %d, En: %d\n", cycle_count, scan.out, scan.en)
  }

  // Catch all error states
  when (do_unknown) { state := s_('ERROR) }
  assert(RegNext(state) =/= s_('ERROR), "[ERROR] LeChiffre: Hit error state\n")
}
