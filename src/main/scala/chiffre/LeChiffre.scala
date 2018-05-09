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
package chiffre

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile.{
  LazyRoCC,
  LazyRoCCModuleImp,
  HasCoreParameters,
  HasL1CacheParameters,
  OpcodeSet}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tilelink.{
  TLClientNode,
  TLClientPortParameters,
  TLClientParameters}

import perfect.util._
import perfect.random._

class LeChiffre(opcode: OpcodeSet, scanId: String)
               (implicit p: Parameters) extends LazyRoCC(opcode) {
  override lazy val module = new LeChiffreModuleImp(this, scanId)
  override val atlNode = TLClientNode(
    Seq(TLClientPortParameters(Seq(TLClientParameters("LeChiffreRoCC")))))
}

class LeChiffreModuleImp(outer: LeChiffre, val scanId: String)
                        (implicit p: Parameters)
    extends LazyRoCCModuleImp(outer)
    with HasCoreParameters
    with HasL1CacheParameters
    with LeChiffreH
    with FletcherH
    with UniformPrintfs
    with ChiffreController {
  val cacheParams = tileParams.icache.get
  override val printfSigil = "LeChiffre: "

  io.busy := false.B
  io.interrupt := false.B
  io.mem.req.valid := false.B

  val funct = io.cmd.bits.inst.funct
  val do_echo             = io.cmd.fire() & funct === f_ECHO.U
  val do_cycle            = io.cmd.fire() & funct === f_CYCLE.U
  val do_enable           = io.cmd.fire() & funct === f_ENABLE.U
  val do_unknown          = io.cmd.fire() & funct  >  f_ENABLE.U

  val Seq(s_WAIT, s_CYCLE_TRANSLATE, s_CYCLE_READ, s_CYCLE_QUIESCE, s_RESP,
          s_ERROR) = Enum(6) // scalastyle:ignore

  val state = RegInit(s_WAIT)

  val checksum = Reg(UInt(checksumWidth.W))
  val Seq(cycle_count, read_count, cycles_to_scan) =
    Seq.fill(3)(Reg(UInt(cycleWidth.W))) // scalastyle:off
  val Seq(rd_d, rs1_d, rs2_d, resp_d) =
    Seq.fill(4)(Reg(UInt())) // scalastyle:off

  val fletcher = Module(new Fletcher(checksumWidth))

  io.cmd.ready := state === s_WAIT

  when (io.cmd.fire() && state === s_WAIT) {
    rd_d := io.cmd.bits.inst.rd
    rs1_d := io.cmd.bits.rs1
    rs2_d := io.cmd.bits.rs2
  }

  when (do_echo) {
    state := s_RESP
    resp_d := io.cmd.bits.rs1
  }

  when (do_cycle) {
    // [todo] figure out how to handle virtual memory
    // state := { if (usingVM) s_CYCLE_TRANSLATE else s_CYCLE_READ }
    state := s_CYCLE_READ
    cycle_count := 0.U
    read_count := 0.U
    cycles_to_scan := 0.U - 1.U
    printfInfo("Cycling: addr 0x%x\n", io.cmd.bits.rs1)
  }

  scan.en := do_enable
  when (do_enable) {
    state := s_RESP
    resp_d := 0.U
  }

  when (state === s_CYCLE_TRANSLATE) {
    state := s_ERROR
    printfError("Address translation not implemented\n")
  }

  val (tl_out, edgesOut) = outer.atlNode.out(0)

  tl_out.a.valid := false.B
  tl_out.d.ready := true.B

  // Parallel-in, Serial-out handling
  val piso = Module(new Piso(cacheDataBits)).io
  val reqSent = RegInit(false.B)
  piso.p.valid := reqSent & tl_out.d.fire() & read_count =/= 0.U
  piso.p.bits.data := tl_out.d.bits.data
  piso.p.bits.count := (cacheDataBits - 1).U
  scan.clk := piso.s.valid
  scan.out := piso.s.bits
  fletcher.io.data.bits.cmd := k_compute.U
  when (piso.s.valid) {
    cycle_count := cycle_count + 1.U
  }

  // Checksum computation
  val fletcherWords = RegEnable(
    tl_out.d.bits.data
      .asTypeOf(Vec(cacheDataBits/(checksumWidth/2), UInt((checksumWidth/2).W))),
    piso.p.valid)
  val idx = cycle_count(log2Ceil(cacheDataBits) - 1, log2Ceil(checksumWidth/2))
  fletcher.io.data.bits.word := fletcherWords(idx)
  val last = cycle_count === cycles_to_scan - 1.U
  fletcher.io.data.valid := piso.s.valid && (last ||
    cycle_count(log2Ceil(checksumWidth / 2) - 1, 0) ===
    (checksumWidth / 2 - 1).U )

  // AUTL Acq/Gnt handling
  val addr_d = rs1_d
  val addr_block = addr_d(coreMaxAddrBits - 1, log2Ceil(xLen/8)) // [todo] fragile
  tl_out.a.bits := edgesOut.Get(
    fromSource = 0.U,
    toAddress = addr_block << log2Ceil(xLen/8),
    lgSize = log2Ceil(xLen/8).U)._2 // [todo] fragile

  /* [todo] Remove this once this is better validated */
  // println(s"""|[info] blockOffBits: $blockOffBits
  //             |[info] cacheDataBits: $cacheDataBits
  //             |[info] lgCacheBlockBytes: $lgCacheBlockBytes""".stripMargin)

  def autlAcqGrant(ready: Bool = true.B): Bool = {
    val done = reqSent & tl_out.d.fire()
    when (!reqSent & ready) {
      tl_out.a.valid := true.B
      reqSent := tl_out.a.fire()
    }
    when (done) {
      reqSent := false.B
    }
    done
  }

  /* This contains no logic handling for when the {checksum, length} is
   * not equal to one data unit (cacheDataBits in length) coming back
   * over TileLink */
  require((checksumWidth + cycleWidth) == cacheDataBits,
          "Header (checksum, cycles) of Chiffre Scan must be equal to cacheDataBits")
  when (state === s_CYCLE_READ) {
    when (autlAcqGrant(piso.p.ready)) {
      read_count := read_count + xLen.U
      addr_d := addr_d + rowBytes.U
      when (read_count === 0.U) {
        val msbs = tl_out.d.bits.data(cycleWidth + checksumWidth - 1, cycleWidth)
        val lsbs = tl_out.d.bits.data(cycleWidth - 1, 0)
        checksum := msbs
        cycles_to_scan := lsbs
        printfInfo("Checksum: 0x%x\n", msbs)
        printfInfo("Bits to scan: 0x%x\n", lsbs)
      }
      when (read_count >= cycles_to_scan) {
        piso.p.bits.count := cacheDataBits.U - read_count + cycles_to_scan - 1.U
        state := s_CYCLE_QUIESCE
      }
    }
  }

  when (state === s_CYCLE_QUIESCE) {
    when (piso.p.ready) { state := s_RESP }
    resp_d := checksum =/= fletcher.io.checksum
  }

  io.resp.valid := state === s_RESP
  when (state === s_RESP) {
    io.resp.bits.data := resp_d
    io.resp.bits.rd := rd_d
    state := s_WAIT

    fletcher.io.data.valid := true.B
    fletcher.io.data.bits.cmd := k_reset.U
  }

  when (state === s_ERROR) {
  }

  when (io.cmd.fire()) { printfInfo("cmd 0x%x, rs1 0x%x, rs2 0x%x\n",
    io.cmd.bits.inst.asUInt, io.cmd.bits.rs1, io.cmd.bits.rs2)
    printfInfo("  status 0x%x\n", io.cmd.bits.status.asUInt())
    printfInfo("   -> fs 0x%x\n", io.cmd.bits.status.fs)
    printfInfo("   -> xs 0x%x\n", io.cmd.bits.status.xs)
  }
  when (io.resp.fire()) { printfInfo("Chiffre: resp rd 0x%x, data 0x%x\n",
    io.resp.bits.rd, io.resp.bits.data) }

  when (tl_out.a.fire()) {
    printfInfo("tl_out.a fire | opcode 0x%x, param 0x%x, size 0x%x, source 0x%x, address 0x%x, mask 0x%x, data 0x%x\n",
      tl_out.a.bits.opcode, tl_out.a.bits.param, tl_out.a.bits.size,
      tl_out.a.bits.source, tl_out.a.bits.address, tl_out.a.bits.mask,
      tl_out.a.bits.data) }

  when (reqSent && tl_out.d.fire()) {
    printfDebug("tl_out.d | opcode 0x%x, param 0x%x, size 0x%x, source 0x%x, sink 0x%x, data 0x%x, error 0x%x\n",
      tl_out.d.bits.opcode, tl_out.d.bits.param, tl_out.d.bits.size,
      tl_out.d.bits.source, tl_out.d.bits.sink, tl_out.d.bits.data,
      tl_out.d.bits.error) }

  when (scan.clk) {
    printfInfo("Scan[%d]: %d, En: %d\n", cycle_count, scan.out, scan.en)
  }

  // Catch all error states
  when (do_unknown) { state := s_ERROR }
  assert(RegNext(state) =/= s_ERROR, "[ERROR] LeChiffre: Hit error state\n")
}
