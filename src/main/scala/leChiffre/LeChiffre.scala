// See LICENSE.ibm for license details.
package leChiffre

import chisel3._
import chisel3.util._
import rocket._
import config._
import perfect.util._
import uncore.tilelink.{HasTileLinkParameters, Get, Put, GetBlock}

trait ScanChainIO {
  val SCAN_clk = Output(Bool())
  val SCAN_en = Output(Bool())
  val SCAN_in = Input(Bool())
  val SCAN_out = Output(Bool())
}

class LeChiffre(implicit p: Parameters) extends RoCC()(p) with UniformPrintfs
    with LeChiffreH with FletcherH with HasTileLinkParameters {
  override lazy val io = new RoCCInterface with ScanChainIO

  io.busy := false.B
  io.interrupt := false.B
  io.mem.req.valid := false.B

  io.SCAN_clk := false.B

  val ptw = io.ptw(0)
  ptw.req.bits.store := false.B
  ptw.req.bits.fetch := false.B

  val do_echo = io.cmd.fire() & io.cmd.bits.inst.funct === f_ECHO.U
  val do_cycle = io.cmd.fire() & io.cmd.bits.inst.funct === f_CYCLE.U
  val do_unknown = io.cmd.fire() & io.cmd.bits.inst.funct > f_CYCLE.U

  val s_ = Enum(UInt(), List('WAIT,
    'CYCLE_TRANSLATE, 'CYCLE_READ, 'CYCLE_QUIESCE,
    'RESP, 'ERROR))
  val state = Reg(init = s_('WAIT))
  val cycle_count = Reg(UInt(64.W))
  val read_count = Reg(UInt(64.W))
  val cycles_to_scan = Reg(UInt(32.W))
  val checksum = Reg(UInt(32.W))
  val ones_to_scan = Reg(UInt(64.W))
  val rd_d = Reg(UInt())
  val rs1_d = Reg(UInt())
  val rs2_d = Reg(UInt())
  val resp_d = Reg(UInt())

  val f32 = Module(new Fletcher(32))

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
    state := Mux(io.cmd.bits.status.vm === 0.U, s_('CYCLE_READ), s_('CYCLE_TRANSLATE))
    cycle_count := 0.U
    read_count := 0.U
    cycles_to_scan := 0.U - 1.U
    ones_to_scan := io.cmd.bits.rs2
    printfInfo("LeChiffre: Cycling: addr 0x%x\n", io.cmd.bits.rs1)
  }

  val f32Word = Reg(UInt(16.W))
  f32.io.data.bits.word := io.SCAN_out ## f32Word(15,1)
  f32.io.data.valid := false.B

  when (state === s_('CYCLE_TRANSLATE)) {
    state := s_('ERROR)
    printfError("LeChiffre: Address translation not implemented\n")
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
  io.SCAN_clk := piso.s.valid
  io.SCAN_out := piso.s.bits
  val last = cycle_count === cycles_to_scan - 1.U
  when (piso.s.valid) {
    cycle_count := cycle_count + 1.U

    f32Word := io.SCAN_out ## f32Word(15,1)
    when (cycle_count(3,0) === 15.U || last) {
      f32.io.data.valid := true.B
      f32.io.data.bits.cmd := k_compute.U
    }
  }

  // [TODO] Add a command to turn this on2
  io.SCAN_en := false.B

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
  when (state === s_('CYCLE_READ)) {
    when (autlAcqGrant(piso.p.ready)) {
      read_count := read_count + xLen.U
      addr_d := addr_d + (tlDataBits / 8).U
      when (read_count === 0.U) {
        checksum := gnt.bits.data(63,32)
        cycles_to_scan := gnt.bits.data(31,0)
        printfInfo("LeChiffre: Bits to scan: 0x%x\n", gnt.bits.data(31,0))
        printfInfo("LeChiffre: Checksum: 0x%x\n", gnt.bits.data(63,32))
      }
      when (read_count >= cycles_to_scan) {
        piso.p.bits.count := tlDataBits.U - read_count + cycles_to_scan - 1.U
        state := s_('CYCLE_QUIESCE)
      }
    }
  }

  when (state === s_('CYCLE_QUIESCE)) {
    when (piso.p.ready) { state := s_('RESP) }
    resp_d := checksum =/= f32.io.checksum
  }

  io.resp.valid := state === s_('RESP)
  when (state === s_('RESP)) {
    io.resp.bits.data := resp_d
    io.resp.bits.rd := rd_d
    state := s_('WAIT)

    f32.io.data.valid := true.B
    f32.io.data.bits.cmd := k_reset.U
  }

  when (state === s_('ERROR)) {
  }

  when (io.cmd.fire()) { printfInfo("LeChiffre: cmd 0x%x, rs1 0x%x, rs2 0x%x\n",
    io.cmd.bits.inst.asUInt, io.cmd.bits.rs1, io.cmd.bits.rs2)
    printfInfo("LeChiffre:   status 0x%x\n", io.cmd.bits.status.asUInt())
    printfInfo("LeChiffre:    -> fs 0x%x\n", io.cmd.bits.status.fs)
    printfInfo("LeChiffre:    -> xs 0x%x\n", io.cmd.bits.status.xs)
    printfInfo("LeChiffre:    -> vm 0x%x\n", io.cmd.bits.status.vm)
  }
  when (io.resp.fire()) { printfInfo("Chiffre: resp rd 0x%x, data 0x%x\n",
    io.resp.bits.rd, io.resp.bits.data) }

  when (acq.fire()) {
    printfInfo("LeChiffre: autl acq.%d | addr 0x%x, addr_block 0x%x, addr_beat 0x%x, addr_byte 0x%x\n",
      acq.bits.a_type, addr_d, acq.bits.addr_block, acq.bits.addr_beat,
      acq.bits.addr_byte())
  }

  when (reqSent && gnt.fire()) {
    printfDebug("LeChiffre: autl.gnt | data 0x%x, beat 0x%x\n",
      gnt.bits.data, gnt.bits.addr_beat) }

  when (io.SCAN_clk) {
    printfInfo("LeChifre: Scan[%d]: %d, En: %d\n", cycle_count, io.SCAN_out,
      io.SCAN_en)
  }

  // Catch all error states
  when (do_unknown) { state := s_('ERROR) }
  assert(RegNext(state) =/= s_('ERROR), "[ERROR] LeChiffre: Hit error state\n")
}
