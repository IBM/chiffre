// See LICENSE.ibm for license details.
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
  val do_write_difficulty = io.cmd.fire() & funct === f_FAULT_DIFFICULTY.U
  val do_write_duration   = io.cmd.fire() & funct === f_FAULT_DURATION.U
  val do_write_seed       = io.cmd.fire() & funct === f_WRITE_SEED.U
  val do_unknown          = io.cmd.fire() & funct  >  f_WRITE_SEED.U

  val s_ = Chisel.Enum(UInt(), List('WAIT,
    'CYCLE_TRANSLATE, 'CYCLE_READ, 'CYCLE_QUIESCE,
    'RESP, 'ERROR))
  val state = Reg(init = s_('WAIT))
  val cycle_count = Reg(UInt(64.W)) // [#2] config parameter
  val read_count = Reg(UInt(64.W)) // [#2] config parameter
  val cycles_to_scan = Reg(UInt(32.W)) // [#2] config parameter
  val checksum = Reg(UInt(32.W)) // [#2] config parameter
  val ones_to_scan = Reg(UInt(64.W)) // [#2] config parameter
  val rd_d = Reg(UInt())
  val rs1_d = Reg(UInt())
  val rs2_d = Reg(UInt())
  val resp_d = Reg(UInt())

  val reg_difficulty = Reg(UInt(32.W)) // [#2] config parameter
  val reg_fault_duration = Reg(UInt(64.W)) // [#2] config parameter
  val reg_enabled = Reg(init = false.B)

  val lfsr = Module(new Lfsr(reg_difficulty.getWidth))
  val fletcher = Module(new Fletcher(checksum.getWidth))

  val emergency = Reg(init = false.B)
  val count = Reg(init = 0.U(reg_fault_duration.getWidth.W))

  scan.en := false.B
  when (emergency)                    { count      := count + 1.U }
  when (count === reg_fault_duration) { emergency  := false.B
                                        scan.en := true.B      }
  when (reg_enabled && (lfsr.io.y < reg_difficulty)) {
    emergency := true.B
    count := 0.U
    scan.en := !emergency
  }

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
    printfInfo("Cycling: addr 0x%x\n", io.cmd.bits.rs1)
  }

  when (do_enable) {
    state := s_('RESP)
    resp_d := 0.U
    reg_enabled := ~reg_enabled
  }

  when (do_write_difficulty) {
    state := s_('RESP)
    resp_d := reg_difficulty
    reg_difficulty := io.cmd.bits.rs2
  }

  when (do_write_duration) {
    state := s_('RESP)
    resp_d := reg_fault_duration
    reg_fault_duration := io.cmd.bits.rs2
  }

  lfsr.io.seed.valid := do_write_seed
  lfsr.io.seed.bits := io.cmd.bits.rs2
  when (do_write_seed) {
    state := s_('RESP)
    resp_d := 0.U
  }

  val fletcherWord = Reg(UInt(16.W))
  fletcher.io.data.bits.word := scan.out ## fletcherWord(15,1)
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

    fletcherWord := scan.out ## fletcherWord(15,1)
    when (cycle_count(3,0) === 15.U || last) {
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
  when (state === s_('CYCLE_READ)) {
    when (autlAcqGrant(piso.p.ready)) {
      read_count := read_count + xLen.U
      addr_d := addr_d + (tlDataBits / 8).U
      when (read_count === 0.U) {
        checksum := gnt.bits.data(63,32)
        cycles_to_scan := gnt.bits.data(31,0)
        printfInfo("Bits to scan: 0x%x\n", gnt.bits.data(31,0))
        printfInfo("Checksum: 0x%x\n", gnt.bits.data(63,32))
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
    printfInfo("Scan[%d]: %d, En: %d\n", cycle_count, scan.out,
      scan.en)
  }

  // Catch all error states
  when (do_unknown) { state := s_('ERROR) }
  assert(RegNext(state) =/= s_('ERROR), "[ERROR] LeChiffre: Hit error state\n")
}
