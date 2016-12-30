// See LICENSE.ibm for license details.
package leChiffre

import chisel3._
import chisel3.util._
import rocket._
import config._
import perfect.util._

trait ScanChainIO {
  val SCAN_en = Output(Bool())
  val SCAN_in = Input(Bool())
  val SCAN_out = Output(Bool())
}

class LeChiffre(implicit p: Parameters) extends RoCC()(p) with UniformPrintfs
    with LeChiffreH with FletcherH {
  override lazy val io = new RoCCInterface with ScanChainIO

  io.busy := false.B
  io.interrupt := false.B
  io.autl.acquire.valid := false.B
  io.mem.req.valid := false.B

  io.SCAN_en := false.B

  val do_cycle = io.cmd.fire() & io.cmd.bits.inst.funct === f_CYCLE.U
  val do_unknown = io.cmd.fire() & io.cmd.bits.inst.funct > f_CYCLE.U

  val s_WAIT :: s_CYCLE :: s_RESP :: s_ERROR :: Nil = Enum(UInt(), 4)
  val state = Reg(init = s_WAIT)
  val cycle_count = Reg(UInt(64.W))
  val cycles_to_scan = Reg(UInt(64.W))
  val ones_to_scan = Reg(UInt(64.W))
  val rd_d = Reg(UInt())
  val rs1_d = Reg(UInt())
  val rs2_d = Reg(UInt())

  val f32 = Module(new Fletcher(32))

  io.cmd.ready := state === s_WAIT

  when (io.cmd.fire() && state === s_WAIT) {
    rd_d := io.cmd.bits.inst.rd
    rs1_d := io.cmd.bits.rs1
    rs2_d := io.cmd.bits.rs2
  }

  when (do_cycle) {
    state := s_CYCLE
    cycle_count := 0.U
    cycles_to_scan := io.cmd.bits.rs1
    ones_to_scan := io.cmd.bits.rs2
    printfInfo("LeChiffre: Cycling: cycles %d, ones %d\n",
      io.cmd.bits.rs1, io.cmd.bits.rs2)
  }

  val f32Word = Reg(UInt(16.W))
  f32.io.data.bits.word := io.SCAN_out ## f32Word(15,1)
  f32.io.data.valid := false.B
  when (state === s_CYCLE) {
    val last = cycle_count === cycles_to_scan - 1.U

    io.SCAN_en := true.B
    io.SCAN_out := cycle_count < ones_to_scan
    cycle_count := cycle_count + 1.U
    when (last) { state := s_RESP }

    f32Word := io.SCAN_out ## f32Word(15,1)
    when (cycle_count(3,0) === 15.U || last) {
      f32.io.data.valid := true.B
      f32.io.data.bits.cmd := k_compute.U
    }
  }

  io.resp.valid := state === s_RESP
  when (state === s_RESP) {
    io.resp.bits.data := f32.io.checksum
    io.resp.bits.rd := rd_d
    state := s_WAIT

    f32.io.data.valid := true.B
    f32.io.data.bits.cmd := k_reset.U
  }

  when (io.cmd.fire()) { printfInfo("Chiffre: cmd 0x%x, rs1 0x%x, rs2 0x%x\n",
    io.cmd.bits.inst.asUInt, io.cmd.bits.rs1, io.cmd.bits.rs2) }
  when (io.resp.fire()) { printfInfo("Chiffre: resp rd 0x%x, data 0x%x\n",
    io.resp.bits.rd, io.resp.bits.data) }

  when (state === s_CYCLE) {
    printfInfo("LeChifre: Cycle[%d]: %d\n", cycle_count, io.SCAN_out)
  }

  // Catch all error states
  when (do_unknown) { state := s_ERROR }
  assert(state =/= s_ERROR, "[ERROR] LeChiffre: Hit error state\n")
}
