// See LICENSE.ibm for license details.
package leChiffre

import chisel3._
import chisel3.util._
import rocket._
import config._
import perfect.util._

trait ScanChainIO {
  val SCAN_en = Input(Bool())
  val SCAN_in = Input(Bool())
  val SCAN_out = Output(Bool())
}

class LeChiffre(implicit p: Parameters) extends RoCC()(p) with UniformPrintfs {
  override lazy val io = new RoCCInterface with ScanChainIO

  io.cmd.ready := true.B
  io.busy := false.B
  io.interrupt := false.B
  io.autl.acquire.valid := false.B
  io.mem.req.valid := false.B

  // Just echo back whatever we read from rs1
  io.resp.valid := RegNext(io.cmd.fire())
  io.resp.bits.data := RegNext(io.cmd.bits.rs1)
  io.resp.bits.rd := RegNext(io.cmd.bits.inst.rd)

  when (io.cmd.fire()) { printfInfo("cmd fire\n" ) }
}
