// See LICENSE.ibm for license details.
package leChiffre

import chisel3._
import leChiffre.scan.InjectorInfo

class ScanIo extends Bundle {
  val clk = Input(Bool())
  val en = Input(Bool())
  val in = Input(Bool())
  val out = Output(Bool())
}

trait AddsScanState {
  self: Module =>

  def info: InjectorInfo
}
