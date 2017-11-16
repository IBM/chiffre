// See LICENSE.ibm for license details.
package leChiffre.scan

import chisel3._

class ScanIo extends Bundle {
  val clk = Input(Bool())
  val en = Input(Bool())
  val in = Input(Bool())
  val out = Output(Bool())
}

trait AddsScanState {
  self: Module =>

  /** Encodes the names and widths of fields that this injector adds to
    * the scan chain */
  def bits: Seq[ScanField]
}
