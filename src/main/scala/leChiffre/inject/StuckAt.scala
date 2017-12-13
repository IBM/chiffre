// See LICENSE.IBM for license details
package leChiffre.inject

import chisel3._
import chisel3.util._
import leChiffre.scan._

class StuckAt(n: Int, id: String) extends Injector(n, id) {
  val mask = Reg(UInt(n.W))
  val value = Reg(UInt(n.W))

  lazy val info = StuckAtInjectorInfo(n)

  io.out := enabled & (
    (!mask & io.in) | ((mask & io.in) & value) | ((mask & !io.in) | value) )

  when (io.scan.clk) {
    enabled := false.B
    mask := io.scan.in ## (mask >> 1)
    value := mask(0) ## (value >> 1)
  }

  io.scan.out := value(0)
}
