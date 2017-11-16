// See LICENSE.IBM for license details
package leChiffre.inject

import chisel3._
import chisel3.util._
import leChiffre.scan._

class CycleInjector(n: Int, cycleWidth: Int, id: String) extends Injector(n, id) {
  val cycleTarget = Reg(UInt(cycleWidth.W))
  val cycleCounter = Reg(UInt(cycleWidth.W))
  val flipMask = Reg(UInt(n.W))
  val enabled = Reg(init = false.B)

  lazy val bits = Seq( Cycle(cycleWidth), Mask(n) )

  val fire = enabled & (cycleCounter === cycleTarget)
  io.out := Mux(fire, io.in ^ flipMask, io.in)

  when (io.scan.clk) {
    enabled := false.B
    cycleTarget := io.scan.in ## (cycleTarget >> 1)
    flipMask := cycleTarget(0) ## (flipMask >> 1)
  }

  io.scan.out := flipMask(0)

  when (io.scan.en) { enabled := true.B }
}
