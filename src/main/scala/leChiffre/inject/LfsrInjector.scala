// See LICENSE.IBM for license details
package leChiffre.inject

import chisel3._
import chisel3.util._
import leChiffre.scan._

class LfsrInjector(lfsrWidth: Int, id: String) extends OneBitInjector(id) {
  val enabled = Reg(init = false.B)
  val difficulty = Reg(init = 0.U(lfsrWidth.W))
  val seed = Reg(init = 1.U(lfsrWidth.W))
  val bits = Seq( Seed(lfsrWidth), Difficulty(lfsrWidth) )

  val lfsr = Module(new perfect.random.Lfsr(lfsrWidth))
  lfsr.io.seed.valid := io.scan.en
  lfsr.io.seed.bits := seed

  val fire = enabled && (lfsr.io.y < difficulty)
  io.out := Mux(fire, ~io.in, io.in)

  when (io.scan.clk) {
    enabled := false.B
    seed := io.scan.in ## (seed >> 1)
    difficulty := seed(0) ## (difficulty >> 1)
  }

  io.scan.out := difficulty(0)

  when (io.scan.en) { enabled := true.B }
}

class LfsrInjector32(n: Int, id: String)
    extends InjectorBitwise(n, id, new LfsrInjector(32, id))
