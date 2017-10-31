// See LICENSE.IBM for license details

package leChiffre

import chisel3._

class InjectorIo extends Bundle {
  val scan = new ScanIo
  val in = Input(Bool())
  val out = Output(Bool())
}

abstract class Injector extends Module {
  val io = IO(new InjectorIo)

  /** Encodes the names and widths of fields that this injector adds to
    * the scan chain */
  def bits: Seq[(String, Int)]
}

class LfsrInjector(width: Int) extends Injector {
  val enabled = Reg(init = false.B)
  val difficulty = Reg(init = 0.U(width.W))
  val seed = Reg(init = 1.U(width.W))

  val bits = Seq( ("seed", width), ("difficulty", width) )

  val lfsr = Module(new perfect.random.Lfsr(width))
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
