// See LICENSE.IBM for license details

package leChiffre

import chisel3._
import chisel3.util._

sealed class InjectorIo(n: Int) extends Bundle {
  val scan = new ScanIo
  val in = Input(UInt(n.W))
  val out = Output(UInt(n.W))
}

sealed abstract class Injector(n: Int) extends Module with AddsScanState {
  val io = IO(new InjectorIo(n))
}

abstract class OneBitInjector extends Injector(1)

sealed class InjectorNBit(n: Int, gen: => Injector) extends Injector(n) {
  val injectors = Seq.fill(n)(Module(gen))
  val bits = injectors.foldLeft(Seq[(String, Int)]()){ case (a, b) => a ++ b.bits }

  var scanLast = io.scan.in
  injectors
    .zipWithIndex
    .map{ case (injector, i) =>
      injector.io.in := io.in(i)
      injector.io.scan.en := io.scan.en
      injector.io.scan.clk := io.scan.clk
      injector.io.scan.in := scanLast
      scanLast = injector.io.scan.out
    }
  io.scan.out := scanLast
  io.out := Cat(injectors.map(_.io.out))
}

class LfsrInjector(lfsrWidth: Int) extends OneBitInjector {
  val enabled = Reg(init = false.B)
  val difficulty = Reg(init = 0.U(lfsrWidth.W))
  val seed = Reg(init = 1.U(lfsrWidth.W))

  val bits = Seq( ("seed", lfsrWidth), ("difficulty", lfsrWidth) )

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

class LfsrInjector32(n: Int) extends InjectorNBit(n, new LfsrInjector(32))
