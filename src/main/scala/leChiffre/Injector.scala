// See LICENSE.IBM for license details

package leChiffre

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselAnnotation

sealed class InjectorIo(n: Int) extends Bundle {
  val scan = new ScanIo
  val in = Input(UInt(n.W))
  val out = Output(UInt(n.W))
}

sealed abstract class Injector(n: Int) extends Module with AddsScanState {
  val io = IO(new InjectorIo(n))
}

abstract class OneBitInjector extends Injector(1)

class InjectorNBit(n: Int, gen: => Injector) extends Injector(n) {
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

  annotate(ChiselAnnotation(this,
                            classOf[passes.ScanChainTransform],
                            s"$bits"))
}
