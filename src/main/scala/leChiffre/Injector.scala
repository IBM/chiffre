// See LICENSE.IBM for license details

package leChiffre

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselAnnotation
import leChiffre.scanChain._
import ScanChainProtocol._
import net.jcazevedo.moultingyaml._

sealed class InjectorIo(n: Int) extends Bundle {
  val scan = new ScanIo
  val in = Input(UInt(n.W))
  val out = Output(UInt(n.W))
}

sealed abstract class Injector(n: Int, id: String) extends Module with AddsScanState {
  val io = IO(new InjectorIo(n))
}

abstract class OneBitInjector(id: String) extends Injector(1, id)

class InjectorNBit(n: Int, id: String, gen: => Injector) extends Injector(n, id) {
  val injectors = Seq.fill(n)(Module(gen))
  lazy val bits = injectors.foldLeft(Seq[ScanField]()){ case (a, b) => a ++ b.bits }

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

  annotate(
    ChiselAnnotation(
      this,
      classOf[passes.ScanChainTransform],
      s"""description:$id:${bits.toYaml.prettyPrint}"""))
}
