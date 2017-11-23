// See LICENSE.IBM for license details
package leChiffre.inject

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselAnnotation
import leChiffre._
import leChiffre.inject._
import leChiffre.passes._
import leChiffre.scan._
import ScanChainProtocol._
import net.jcazevedo.moultingyaml._

/** An injector interface */
sealed class InjectorIo(n: Int) extends Bundle {
  val scan = new ScanIo
  val in = Input(UInt(n.W))
  val out = Output(UInt(n.W))
}

/** The sketch of an injector module */
sealed abstract class InjectorLike(n: Int, id: String) extends Module
    with AddsScanState {
  val io = IO(new InjectorIo(n))
}

/** An injector that does not add its bits to the scan chain. Extend
  * this to make smaller injectors wrapped by a bigger [[Injector]]
  * that adds this module's bits to the chain, cf.
  * [[InjectorBitwise]].
  */
abstract class InjectorPrimitive(n: Int, id: String) extends InjectorLike(n, id)

/** An injector that adds bits to the scan chain */
abstract class Injector(n: Int, id: String) extends InjectorLike(n, id) {
  if (bits == null) {
    throw new FaultInstrumentationException(
      "Children of class Injector must use a `lazy val` for member `bits`") }
  annotate(
    ChiselAnnotation(
      this,
      classOf[ScanChainTransform],
      s"""description:$id:${bits.toYaml.prettyPrint}"""))
}

/** A one-bit injector primmitive */
abstract class OneBitInjector(id: String) extends InjectorPrimitive(1, id)

/** An injector composed of individual single-bit injectors */
class InjectorBitwise(n: Int, id: String, gen: => OneBitInjector)
    extends Injector(n, id) {
  lazy val injectors = Seq.fill(n)(Module(gen))
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
}
