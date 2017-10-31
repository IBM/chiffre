// See LICENSE.IBM for license details.

package leChiffre

import chisel3._
import chisel3.util._
import chisel3.internal.InstanceId
import chisel3.experimental.ChiselAnnotation
import scala.collection.mutable

sealed trait ChiffreScan {
  self: Module =>
  val scan = Wire(new ScanIo)
}

trait ChiffreController extends ChiffreScan {
  self: Module =>

  /** Scan Chain Identifier used to differentiate scan chains. This must
    * be a `lazy val`. */
  def scanId: String

  private def addSource(component: InstanceId, name: String): Unit = {
    annotate(ChiselAnnotation(component, classOf[_root_.firrtl.passes.wiring.WiringTransform], s"source $name"))
  }

  private def scanMaster(in: InstanceId, out: InstanceId, name: String): Unit = {
    if (scanId == null) { throw new Exception(
      "Chiffre Controller attribute 'scanId' was 'null' (should be a 'lazy val')") }
    annotate(ChiselAnnotation(in, classOf[leChiffre.passes.ScanChainTransform],
      s"master:in:$name"))
    annotate(ChiselAnnotation(out, classOf[leChiffre.passes.ScanChainTransform],
      s"master:out:$name"))
  }

  addSource(scan.clk, "scan_clk")
  addSource(scan.en, "scan_en")
  scanMaster(scan.in, scan.out, scanId)
}

trait ChiffreInjectee extends ChiffreScan {
  self: Module =>

  private var scanLastDangle = scan.in
  private var id: Int = 0
  private var touched: Boolean = false

  private def addSink(component: InstanceId, name: String): Unit = {
    annotate(ChiselAnnotation(component, classOf[_root_.firrtl.passes.wiring.WiringTransform], s"sink $name"))
  }

  private def scanSlave(in: InstanceId, out: InstanceId, name: String): Unit = {
    annotate(ChiselAnnotation(in, classOf[leChiffre.passes.ScanChainTransform],
      s"slave:in:$name"))
    annotate(ChiselAnnotation(out, classOf[leChiffre.passes.ScanChainTransform],
      s"slave:out:$name"))
  }

  private def bindOriginalToReplacement(
    original: InstanceId, connection:InstanceId, replacement: InstanceId):
      Unit = {
    annotate(ChiselAnnotation(original,
      classOf[leChiffre.passes.FaultInstrumentationTransform], s"originalId:$id"))
    annotate(ChiselAnnotation(connection,
      classOf[leChiffre.passes.FaultInstrumentationTransform], s"connectionId:$id"))
    annotate(ChiselAnnotation(replacement,
      classOf[leChiffre.passes.FaultInstrumentationTransform], s"replacementId:$id"))
    id += 1
  }

  def isFaulty(component: Seq[InstanceId], id: String, lfsrWidth: Int): Unit = {
    if (touched) { throw new Exception(s"Tried to instrument module twice!") }
    component.map(_ match {
      case c: Bits =>
        val in = Wire(c.cloneType)
        val inVec = Wire(Vec(c.getWidth, Bool()))
        val outVec = Wire(Vec(c.getWidth, Bool()))
        inVec.zipWithIndex.map{ case (l, i) => l := in(i) }
        val injectors = Seq.fill(c.getWidth)(Module(new LfsrInjector(lfsrWidth)))
        var bits = Seq[(String, Int)]()
        injectors
          .zipWithIndex
          .map{ case(injector, i) =>
            injector.io.in := inVec(i)
            injector.io.scan.in := scanLastDangle
            injector.io.scan.en := scan.en
            injector.io.scan.clk := scan.clk
            outVec(i) := injector.io.out
            scanLastDangle = injector.io.scan.out
            bits ++= injector.bits
          }
        val inject = c.fromBits(outVec.asUInt)
        scan.out := scanLastDangle
        bindOriginalToReplacement(c, in, inject)
      case c => throw new Exception(s"Type not implemented for: $c")
    })
    scanSlave(scan.in, scan.out, id)
    addSink(scan.clk, "scan_clk")
    addSink(scan.en, "scan_en")
    touched = true
  }
}

// case class ScanInfo(io: Seq[(InstanceId, InstanceId)], size: Seq[(String, Int)]) {
//   def ++ (a: ScanInfo): ScanInfo = ScanInfo(this.io ++ a.io, this.size ++ a.size)
// }

// object ScanChain {
//   private val scans = mutable.HashMap[String, ScanInfo]()

//   def append(id: String, in: InstanceId, out: InstanceId, bits: Seq[(String, Int)]):
//       Unit = {
//     scans(id) = scans.getOrElse(id, ScanInfo(Seq.empty, Seq.empty)) ++ (
//       ScanInfo(Seq((in, out)), bits) )
//   }

//   def serialize(indent: String): String = {
//     scans.map{ case (k, b) => s"""${indent} ${k}:
// ${indent}   io: ${v.io.map{case(a,b) => s"$indent     - $a, $b"}}
// """
//     }
//   }
// }
