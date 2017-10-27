// See LICENSE.IBM for license details.

package leChiffre

import chisel3._
import chisel3.util._
import chisel3.internal.InstanceId
import chisel3.experimental.ChiselAnnotation

sealed trait ChiffreScan {
  self: Module =>
  val scan = Wire(new ScanChain)
}

trait ChiffreController extends ChiffreScan {
  self: Module =>

  private def addSource(component: InstanceId, name: String): Unit = {
    annotate(ChiselAnnotation(component, classOf[_root_.firrtl.passes.wiring.WiringTransform], s"source $name"))
  }

  addSource(scan.clk, "scan_clk")
  addSource(scan.en, "scan_en")
}

trait ChiffreInjectee extends ChiffreScan {
  self: Module =>

  var scanIn = scan.in
  var id: Int = 0

  private def addSink(component: InstanceId, name: String): Unit = {
    annotate(ChiselAnnotation(component, classOf[_root_.firrtl.passes.wiring.WiringTransform], s"sink $name"))
  }

  private def bindOriginalToReplacement(
    original: InstanceId, connection:InstanceId, replacement: InstanceId):
      Unit = {
    annotate(ChiselAnnotation(original,
      classOf[firrtl.passes.FaultInstrumentationTransform], s"originalId:$id"))
    annotate(ChiselAnnotation(connection,
      classOf[firrtl.passes.FaultInstrumentationTransform], s"connectionId:$id"))
    annotate(ChiselAnnotation(replacement,
      classOf[firrtl.passes.FaultInstrumentationTransform], s"replacementId:$id"))
    id += 1
  }

  def isFaulty(component: InstanceId, lfsrWidth: Int): Unit = {
    component match {
      case c: Bits =>
        val in = Wire(c.cloneType)
        val inVec = Wire(Vec(c.getWidth, Bool()))
        val outVec = Wire(Vec(c.getWidth, Bool()))
        inVec.zipWithIndex.map{ case (l, i) => l := in(i) }
        val injectors = Seq.fill(c.getWidth)(Module(new LfsrInjector(lfsrWidth)))
        injectors
          .zipWithIndex
          .map{ case(injector, i) =>
            injector.io.in := inVec(i)
            injector.io.scan.in := scanIn
            injector.io.scan.en := scan.en
            injector.io.scan.clk := scan.clk
            outVec(i) := injector.io.out
            scanIn = injector.io.scan.out
          }
        val inject = c.fromBits(outVec.asUInt)
        bindOriginalToReplacement(component, in, inject)
      case c => throw new Exception(s"Type not implemented for: $c")
    }

    addSink(scan.clk, "scan_clk")
    addSink(scan.en, "scan_en")
    // [todo] connect scan.in and scan.out
  }
}
