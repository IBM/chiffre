// See LICENSE.IBM for license details.

package leChiffre

import chisel3._
import chisel3.util._
import chisel3.internal.InstanceId
import chisel3.experimental.ChiselAnnotation

trait ChiffreAnnotator {
  self: Module =>

  def addSink(component: InstanceId, name: String): Unit = {
    annotate(ChiselAnnotation(component, classOf[_root_.firrtl.passes.wiring.WiringTransform], s"sink $name"))
  }

  def addSource(component: InstanceId, name: String): Unit = {
    annotate(ChiselAnnotation(component, classOf[_root_.firrtl.passes.wiring.WiringTransform], s"source $name"))
  }

  def isFaulty(component: InstanceId, tpe: String): Unit = {
    annotate(ChiselAnnotation(component, classOf[firrtl.passes.FaultInstrumentationTransform], tpe))
  }
}
