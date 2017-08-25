// See LICENSE.IBM for license details.

package leChiffre

import chisel3._
import chisel3.util._
import chisel3.internal.InstanceId
import chisel3.experimental.ChiselAnnotation
import firrtl.annotations.{Annotation, Named}

object ChiffreAnnotation {
  def apply(target: Named, value: String): Annotation =
    Annotation(target, classOf[ChiffreInjectionTransform], value)

  def unapply(a: Annotation): Option[(Named, String)] = a match {
    case Annotation(named, t, value) if t == classOf[ChiffreInjectionTransform] =>
      Some((named, value))
    case _ => None
  }
}

trait ChiffreAnnotator {
  self: Module =>

  def broadcastSource(name: String, width: Int): Unit = {
    annotate(ChiselAnnotation(this, classOf[ChiffreInjectionTransform], s"broadcast,source,${name},${width}"))
  }

  def broadcastSink(name: String): Unit = {
    annotate(ChiselAnnotation(this, classOf[ChiffreInjectionTransform], s"broadcast,sink,${name}"))
  }

  def isInjectee(): Unit = {
    broadcastSink("io_SCAN_clk")
    broadcastSink("io_SCAN_en")
  }

  def isInjector(): Unit = {
    broadcastSource("io_SCAN_clk", 1)
    broadcastSource("io_SCAN_en", 1)
  }
}
