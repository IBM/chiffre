// See LICENSE.IBM for license details.

package leChiffre

import chisel3._
import chisel3.util._
import chisel3.internal.InstanceId
import chisel3.experimental.ChiselAnnotation

sealed trait ChiffreScan {
  self: Module =>
  val scan = Wire(new leChiffre.ScanChain)
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

  private def bindOriginalToReplacement(orig: InstanceId, replacement: InstanceId):
      Unit = {
    annotate(ChiselAnnotation(orig,
      classOf[firrtl.passes.FaultInstrumentationTransform], s"originalId:$id"))
    annotate(ChiselAnnotation(replacement,
      classOf[firrtl.passes.FaultInstrumentationTransform], s"replacementId:$id"))
    id += 1
  }

  def isFaulty(component: InstanceId, lfsrWidth: Int): Unit = {
    component match {
      case c: Bits =>
        val repls = Vec.fill(c.getWidth)(Wire(Bool()))
        Seq.fill(c.getWidth)(Module(new perfect.random.Lfsr(lfsrWidth)))
          .zipWithIndex.map{ case (lfsr, i) => {
            val seed = Reg(UInt(lfsrWidth.W))
            val difficulty = Reg(UInt(lfsrWidth.W))
            val enabled = Reg(init = false.B)
            lfsr.io.seed.valid := scan.en
            lfsr.io.seed.bits := seed
            repls(i) := c(i)
            when (enabled & (lfsr.io.y < difficulty)) { repls(i) := c(i) ^ 1.U }
            when (scan.clk) {
              enabled := false.B
              seed := scanIn ## (seed >> 1)
              difficulty := seed(0) ## (difficulty >> 1)
            }
            when (scan.en) { enabled := true.B }
            scanIn = difficulty(0)
          }}
        bindOriginalToReplacement(component, repls.asUInt)
      case c => throw new Exception(s"Type not implemented for: $c")
    }

    addSink(scan.clk, "scan_clk")
    addSink(scan.en, "scan_en")
  }
}
