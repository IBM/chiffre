// Copyright 2017 IBM
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
package leChiffre

import chisel3._
import chisel3.util._
import chisel3.internal.InstanceId
import chisel3.experimental.ChiselAnnotation
import scala.collection.mutable
import leChiffre.scan._

trait ChiffreController {
  self: Module =>

  /** Scan Chain Identifier used to differentiate scan chains. This must
    * be a `lazy val`. */
  def scanId: String

  private def addSource(component: InstanceId, name: String): Unit = {
    annotate(
      ChiselAnnotation(component,
                       classOf[_root_.firrtl.passes.wiring.WiringTransform],
                       s"source $name"))
  }

  private def scanMaster(in: InstanceId, out: InstanceId, name: String): Unit = {
    if (scanId == null) {
      throw new Exception(
        "Chiffre Controller attribute 'scanId' was 'null' (should be a 'lazy val')") }
    annotate(
      ChiselAnnotation(
        in,
        classOf[leChiffre.passes.ScanChainTransform],
        s"master:in:$name"))
    annotate(
      ChiselAnnotation(
        out,
        classOf[leChiffre.passes.ScanChainTransform],
        s"master:out:$name"))
  }

  val scan = Wire(new ScanIo)

  addSource(scan.clk, "scan_clk")
  addSource(scan.en, "scan_en")
  scanMaster(scan.in, scan.out, scanId)
}

trait ChiffreInjectee {
  self: Module =>

  def isFaulty[T <: inject.Injector](component: InstanceId, id: String,
                                     tpe: Class[T]): Unit = {
    component match {
      case c: Bits =>
        annotate(ChiselAnnotation(c,
          classOf[passes.FaultInstrumentationTransform],
          s"injector:$id:${tpe.getName}"))
      case c => throw new Exception(s"Type not implemented for: $c")
    }
  }
}
