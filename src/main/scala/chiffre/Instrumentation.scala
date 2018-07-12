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
package chiffre

import chisel3._
import chisel3.core.BaseModule
import chisel3.internal.InstanceId
import chisel3.experimental.{ChiselAnnotation, RunFirrtlTransform}
import chiffre.passes.{ScanChainAnnotation, FaultInjectionAnnotation,
  ScanChainTransform, FaultInstrumentationTransform}
import chiffre.inject.Injector
import chiffre.passes.ScanChainDescriptionAnnotation

trait ChiffreController extends BaseModule {
  self: BaseModule =>

  /** Scan Chain Identifier used to differentiate scan chains. This must
    * be a `lazy val`. */
  def scanId: String

  private def scanMaster(scan: Data, name: String): Unit = {
    // if (scanId == null) { // scalastyle:off
    //   throw new Exception(
    //     "Chiffre Controller 'scanId' should be a 'lazy val'") }
    chisel3.experimental.annotate(
      new ChiselAnnotation {
        def toFirrtl = ScanChainAnnotation(scan.toNamed, "master", "scan", name, None)
      }
    )
  }

  val scan = Wire(new ScanIo)
  scan.in := false.B

  scanMaster(scan, scanId)
}

trait ChiffreInjector { this: Injector =>
  val scanId: String

  chisel3.experimental.annotate {
    val x = this
    new ChiselAnnotation {
      def toFirrtl = ScanChainDescriptionAnnotation(x.toNamed, scanId, info)
    }}
}

trait ChiffreInjectee extends BaseModule {
  self: BaseModule =>

  def isFaulty[T <: Injector](component: InstanceId, id: String, gen: Class[_ <: Injector]): Unit = {
    component match {
      case c: Bits =>
        chisel3.experimental.annotate(
          new ChiselAnnotation with RunFirrtlTransform {
            def toFirrtl = FaultInjectionAnnotation(c.toNamed, id, gen)
            def transformClass = classOf[FaultInstrumentationTransform]
          })
      case c => throw new Exception(s"Type not implemented for: $c")
    }
  }
}
