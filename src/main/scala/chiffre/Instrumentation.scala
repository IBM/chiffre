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
import chisel3.experimental.{ChiselAnnotation, RunFirrtlTransform}
import chiffre.passes.{ScanChainAnnotation, FaultInjectionAnnotation,
  ScanChainTransform, FaultInstrumentationTransform}
import chiffre.inject.Injector
import chiffre.passes.ScanChainDescriptionAnnotation

trait ChiffreController { this: BaseModule =>
  /** Scan Chain Identifier used to differentiate scan chains. This must
    * be a `lazy val`. */
  def scanId: String

  private def scanMaster(scan: Data, name: String): Unit = {
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
}

trait ChiffreInjectee { this: BaseModule =>

  /** Make a specific signal run-time fault injectable
    *
    * @param component the component to make injectable
    * @param id the name of the scan chain used to configure injections into this component
    * @param gen a class of [[Injector]] to use for fault injections
    */
  def isFaulty(component: Data, id: String, gen: Class[_ <: Injector]): Unit = {
    chisel3.experimental.annotate(
      new ChiselAnnotation with RunFirrtlTransform {
        def toFirrtl = FaultInjectionAnnotation(component.toNamed, id, gen)
        def transformClass = classOf[FaultInstrumentationTransform]
      })
  }
}
