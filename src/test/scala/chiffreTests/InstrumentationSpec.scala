// Copyright 2018 IBM
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
package chiffreTests

import chisel3._
import chisel3.iotesters.ChiselFlatSpec
import chisel3.stage.ChiselStage
import chiffre.{InjectorInfo, ChiffreController, ChiffreInjector, ChiffreInjectee}
import chiffre.passes.{ScanChainAnnotation, ScanChainDescriptionAnnotation, FaultInjectionAnnotation}
import chiffre.inject.{Injector, LfsrInjector32}

class DummyController extends Module with ChiffreController {
  val io = IO(new Bundle{})
  val scanId = "dummy"
}

class DummyInjector extends Injector(1) with ChiffreInjector {
  val scanId = "dummy"
  val info = EmptyInjectorInfo
}

class DummyInjectee extends Module with ChiffreInjectee {
  val io = IO(new Bundle{})
  val x = Reg(UInt(1.W))
  isFaulty(x, "dummy", classOf[LfsrInjector32])
}

class InstrumentationSpec extends ChiselFlatSpec {
  behavior of "ChiffreController annotation"

  it should "emit a ScanChainAnnotation" in {
    val circuit = ChiselStage.elaborate(new DummyController)
    circuit.annotations.map(_.toFirrtl).collect{ case a: ScanChainAnnotation => a }.size should be (1)
  }

  behavior of "Chiffree Injectee annotation"

  it should "emit an annotation" in {
    val circuit = ChiselStage.elaborate(new DummyInjectee)
    circuit.annotations.map(_.toFirrtl).collect{ case a: FaultInjectionAnnotation => a }.size should be (1)
  }
}
