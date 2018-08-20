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
package chiffreTests.passes

import chiffre.passes.{FaultInjectionAnnotation, FaultInstrumentationTransform, ScanChainAnnotation}
import chiffre.inject.IdentityInjector
import chiffre.{ChiffreController, ChiffreInjectee, ChiffreInjector}

import chisel3._
import firrtl.annotations.{ComponentName, ModuleName, CircuitName}
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester, Driver, TesterOptionsManager}

class InjectorA(bitWidth: Int, id: String) extends IdentityInjector(bitWidth, id)
class InjectorB(bitWidth: Int, id: String) extends InjectorA(bitWidth, id)
class InjectorC(bitWidth: Int, id: String) extends InjectorB(bitWidth, id)

class TestInjector(bitWidth: Int, scanId: String) extends IdentityInjector(bitWidth, scanId) {
  assert(io.scan.clk =/= io.scan.en)
  io.out := ~io.in
}

class TestController(val scanId: String) extends Module with ChiffreController {
  val io = IO(new Bundle{})
  val clk = Reg(UInt(1.W))
  clk := ~clk
  scan.clk := clk
  scan.en := ~clk
  scan.out := clk

  scan.out := clk
  assert(scan.in === scan.out)
}

class TestInjectee(faulty: Boolean) extends Module with ChiffreInjectee {
  val io = IO(new Bundle{})
  val x = RegInit(~0.U(1.W))
  val y = RegInit(~0.U(2.W))
  val scanId = "id"

  if (faulty) {
    isFaulty(x, scanId, classOf[TestInjector])
    assert(x === RegNext(~x), "Faulty register 'x' did not flip")
    assert(y === RegNext(y), "Non-faulty register 'y' did not hold its value")
  } else {
    isFaulty(y, scanId, classOf[TestInjector])
    assert(x === RegNext(x), "Non-faulty register 'x' did not hold its value")
    assert(y === RegNext(~y), "Faulty register 'y' did not flip")
  }
}

class TestInstrumentation extends Module {
  val io = IO(new Bundle{})
  val scanId = "id"

  val controller = Module(new TestController(scanId))
  val injectee_enabled = Module(new TestInjectee(true))
  val injectee_disabled = Module(new TestInjectee(false))
}

class TestTop(dut: TestInstrumentation) extends PeekPokeTester(dut) {
  step(4)
}

class FaultInstrumentationTransformSpec extends ChiselFlatSpec {

  behavior of "FaultInjectionAnnotation"

  it should "be serializable" in {
    val c = ComponentName("foo", ModuleName("bar", CircuitName("baz")))
    val x = FaultInjectionAnnotation(c, "id", classOf[IdentityInjector])
    val json = firrtl.annotations.JsonProtocol.serialize(Seq(x))
  }

  behavior of "FaultInstrumentationTransform"

  it should "add a single fault injector that flips signal 'x'" in {
    Driver(() => new TestInstrumentation) { dut => new TestTop(dut) }
  }
}
