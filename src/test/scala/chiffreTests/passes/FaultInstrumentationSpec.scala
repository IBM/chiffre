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

import chiffre.passes.FaultInstrumentation
import chiffre.inject.StuckAtInjector

import firrtl._
import firrtl.ir.Circuit
import chisel3.iotesters.ChiselFlatSpec
import firrtl.annotations.{ComponentName, ModuleName, CircuitName}

class FaultInstrumentationSpec extends ChiselFlatSpec {

  behavior of "FaultInstrumentation"

  it should "add the specified injector" in {
    val component = ComponentName("x", ModuleName("top", CircuitName("top")))
    val gen = (bitWidth: Int, id: String) => StuckAtInjector(bitWidth, id)
    val compMap = Map(component.module.name -> Seq((component, "dummyId", gen)))
    val f = new FaultInstrumentation(compMap)

    val input = """|circuit top:
                   |  module top:
                   |    input clock: Clock
                   |    input in: UInt<1>
                   |    output out: UInt<1>
                   |    reg x: UInt<1>, Clock
                   |    x <= in
                   |    out <= x
                   |""".stripMargin

    val circuit = Parser.parse(input)
    val state = CircuitState(circuit, MidForm, Seq.empty, None)

    val output = f.execute(state)
    output.circuit.modules.map(_.name) should contain ("StuckAtInjectoranon1")
  }
}
