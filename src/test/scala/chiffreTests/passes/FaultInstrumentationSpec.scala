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

import chiffre.InjectorInfo
import chiffre.passes.{FaultInstrumentation, FaultInstrumentationException}
import chiffre.inject.{Injector, IdentityInjector, NoInjectorInfo}

import chisel3._
import firrtl._
import firrtl.ir._
import firrtl.analyses.InstanceGraph
import chisel3.iotesters.ChiselFlatSpec
import firrtl.annotations.{ComponentName, ModuleName, CircuitName}

import scala.collection.mutable.ArrayBuffer

class FaultInstrumentationSpec extends ChiselFlatSpec {
  private def collect(connections: ArrayBuffer[Connect])(s: Statement): Statement = {
    s match {
      case b: Block => b.mapStmt(collect(connections))
      case c: Connect => connections += c; c
      case _ => s
    }
  }

  behavior of "FaultInstrumentation"

  it should "add the specified injector" in {
    val component = ComponentName("x", ModuleName("top", CircuitName("top")))
    val compMap = Map(component.module.name -> Seq((component, "dummyId", classOf[IdentityInjector])))
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

    info("The injector module was added to the circuit")
    output.circuit.modules.map(_.name) should contain ("IdentityInjector")

    val iGraph = new InstanceGraph(output.circuit)
    val insts = iGraph.getChildrenInstances("top").map(_.copy(info=NoInfo, tpe=UnknownType))

    info("The injector was instantiated")
    insts should contain (WDefInstance(NoInfo, "IdentityInjector", "IdentityInjector", UnknownType))
  }

  it should "inject faults into a vector type" in {
    val component = ComponentName("x", ModuleName("top", CircuitName("top")))
    val compMap = Map(component.module.name -> Seq((component, "dummyId", classOf[IdentityInjector])))
    val f = new FaultInstrumentation(compMap)

    val input = """|circuit top:
                   |  module top:
                   |    input clock: Clock
                   |    input in: UInt<1>[4]
                   |    output out: UInt<1>
                   |    reg x: UInt<1>[4], Clock
                   |    x <= in
                   |    out <= xorr(x)
                   |""".stripMargin

    val circuit = Parser.parse(input)
    val state = CircuitState(circuit, MidForm, Seq.empty, None)
    val connections = new ArrayBuffer[Connect]()
    f.execute(state).circuit.modules.filter(_.name == component.module.name).foreach(_.mapStmt(collect(connections)))
    for (i <- 0 to 3) {
      connections.map(_.serialize) should contain (s"x_fault[$i] <= asUInt(bits(IdentityInjector.io.out, $i, $i))")
    }
    connections.last.expr.serialize should be ("cat(cat(asUInt(x[3]), asUInt(x[2])), cat(asUInt(x[1]), asUInt(x[0])))")
  }

  it should "inject faults into a bundle type" in {
    val component = ComponentName("x", ModuleName("top", CircuitName("top")))
    val compMap = Map(component.module.name -> Seq((component, "dummyId", classOf[IdentityInjector])))
    val f = new FaultInstrumentation(compMap)

    val tpe = "{a: UInt<3>, b: UInt<5>}"
    val input = s"""|circuit top:
                    |  module top:
                    |    input clock: Clock
                    |    input in: $tpe
                    |    output out: $tpe
                    |    reg x: $tpe, Clock
                    |    x <= in
                    |    out <= x
                    |""".stripMargin
    
    val circuit = Parser.parse(input)
    val state = CircuitState(circuit, MidForm, Seq.empty, None)
    val connections = new ArrayBuffer[Connect]()
    f.execute(state).circuit.modules.filter(_.name == component.module.name).foreach(_.mapStmt(collect(connections)))
    connections.map(_.serialize) should contain ("x_fault.b <= asUInt(bits(IdentityInjector.io.out, 4, 0))")
    connections.map(_.serialize) should contain ("x_fault.a <= asUInt(bits(IdentityInjector.io.out, 7, 5))")
    connections.last.expr.serialize should be ("cat(asUInt(x.a), asUInt(x.b))")
  }

  it should "error if a 0-width Vec is instrumented" in {
    val component = ComponentName("x", ModuleName("top", CircuitName("top")))
    val compMap = Map(component.module.name -> Seq((component, "dummyId", classOf[IdentityInjector])))
    val f = new FaultInstrumentation(compMap)

    val input = """|circuit top:
                   |  module top:
                   |    input clock: Clock
                   |    input in: UInt<1>
                   |    output out: UInt<1>
                   |    reg x: UInt<1>[0], Clock
                   |    x[0] <= in
                   |    out <= x[0]
                   |""".stripMargin

    val circuit = Parser.parse(input)
    val state = CircuitState(circuit, MidForm, Seq.empty, None)
    a [FIRRTLException] should be thrownBy (f.execute(state))
  }

  it should "error if an ExtModule is instrumented" in {
    val component = ComponentName("x", ModuleName("top", CircuitName("top")))
    val compMap = Map(component.module.name -> Seq((component, "dummyId", classOf[IdentityInjector])))
    val f = new FaultInstrumentation(compMap)

    val input = """|circuit top:
                   |  extmodule top:
                   |    input clock: Clock
                   |    input in: UInt<1>
                   |    output out: UInt<1>
                   |""".stripMargin

    val circuit = Parser.parse(input)
    val state = CircuitState(circuit, MidForm, Seq.empty, None)

    a [FaultInstrumentationException] should be thrownBy (f.execute(state))
  }
}
