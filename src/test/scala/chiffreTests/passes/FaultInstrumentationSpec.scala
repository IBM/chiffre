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

import firrtl._
import firrtl.ir.{Circuit, Connect, NoInfo, Statement, UnknownType}
import firrtl.analyses.InstanceGraph
import chisel3.iotesters.ChiselFlatSpec
import firrtl.annotations.{ComponentName, ModuleName, CircuitName}
import firrtl.Mappers._

import scala.collection.mutable.ArrayBuffer

class FaultInstrumentationSpec extends ChiselFlatSpec {
  private def collect(connections: ArrayBuffer[Connect])(s: Statement): Statement = s map collect(connections) match {
    case c: Connect => connections += c; c
    case sx => sx
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

  it should "add two injectors if two registers are annotated" in {
    val components = Seq(ComponentName("x", ModuleName("top", CircuitName("top"))),
                         ComponentName("y", ModuleName("top", CircuitName("top"))))
    val compMap = Map("top" -> components.map((_, "dummyId", classOf[IdentityInjector])).toSeq)
    val f = new FaultInstrumentation(compMap)

    val input = """|circuit top:
                   |  module top:
                   |    input clock: Clock
                   |    input in: UInt<1>[2]
                   |    output out: UInt<1>[2]
                   |    reg x: UInt<1>, Clock
                   |    reg y: UInt<1>, Clock
                   |    x <= in[0]
                   |    y <= in[1]
                   |    out[0] <= x
                   |    out[1] <= y
                   |""".stripMargin

    val circuit = Parser.parse(input)
    val state = CircuitState(circuit, MidForm, Seq.empty, None)

    val output = f.execute(state)

    info("The injector module was added to the circuit")
    output.circuit.modules.map(_.name).filter(_.contains("IdentityInjector")).size should be (2)

    val iGraph = new InstanceGraph(output.circuit)
    val insts = iGraph.getChildrenInstances("top").map(_.copy(info=NoInfo, tpe=UnknownType))

    info("The injectors were instantiated")
    insts.map(_ match {
      case WDefInstance(NoInfo, "IdentityInjector", "IdentityInjector", UnknownType) => true
      case _ => false
    }).size should be (2)
  }

  it should "inject faults into a ground type" in {
    val component = ComponentName("x", ModuleName("top", CircuitName("top")))
    val compMap = Map(component.module.name -> Seq((component, "dummyId", classOf[IdentityInjector])))
    val f = new FaultInstrumentation(compMap)

    val input = """|circuit top:
                   |  module top:
                   |    input clock: Clock
                   |    input in: UInt<4>
                   |    output out: UInt<4>
                   |    reg x: UInt<4>, Clock
                   |    x <= in
                   |    out <= x
                   |""".stripMargin

    val circuit = Parser.parse(input)
    val state = CircuitState(circuit, MidForm, Seq.empty, None)
    val connections = new ArrayBuffer[Connect]()
    f.execute(state).circuit.modules.filter(_.name == component.module.name).foreach(_.mapStmt(collect(connections)))
    connections.map(_.serialize) should contain ("out <= x_fault")
    connections.map(_.serialize) should contain ("x_fault <= asUInt(bits(IdentityInjector.io.out, 3, 0))")
    connections.last.expr.serialize should be ("asUInt(x)")
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

  it should "inject faults into a nested bundle type" in {
    val component = ComponentName("x", ModuleName("top", CircuitName("top")))
    val compMap = Map(component.module.name -> Seq((component, "dummyId", classOf[IdentityInjector])))
    val f = new FaultInstrumentation(compMap)

    val tpe = "{a: {a1: UInt<1>, a2: UInt<3>}, b: UInt<3>}"
    val input = s"""|circuit top:
                    |  module top:
                    |    input clock: Clock
                    |    input in: $tpe
                    |    output out: {a: UInt<1>, b: UInt<4>}
                    |    reg x: $tpe, Clock
                    |    x <= in
                    |    out.a <= and(x.a.a1, andr(x.b))
                    |    out.b <= add(x.a.a2, x.b)
                    |""".stripMargin
    
    val circuit = Parser.parse(input)
    val state = CircuitState(circuit, MidForm, Seq.empty, None)
    val connections = new ArrayBuffer[Connect]()
    f.execute(state).circuit.modules.filter(_.name == component.module.name).foreach(_.mapStmt(collect(connections)))
    connections.map(_.serialize) should contain ("x_fault.a.a1 <= asUInt(bits(IdentityInjector.io.out, 6, 6))")
    connections.map(_.serialize) should contain ("x_fault.a.a2 <= asUInt(bits(IdentityInjector.io.out, 5, 3))")
    connections.map(_.serialize) should contain ("x_fault.b <= asUInt(bits(IdentityInjector.io.out, 2, 0))")
    connections.last.expr.serialize should be ("cat(cat(asUInt(x.a.a1), asUInt(x.a.a2)), asUInt(x.b))")
  }

  it should "inject faults into a combined vector and bundle type" in {
    val component = ComponentName("x", ModuleName("top", CircuitName("top")))
    val compMap = Map(component.module.name -> Seq((component, "dummyId", classOf[IdentityInjector])))
    val f = new FaultInstrumentation(compMap)

    val tpe = "{a: {a1: UInt<1>, a2: UInt<1>[2]}, b: UInt<3>}[2]"
    val input = s"""|circuit top:
                    |  module top:
                    |    input clock: Clock
                    |    input in: $tpe
                    |    output out: UInt<1>
                    |    reg x: $tpe, Clock
                    |    x <= in
                    |    out <= orr(asUInt(x))
                    |""".stripMargin
    
    val circuit = Parser.parse(input)
    val state = CircuitState(circuit, MidForm, Seq.empty, None)
    val connections = new ArrayBuffer[Connect]()
    f.execute(state).circuit.modules.filter(_.name == component.module.name).foreach(_.mapStmt(collect(connections)))
    connections.map(_.serialize) should contain ("x_fault[0].b <= asUInt(bits(IdentityInjector.io.out, 2, 0))")
    connections.map(_.serialize) should contain ("x_fault[0].a.a2[0] <= asUInt(bits(IdentityInjector.io.out, 3, 3))")
    connections.map(_.serialize) should contain ("x_fault[0].a.a2[1] <= asUInt(bits(IdentityInjector.io.out, 4, 4))")
    connections.map(_.serialize) should contain ("x_fault[0].a.a1 <= asUInt(bits(IdentityInjector.io.out, 5, 5))")
    connections.map(_.serialize) should contain ("x_fault[1].b <= asUInt(bits(IdentityInjector.io.out, 8, 6))")
    connections.map(_.serialize) should contain ("x_fault[1].a.a2[0] <= asUInt(bits(IdentityInjector.io.out, 9, 9))")
    connections.map(_.serialize) should contain ("x_fault[1].a.a2[1] <= asUInt(bits(IdentityInjector.io.out, 10, 10))")
    connections.map(_.serialize) should contain ("x_fault[1].a.a1 <= asUInt(bits(IdentityInjector.io.out, 11, 11))")
    connections.last.expr.serialize should be ("cat(cat(cat(asUInt(x[1].a.a1), cat(asUInt(x[1].a.a2[1]), asUInt(x[1].a.a2[0]))), asUInt(x[1].b)), cat(cat(asUInt(x[0].a.a1), cat(asUInt(x[0].a.a2[1]), asUInt(x[0].a.a2[0]))), asUInt(x[0].b)))")
  }

  it should "skip over 0-width bundle fields" in {
    val component = ComponentName("x", ModuleName("top", CircuitName("top")))
    val compMap = Map(component.module.name -> Seq((component, "dummyId", classOf[IdentityInjector])))
    val f = new FaultInstrumentation(compMap)

    val tpe = "{a: UInt<3>, b: UInt<5>}"
    val input = """|circuit top:
                   |  module top:
                   |    input clock: Clock
                   |    input in: {a: UInt<3>, b: UInt<5>[0]}
                   |    output out: UInt<3>
                   |    reg x: {a: UInt<3>, b: UInt<5>[0]}, Clock
                   |    x <= in
                   |    out <= x.a
                   |""".stripMargin

    val circuit = Parser.parse(input)
    val state = CircuitState(circuit, MidForm, Seq.empty, None)
    val connections = new ArrayBuffer[Connect]()
    f.execute(state).circuit.modules.filter(_.name == component.module.name).foreach(_.mapStmt(collect(connections)))
    connections.foreach(c => println(c.serialize))
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
    a [FaultInstrumentationException] should be thrownBy (f.execute(state))
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
