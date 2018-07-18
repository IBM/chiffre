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

import chiffre.passes.{ScanChainTransform, ScanChainAnnotation, ScanChainInjectorAnnotation,
  ScanChainDescriptionAnnotation}
import chiffre.inject.NoInjectorInfo

import firrtl._
import firrtl.annotations.{ComponentName, ModuleName, CircuitName}
import firrtl.passes.wiring.{SourceAnnotation, SinkAnnotation}
import chisel3.iotesters.ChiselFlatSpec
import java.io.File

class ScanChainTransformSpec extends ChiselFlatSpec {
  behavior of "ScanChainTransform"

  val targetDir = "test_run_dir"

  def fileShouldExist(fileName: String): Unit = {
    val file = new File(fileName)
    info(s"$fileName was created")
    file.exists() should be (true)
    file.delete()
  }

  it should "emit annotations to wire a scan chain" in {
    val input =
      """|circuit Top:
         |  module Top:
         |    input clock: Clock
         |    reg x: UInt<1>, clock
         |    inst controller of Controller
         |    inst injector of Injector
         |  module Injector:
         |    input clock: Clock
         |  module Controller:
         |    input clock: Clock
         |""".stripMargin

    val circuit = Parser.parse(input)
    val m = ModuleName("Top", CircuitName("Top"))
    val annos = Seq(
      ScanChainAnnotation(ComponentName("controller.io.scan", m), "master", "scan", "foo", None),
      ScanChainInjectorAnnotation(ComponentName("x", m), "foo", "Injector"),
      ScanChainAnnotation(ComponentName("injector.io.scan.in", m), "slave", "in", "foo", Some(ComponentName("x", m))),
      ScanChainAnnotation(ComponentName("injector.io.scan.out", m), "slave", "out", "foo", Some(ComponentName("x", m))),
      ScanChainDescriptionAnnotation(ModuleName("Injector", CircuitName("Top")), "foo", NoInjectorInfo),
      TargetDirAnnotation(targetDir)
    )
    val state = CircuitState(circuit, MidForm, annos, None)

    val f = new ScanChainTransform
    val output = f.execute(state)

    val annosExpected = Set(
      // Broadcast connections, scan enable and scan clock
      SourceAnnotation(ComponentName("controller.io.scan.en", m), "scan_en"),
      SourceAnnotation(ComponentName("controller.io.scan.clk", m), "scan_clk"),
      // Scan chain connections
      SourceAnnotation(ComponentName("controller.io.scan.out", m), "scan_foo_0"),
      SinkAnnotation(ComponentName("injector.io.scan.in", m), "scan_foo_0"),
      SourceAnnotation(ComponentName("injector.io.scan.out", m), "scan_foo_1"),
      SinkAnnotation(ComponentName("controller.io.scan.in", m), "scan_foo_1")
    )

    annosExpected.foreach(a => output.annotations.toSet should contain (a))
    fileShouldExist(targetDir + "/scan-chain.json")
  }

  // [todo] This currently fails due to (I think) the same reasons as #16.
  // There is no way to disambiguate the injectors for registers x and y.
  // This may be fixed by #18.
  ignore should "emit annotations to wire a scan chain for multiply instantiated injectors" in {
    val input =
      """|circuit Top:
         |  module Top:
         |    input clock: Clock
         |    reg x: UInt<1>, clock
         |    reg y: UInt<1>, clock
         |    inst controller of Controller
         |    inst injector_0 of Injector
         |    inst injector_1 of Injector
         |  module Injector:
         |    input clock: Clock
         |  module Controller:
         |    input clock: Clock
         |""".stripMargin

    val circuit = Parser.parse(input)
    val m = ModuleName("Top", CircuitName("Top"))
    val annos = Seq(
      ScanChainAnnotation(ComponentName("controller.io.scan", m), "master", "scan", "foo", None),
      ScanChainInjectorAnnotation(ComponentName("x", m), "foo", "Injector"),
      ScanChainAnnotation(ComponentName("injector_0.io.scan.in", m), "slave", "in", "foo", Some(ComponentName("x", m))),
      ScanChainAnnotation(ComponentName("injector_0.io.scan.out", m), "slave", "out", "foo", Some(ComponentName("x", m))),
      ScanChainDescriptionAnnotation(ModuleName("Injector", CircuitName("Top")), "foo", NoInjectorInfo),
      ScanChainInjectorAnnotation(ComponentName("y", m), "foo", "Injector"),
      ScanChainAnnotation(ComponentName("injector_1.io.scan.in", m), "slave", "in", "foo", Some(ComponentName("x", m))),
      ScanChainAnnotation(ComponentName("injector_1.io.scan.out", m), "slave", "out", "foo", Some(ComponentName("x", m))),
      ScanChainDescriptionAnnotation(ModuleName("Injector", CircuitName("Top")), "foo", NoInjectorInfo),
      TargetDirAnnotation(targetDir)
    )
    val state = CircuitState(circuit, MidForm, annos, None)

    val f = new ScanChainTransform
    val output = f.execute(state)

    output.annotations.foreach(a => println(a.serialize))

    val annosExpected = Set(
      // Broadcast connections, scan enable and scan clock
      SourceAnnotation(ComponentName("controller.io.scan.en", m), "scan_en"),
      SourceAnnotation(ComponentName("controller.io.scan.clk", m), "scan_clk"),
      // Scan chain connections
      SourceAnnotation(ComponentName("controller.io.scan.out", m), "scan_foo_0"),
      SinkAnnotation(ComponentName("injector_0.io.scan.in", m), "scan_foo_0"),
      SourceAnnotation(ComponentName("injector_0.io.scan.out", m), "scan_foo_1"),
      SinkAnnotation(ComponentName("injector_1.io.scan.in", m), "scan_foo_1"),
      SourceAnnotation(ComponentName("injector_1.io.scan.out", m), "scan_foo_2"),
      SinkAnnotation(ComponentName("controller.io.scan.in", m), "scan_foo_2")
    )

    annosExpected.foreach(a => output.annotations.toSet should contain (a))
  }
}
