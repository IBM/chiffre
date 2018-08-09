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

import chiffre.FaultyComponent
import chiffre.scan.{ScanChain, JsonProtocol}
import chiffre.inject.{StuckAtInjectorInfo, LfsrInjectorInfo, CycleInjectorInfo}
import chiffre.util.{Driver, ScanChainException}
import chisel3.iotesters.ChiselFlatSpec

import java.io.{File, FileWriter}

class ScanChainConfigSpec extends ChiselFlatSpec {

  val test_dir = s"test_run_dir/${this.getClass.getSimpleName}"

  val scanChain: ScanChain = Map(
    "id-0" -> Seq(
      FaultyComponent("Top.Top.x", StuckAtInjectorInfo(5)),
      FaultyComponent("Top.Top.y", LfsrInjectorInfo(4, 32)),
      FaultyComponent("Top.Top.z", CycleInjectorInfo(3, 8))
    )
  )
  val scanFile = s"$test_dir/scan-chain.json"
  writeScanChainToFile(scanChain, scanFile)

  val scanChainBound: ScanChain = Map(
    "id-0" -> Seq(
      FaultyComponent("Top.Top.x", StuckAtInjectorInfo(5))
    )
  )
  scanChainBound("id-0")(0).injector.fields(0).bind(1)
  val scanBoundFile = s"$test_dir/scan-chain-partially-bound.json"
  writeScanChainToFile(scanChainBound, scanBoundFile)

  def writeScanChainToFile(scanChain: ScanChain, fileName: String): Unit = {
    val dir = new File(test_dir)
    if (!dir.exists()) { dir.mkdirs() }
    val file = new File(fileName)
    val w = new FileWriter(file)
    w.write(JsonProtocol.serialize(scanChain))
    w.close()
  }

  behavior of "ScanChainConfig"

  it should "error if missing command line arguments" in {
    val args = Array(scanFile)
    (the [ScanChainException] thrownBy {
      Driver.main(args)
    }).msg should startWith ("Unable to bind")
  }

  it should "work if all command line arguments are specified" in {
    val args = Array("--probability", "0.5",
                     "--mask", "3",
                     "--stuck-at", "2",
                     "--cycle", "13",
                     "--cycle-inject", "4",
                     scanFile)
    Driver.main(args)
  }

  it should "not override existing fields" in {
    val dir = s"$test_dir/override"
    val args = Array("--mask", "3",
                     "--stuck-at", "2",
                     "--output-dir", dir,
                     scanBoundFile)
    Driver.main(args)

    val roundTrip: ScanChain = JsonProtocol.deserialize(new File(s"$dir/bound.json"))
    roundTrip("id-0")(0).injector.fields(0).value should be (Some(1))
  }

  it should "throw an exception if binding a bound field with --error-if-bound" in {
    val args = Array("--mask", "3",
                     "--stuck-at", "2",
                     "--error-if-bound",
                     scanBoundFile)
    (the [ScanChainException] thrownBy {
      Driver.main(args)
    }).msg should startWith ("Tried to rebind already bound ScanField")
  }

  it should "properly compute the Fletcher checksum" in (pending)
  it should "report the correct width" in (pending)
}
