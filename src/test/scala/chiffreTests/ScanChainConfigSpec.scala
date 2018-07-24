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

  def writeScanChainToFile(scanChain: ScanChain, fileName: String): Unit = {
    val dir = new File(test_dir)
    if (!dir.exists()) { dir.mkdirs() }
    val file = new File(s"$test_dir/scan-chain.json")
    val w = new FileWriter(file)
    w.write(JsonProtocol.serialize(scanChain))
    w.close()
  }

  behavior of "ScanChainConfig"

  it should "error if missing command line arguments" in {
    val scanFile = s"$test_dir/scan-chain.json"
    writeScanChainToFile(scanChain, scanFile)
    val args = Array(scanFile)
    (the [ScanChainException] thrownBy {
      Driver.main(args)
    }).msg should startWith ("Cannot bind ScanField")
  }

  it should "work if all command line arguments are specified" in {
    val scanFile = s"$test_dir/scan-chain.json"
    writeScanChainToFile(scanChain, scanFile)
    val args = Array("--probability", "0.5",
                     "--mask", "3",
                     "--stuck-at", "2",
                     "--cycle", "13",
                     "--cycle-inject", "4",
                     scanFile)
    Driver.main(args)
  }

  it should "properly compute the Fletcher checksum" in (pending)
  it should "report the correct width" in (pending)
}
