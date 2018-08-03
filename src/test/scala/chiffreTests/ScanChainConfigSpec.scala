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

import chiffre.{FaultyComponent, ScanFieldException}
import chiffre.scan.{ScanChain, JsonProtocol}
import chiffre.inject.{StuckAtInjectorInfo, LfsrInjectorInfo, CycleInjectorInfo}
import chiffre.util.{Driver, ScanChainException}
import chisel3.iotesters.ChiselFlatSpec

import java.io.{File, FileWriter}

class ScanChainConfigSpec extends ChiselFlatSpec {

  val test_dir = s"test_run_dir/${this.getClass.getSimpleName}"

  def writeScanChainToFile(scanChain: ScanChain, fileName: String): Unit = {
    val dir = new File(test_dir)
    if (!dir.exists()) { dir.mkdirs() }
    val file = new File(fileName)
    val w = new FileWriter(file)
    w.write(JsonProtocol.serialize(scanChain))
    w.close()
  }

  it should "error if all scan chain fields are unbound" in {
    val scanFile = s"$test_dir/scan-chain-0.json"
    writeScanChainToFile(Map("id-0" -> Seq(FaultyComponent("Top.Top.x", StuckAtInjectorInfo(5)))), scanFile)
    a [ScanChainException] should be thrownBy (Driver.main(Array(scanFile)))
  }

  it should "error if any scan chain fields are unbound" in {
    val scanFile = s"$test_dir/scan-chain-0.json"
    val dir = new File(test_dir)
    if (!dir.exists()) { dir.mkdirs() }
    val w = new FileWriter(new File(scanFile))
    w.write("""|{
               |  "id-2":[
               |    {
               |      "name":"Top.Top.x",
               |      "injector":{
               |        "class":"chiffre.inject.StuckAtInjectorInfo",
               |        "bitWidth":5,
               |        "fieldValues":[31]
               |      }
               |    }
               |  ]
               |}""".stripMargin)
    w.close()
    a [ScanFieldException] should be thrownBy (Driver.main(Array(scanFile)))
  }

  it should "work if all scan chain fields are bound" in {
    val scanFile = s"$test_dir/scan-chain-2.json"
    writeScanChainToFile(Map("id-2" -> Seq(FaultyComponent("Top.Top.x", StuckAtInjectorInfo(5, Some(Seq(31, 23)))))), scanFile)
    Driver.main(Array(scanFile))
  }

  it should "properly compute the Fletcher checksum" in (pending)
  it should "report the correct width" in (pending)
}
