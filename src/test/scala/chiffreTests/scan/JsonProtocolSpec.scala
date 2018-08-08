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
package chiffreTests.scan

import chiffre.{FaultyComponent, InjectorInfo}
import chiffre.scan.{ScanChain, JsonProtocol}
import chiffre.inject.{StuckAtInjectorInfo, LfsrInjectorInfo, CycleInjectorInfo}
import chisel3.iotesters.ChiselFlatSpec

case class TestInfo(hello: Option[Int] = None) extends InjectorInfo {
  val fields = Seq.empty
}

class JsonProtocolSpec extends ChiselFlatSpec {

  val scanChain: ScanChain = Map(
    "id-0" -> Seq(
      FaultyComponent("Top.Top.a", StuckAtInjectorInfo(5)),
      FaultyComponent("Top.Top.b", LfsrInjectorInfo(4, 32)),
      FaultyComponent("Top.Top.c", CycleInjectorInfo(3, 8))
    )
  )

  behavior of "JsonProtocol"

  it should "round trip from ScanChain -> JSON -> ScanChain" in {

    info("binding one of the values")
    scanChain("id-0")(0).injector.fields(0).bind(BigInt(1))
    val json: String = JsonProtocol.serialize(scanChain)
    println(json)
    val roundTrip: ScanChain = JsonProtocol.deserialize(json)

    info("round trip matches with bound value")
    roundTrip should be (scanChain)

    roundTrip("id-0")(0).injector.unbind
    info("unbinding the bound value causes it to not match")
    roundTrip should not be (scanChain)
  }
}
