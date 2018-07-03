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
package chiffreTests.inject

import chiffre.inject.{StuckAtInjector, StuckAtInjectorInfo, Mask, StuckAt}
import chisel3.iotesters.{ChiselFlatSpec, Driver}

class StuckAtTester(dut: StuckAtInjector) extends InjectorTester(dut) {
  require(dut.bitWidth == 8)
  val mask = BigInt("11110000", 2)
  val stuckAt = BigInt("00110000", 2)
  val in = BigInt("01010101", 2)
  val faulty = BigInt("00110101", 2)

  dut.info.fields.foreach {
    case m: Mask => m.bind(mask)
    case s: StuckAt => s.bind(stuckAt)
  }

  poke(dut.io.scan.en, 0)
  poke(dut.io.in, in)
  load(dut.info)
  poke(dut.io.scan.en, 1)
  step(1)
  poke(dut.io.scan.en, 0)
  step(1)

  val fault = peek(dut.io.out)
  assert(fault == faulty, s"Expected to see $faulty, but got $fault")

  poke(dut.io.scan.en, 1)
  step(1)
  poke(dut.io.scan.en, 0)
  val noFault = peek(dut.io.out)
  assert(noFault == in, s"Expected to see $in, but got $noFault")
}

class StuckAtInjectorSpec extends ChiselFlatSpec {
  behavior of "StuckAtInjectorInfo"

  it should "generate a sensible name" in {
    val x = StuckAtInjectorInfo(9001)
    x.name should be ("stuckAt")
  }

  it should "be the expected width" in {
    val x = StuckAtInjectorInfo(1337)
    x.width should be (1337 * 2)
  }

  behavior of "StuckAtInjector"

  it should "be able to cycle a configuration" in {
    Driver(() => new StuckAtInjector(13)) { dut => new InjectorCycleTester(dut) }
  }

  it should "make a signal stuck when enabled" in {
    Driver(() => new StuckAtInjector(8)) { dut => new StuckAtTester(dut) }
  }
}
