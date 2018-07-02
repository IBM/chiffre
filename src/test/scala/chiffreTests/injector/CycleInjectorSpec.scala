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

import chiffre.inject.{CycleInjectorInfo, CycleInjector, Cycle, CycleInject}
import chisel3.iotesters.{ChiselFlatSpec, Driver}

class TimingTester(dut: CycleInjector, time: Int) extends InjectorTester(dut) {
  val info = dut.info.copy()

  info.fields.foreach {
    case c: Cycle => c.bind(time)
    case i: CycleInject => i.bind(i.maxValue)
  }

  val correct = 1
  val faulty = info.fields.collectFirst{ case c: CycleInject => c }.get.maxValue ^ correct

  poke(dut.io.scan.en, 0)
  poke(dut.io.in, correct)
  load(info)
  poke(dut.io.scan.en, 1)
  step(1)
  poke(dut.io.scan.en, 0)
  step(time)
  val fault = peek(dut.io.out)
  assert(fault == faulty, s"Expected fault $time cycles after enabling, but no fault observed")
  step(1)

  var additionalFaults: BigInt = 0
  for (i <- 0 until math.pow(2, dut.cycleWidth).toInt) {
    additionalFaults += peek(dut.io.out) ^ correct
    step(1)
  }
  assert(additionalFaults == 0, "Saw additional faults after first fault")
}

class CycleInjectorSpec extends ChiselFlatSpec {
  behavior of "CycleInjectorInfo"

  it should "generate a sensible name" in {
    val x = CycleInjectorInfo(512, 1024)
    x.name should be (s"cycle1024")
  }

  it should "be the expected width" in {
    val x = CycleInjectorInfo(2048, 4096)
    x.width should be (2048 + 4096)
  }

  behavior of "CycleInjector"

  it should "be able to cycle a configuration with 1-bit fields" in {
    Driver(() => new CycleInjector(1, 1)) { dut => new InjectorCycleTester(dut) }
  }

  it should "inject with a delay after being enabled, then disable itself" in {
    Driver(()  => new CycleInjector(13, 8)) { dut => new TimingTester(dut, 42) }
  }
}
