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

import chiffre.ScanFieldBindingException
import chiffre.inject.{Seed, Difficulty, LfsrInjectorInfo, LfsrInjector, LfsrInjectorN}
import chiffreTests.ChiffreSpecUtils.backToInt
import chisel3.iotesters.{ChiselFlatSpec, Driver}

import scala.collection.mutable.StringBuilder

/** Checks that the expected probability matches the observed
  * probability */
class ProbabilisticTester(dut: LfsrInjector, probability: Double) extends InjectorTester(dut) {
  val info = LfsrInjectorInfo(1, dut.lfsrWidth)

  info.fields.foreach{
    case s: Seed => s.bind(1)
    case d: Difficulty => d.bind(probability)
  }

  poke(dut.io.scan.en, 0)
  poke(dut.io.in, 0)
  load(info)
  poke(dut.io.scan.en, 1)
  step(1)
  poke(dut.io.scan.en, 0)
  val iterations = math.pow(2, dut.lfsrWidth).toInt - 1
  var faultCount: BigInt = 0
  for (i <- 0 to iterations - 1) {
    faultCount += peek(dut.io.out)
    step(1)
  }
  poke(dut.io.scan.en, 1)
  step(1)
  println(s"Saw $faultCount faults in $iterations iterations")
  val expectedFaults = probability * iterations
  println(s"Expected faults: $expectedFaults")
  assert(expectedFaults.floor == faultCount, "Expected faults didn't match measured faults")
}

class LfsrInjectSpec extends ChiselFlatSpec {
  behavior of "Difficulty ScanField"

  it should "throw a ScanFieldException if the probability is nonsensical" in {
    val x = Difficulty(width = 24)
    info("when binding -0.1")
    a [ScanFieldBindingException] should be thrownBy (x.bind(-0.1))
    info("when binding 1.1")
    a [ScanFieldBindingException] should be thrownBy (x.bind(1.1))
  }

  it should "map 1.0 probability should to maxValue" in {
    val x = Difficulty(width = 23)
    x.bind(1.0)
    backToInt(x) should be (x.maxValue)
  }

  it should "map 0.0 probability to 0" in {
    val x = Difficulty(width = 23)
    x.bind(0.0)
    backToInt(x) should be (0)
  }

  behavior of "LfsrInjectorInfo"

  it should "be the expected width" in {
    val x = LfsrInjectorInfo(2048, 4096)
    x.width should be (2048 * 4096 * 2)
  }

  behavior of "LfsrInjector"

  it should "be able to cycle a configuration" in {
    Driver(() => new LfsrInjector(4)) { dut => new InjectorCycleTester(dut) }
  }

  Range(0, 11).map(_ / 10.0).map( probability =>
    it should s"fire expectedly for ${probability * 100}% probability" in {
      Driver(() => new LfsrInjector(8)) { dut => new ProbabilisticTester(dut, probability) }
    }
  )

  behavior of "LfsrInjectorN"

  it should "be able to cycle a configuration" in {
    Driver(() => new LfsrInjectorN(4, 16, "main")) { dut => new InjectorCycleTester(dut) }
  }
}
