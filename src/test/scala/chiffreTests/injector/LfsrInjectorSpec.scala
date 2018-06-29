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

import chiffre.{ScanFieldException, SimpleScanField}
import chiffre.inject.{Difficulty, LfsrInjectorInfo, InjectorLike, LfsrInjector}
import chiffreTests.ChiffreSpecUtils.backToInt
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester, Driver}

import scala.collection.mutable.StringBuilder

class LfsrInjectSpec extends ChiselFlatSpec {
  behavior of "Difficulty ScanField"

  it should "throw a ScanFieldException if the probability is nonsensical" in {
    val x = Difficulty(width = 24)
    a [ScanFieldException] should be thrownBy (x.bind(-0.1))
    a [ScanFieldException] should be thrownBy (x.bind(1.1))
  }

  it should "map 1.0 probability should to maxValue" in {
    val x = Difficulty(width = 23)
    x.bind(0.0)
    backToInt(x) should be (0)
  }

  it should "map 0.0 probability to 0" in {
    val x = Difficulty(width = 23)
    x.bind(1.0)
    backToInt(x) should be (x.maxValue)
  }

  behavior of "LfsrInjectorInfo"

  it should "generate a sensible name" in {
    val x = LfsrInjectorInfo(512, 1024)
    x.name should be ("lfsr1024")
  }
  it should "be the expected width" in {
    val x = LfsrInjectorInfo(2048, 4096)
    x.width should be (2048 * 4096 * 2)
  }

  behavior of "LfsrInjector"

  class LfsrTester(dut: LfsrInjector) extends PeekPokeTester(dut) {
    case class Chunk(width: Int) extends SimpleScanField

    def load(bitString: String): String = {
      val out = new StringBuilder(bitString.size)
      bitString.map(x => s"$x".toInt).foreach{ bit =>
        poke(dut.io.scan.clk, 0)
        poke(dut.io.scan.in, bit)
        out ++= peek(dut.io.scan.out).toString
        step(1)
        poke(dut.io.scan.clk, 1)
        step(1)
      }
      poke(dut.io.scan.en, 0)
      out.toString
    }

    def load(in: Chunk): Chunk = {
      val out = in.copy()
      out.bind(BigInt(load(in.toBits).reverse.toString, 2))
      out
    }

    def load(in: LfsrInjectorInfo): LfsrInjectorInfo = {
      var outS: String = load(in.toBits).reverse.toString
      val out = in.copy()
      out.fields.foreach{ f =>
        val (car, cdr) = outS.splitAt(f.width)
        f.bind(BigInt(car, 2))
        outS = cdr
      }
      out
    }
  }

  class LfsrCycleTester(dut: LfsrInjector) extends LfsrTester(dut) {
    val ones = new Chunk(dut.info.width)
    ones.bind(ones.maxValue)

    val zeros = new Chunk(dut.info.width)
    zeros.bind(0)

    poke(dut.io.scan.en, 0)
    load(ones)
    val outOnes = load(zeros)
    assert(outOnes == ones)
    assert(outOnes != zeros)

    val outZeros = load(zeros)
    assert(outZeros == zeros)
    assert(outZeros != ones)
    assert(outZeros != outOnes)
  }

  it should "be able to cycle a configuration" in {
    Driver(() => new LfsrInjector(4, "dummy")) { dut => new LfsrCycleTester(dut) }
  }

  class ProbabilisticTester(dut: LfsrInjector, p: Double) extends LfsrTester(dut) {
    val info = LfsrInjectorInfo(1, dut.lfsrWidth)
  }

  it should "always fire for minimum difficulty" in {
    Driver(() => new LfsrInjector(32, "dummy")) { dut => new ProbabilisticTester(dut, 0.0) }
  }

  it should "never fire for maximum difficulty" in {
    Driver(() => new LfsrInjector(32, "dummy")) { dut => new ProbabilisticTester(dut, 0.0) }
  }

  it should "generate pseudo random faults when enabled" in (pending)
}
