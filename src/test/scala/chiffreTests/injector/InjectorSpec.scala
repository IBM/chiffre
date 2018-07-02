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

import chiffre.ScanField
import chiffre.inject.{Injector, LfsrInjectorInfo}
import chisel3.iotesters.PeekPokeTester

case class Chunk(width: Int) extends ScanField

class InjectorTester[T <: Injector](dut: T) extends PeekPokeTester(dut) {
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
    poke(dut.io.scan.clk, 0)
    out.toString
  }

  def load(in: Chunk): Chunk = {
    val out = in.copy()
    out.bind(BigInt(load(in.toBits).reverse.toString, 2))
    out
  }

  def load(in: LfsrInjectorInfo): LfsrInjectorInfo = {
    var outS: String = load(in.toBits.reverse).reverse.toString
    val out = in.copy()
    out.fields.foreach{ f =>
      val (car, cdr) = outS.splitAt(f.width)
      f.bind(BigInt(car, 2))
      outS = cdr
    }
    out
  }
}

/** Test that data cycled in can also be cycled out
  *
  * @param dut an injector to be tested
  */
class InjectorCycleTester[T <: Injector](dut: T) extends InjectorTester(dut) {
  val scanId = "dummy"

  val ones = new Chunk(dut.info.width)
  ones.bind(ones.maxValue)

  val zeros = new Chunk(dut.info.width)
  zeros.bind(0)

  poke(dut.io.scan.en, 0)
  load(ones)
  val outOnes = load(zeros)
  assert(outOnes == ones, "Expected all ones at output, got something else")

  val outZeros = load(zeros)
  assert(outZeros == zeros, "Expected all zeros at output, got something else")
}
