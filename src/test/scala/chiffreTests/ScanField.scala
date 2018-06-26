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

import chiffre.{ScanField, ScanFieldException}
import chiffre.inject.Difficulty
import chisel3.iotesters.ChiselFlatSpec

case class DummyField(width: Int, value: Option[BigInt] = None) extends ScanField

class ScanFieldSpec extends ChiselFlatSpec {

  def backToInt(f: ScanField): Int = Integer.parseInt(f.toBits, 2)

  behavior of "ScanField"

  it should "throw a ScanFieldException if initialized to a non-zero value" in {
    a [ScanFieldException] should be thrownBy (new DummyField(0))
  }

  it should "throw a ScanFieldException if value is outside domain inferred from the width" in {
    val x = DummyField(8)
    a [ScanFieldException] should be thrownBy (x.copy(value = Some(-1)))
    a [ScanFieldException] should be thrownBy (x.copy(value = Some(256)))
  }

  it should "serialize bits correctly" in {
    val x = DummyField(8)

    (0 until x.maxValue.toInt + 1).foreach( v => v should be (backToInt(x.copy(value = Some(v)))) )
  }

  behavior of "Difficulty"

  it should "throw a ScanFieldException if the probability is nonsensical" in {
    val x = Difficulty(width = 24)
    a [ScanFieldException] should be thrownBy (x.copy(probability = Some(-0.1f)))
    a [ScanFieldException] should be thrownBy (x.copy(probability = Some(1.1f)))
  }

  it should "map 1.0 probability should to maxValue" in {
    backToInt(Difficulty(width = 23, Some(0))) should be (0)
  }

  it should "map 0.0 probability to 0" in {
    val x = Difficulty(width = 23, Some(1))
    backToInt(x) should be (x.maxValue)
  }
}
