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

import chiffre.{InjectorInfo, ScanField}
import chisel3.iotesters.ChiselFlatSpec

case object EmptyInjectorInfo extends InjectorInfo {
  val name = "DummyInjectorInfo"
  val fields = Seq.empty
}

class InjectorInfoSpec extends ChiselFlatSpec {

  case class DummyField(width: Int) extends ScanField
  case class DummyInjectorInfo(fields: Seq[ScanField]) extends InjectorInfo {
    val name = "dummy"
  }

  behavior of "The InjectoInfo trait"

  it should "have width 0 and report bound if without fields" in {
    val x = EmptyInjectorInfo
    x.width should be (0)
    x.isBound should be (true)
  }

  it should "report the width as the sum of its fields" in {
    val widths = Range(1, 10, 2)
    val x = new DummyInjectorInfo(widths.map(DummyField(_)))
    x.width should be (widths.sum)
  }

  it should "bind and unbind fields to values" in {
    val widths = Range(1, 10, 2)
    val x = new DummyInjectorInfo(widths.map(DummyField(_)))

    x.fields.foreach( _.value should be (None) )
    x.fields.foreach( _.isBound should be (false) )

    val values = widths.map(BigInt(2).pow(_) - 1)
    x.fields.zip(values).foreach{ case (f, v) => f match { case f: DummyField => f.bind(v) } }
    x.fields.zip(values).foreach{ case (f, v) => f.value should be (Some(v)) }
    x.fields.foreach( _.isBound should be (true) )

    x.fields.foreach(_.unbind)
    x.fields.foreach( _.value should be (None) )
    x.fields.foreach( _.isBound should be (false) )
  }
}
