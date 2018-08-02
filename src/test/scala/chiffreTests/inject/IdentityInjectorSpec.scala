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

import chiffre.inject.{NoInjectorInfo, IdentityInjector}
import chisel3.iotesters.{ChiselFlatSpec, Driver}

class IdentityInjectorTransparencyTester(dut: IdentityInjector) extends InjectorTester(dut) {
  poke(dut.io.scan.in, 0)
  assert(peek(dut.io.scan.out) == 0)

  poke(dut.io.scan.in, 1)
  assert(peek(dut.io.scan.out) == 1)
}

class IdentityInjectorTester(dut: IdentityInjector) extends InjectorTester(dut) {
  poke(dut.io.scan.en, 1)
  step(1)
  poke(dut.io.in, 0)
  assert(peek(dut.io.out) == 0)

  poke(dut.io.in, 1)
  assert(peek(dut.io.out) == 1)
}

class IdentityInjectorSpec extends ChiselFlatSpec {
  behavior of "NoInjectorInfo"

  it should "be the expected width" in {
    val x = NoInjectorInfo
    x.width should be (0)
  }

  behavior of "IdentityInjector"

  it should "have a combinational scan chain" in {
    Driver(() => new IdentityInjector(42, "dummy")) { dut => new IdentityInjectorTransparencyTester(dut) }
  }

  it should "do nothing when enabled" in {
    Driver(() => new IdentityInjector(21, "dummy")) { dut => new IdentityInjectorTester(dut) }
  }
}
