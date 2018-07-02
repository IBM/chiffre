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

import chiffre.inject.{CycleInjectorInfo, CycleInjector}
import chisel3.iotesters.{ChiselFlatSpec, Driver}

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

  it should "be able to cycle a configuration" in {
    Driver(() => new CycleInjector(4, 8)) { dut => new InjectorCycleTester(dut) }
  }

  it should "inject with a delay after being enabled" in (pending)
}
