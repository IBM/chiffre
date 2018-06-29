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

import chisel3.iotesters.ChiselFlatSpec

class CycleInjectorSpec extends ChiselFlatSpec {
  behavior of "CycleInjectorInfo"

  it should "generate a sensible name" in (pending)
  it should "be the expected width" in (pending)

  behavior of "CycleInjector"

  it should "be able to cycle a configuration" in (pending)
  it should "inject with a delay after being enabled" in (pending)
}
