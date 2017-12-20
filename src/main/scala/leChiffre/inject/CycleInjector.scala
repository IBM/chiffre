// Copyright 2017 IBM
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
package leChiffre.inject

import chisel3._
import chisel3.util._
import leChiffre.scan._

class CycleInjector(n: Int, cycleWidth: Int, id: String) extends Injector(n, id) {
  val cycleTarget = Reg(UInt(cycleWidth.W))
  val cycleCounter = Reg(UInt(cycleWidth.W))
  val flipMask = Reg(UInt(n.W))

  lazy val info = CycleInjectorInfo(n, cycleWidth)

  val fire = enabled & (cycleCounter === cycleTarget)
  io.out := Mux(fire, io.in ^ flipMask, io.in)

  when (enabled) {
    cycleCounter := cycleCounter + 1.U
  }

  when (io.scan.clk) {
    enabled := false.B
    cycleCounter := 0.U
    cycleTarget := io.scan.in ## (cycleTarget >> 1)
    flipMask := cycleTarget(0) ## (flipMask >> 1)
  }
  io.scan.out := flipMask(0)

  when (io.scan.en && !enabled) {
    printf(s"""|[info] $name enabled
               |[info]   - target: 0x%x
               |[info]   - mask: 0x%x
               |""".stripMargin, cycleTarget, flipMask)
  }

  when (io.scan.en && enabled) {
    printf(s"[info] $name disabled\n")
  }

  when (fire) {
    printf(s"[info] $name injecting!\n")
  }
}

class CycleInjector32(n: Int, id: String) extends CycleInjector(n, 32, id)
