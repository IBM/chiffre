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
package chiffre.inject

import chisel3._
import chiffre.ChiffreInjector

import chiffre.{InjectorInfo, ScanField, HasWidth}

case class Cycle(width: Int) extends ScanField
case class CycleInject(width: Int) extends ScanField

case class CycleInjectorInfo(bitWidth: Int, cycleWidth: Int) extends InjectorInfo {
  val fields = Seq(Cycle(cycleWidth), CycleInject(bitWidth))
}

class CycleInjector(bitWidth: Int, val cycleWidth: Int) extends Injector(bitWidth) {
  val cycleTarget = Reg(UInt(cycleWidth.W))
  val cycleCounter = Reg(UInt(cycleWidth.W))
  val flipMask = Reg(UInt(bitWidth.W))

  lazy val info = CycleInjectorInfo(bitWidth, cycleWidth)

  val fire = enabled & (cycleCounter === cycleTarget)
  io.out := Mux(fire, io.in ^ flipMask, io.in)

  when (enabled) {
    cycleCounter := cycleCounter + 1.U
  }

  when (io.scan.clk) {
    enabled := false.B
    cycleCounter := 0.U
    cycleTarget := (io.scan.in ## cycleTarget) >> 1
    flipMask := (cycleTarget(0) ## flipMask) >> 1
  }
  io.scan.out := flipMask(0)

  when (enabled && RegNext(!enabled)) {
    printf(s"""|[info] $name enabled
               |[info]   - target: 0x%x
               |[info]   - mask: 0x%x
               |""".stripMargin, cycleTarget, flipMask)
  }

  when (!enabled && RegNext(enabled)) {
    printf(s"[info] $name disabled\n")
  }

  when (fire) {
    printf(s"[info] $name injecting 0x%x into 0x%x to output 0x%x!\n", flipMask, io.in, io.out)
    enabled := false.B
  }
}

// scalastyle:off magic.number
class CycleInjector32(bitWidth: Int, val scanId: String) extends CycleInjector(bitWidth, 32) with ChiffreInjector
// scalastyle:on magic.number
