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
import chisel3.util.Fill
import chiffre.ChiffreInjector

import chiffre.{ScanField, InjectorInfo}

case class Mask(width: Int) extends ScanField
case class StuckAt(width: Int) extends ScanField

case class StuckAtInjectorInfo(bitWidth: Int) extends InjectorInfo {
  val fields = Seq(Mask(bitWidth), StuckAt(bitWidth))
}

class StuckAtInjector(val bitWidth: Int) extends Injector(bitWidth) {
  val mask = Reg(UInt(bitWidth.W))
  val value = Reg(UInt(bitWidth.W))

  lazy val info = StuckAtInjectorInfo(bitWidth)

  val select = mask & Fill(mask.getWidth, enabled)
  io.out := (io.in & ~select) | (value & select)

  when (io.scan.clk) {
    enabled := false.B
    mask := (io.scan.in ## mask) >> 1
    value := (mask(0) ## value) >> 1
  }

  io.scan.out := value(0)

  when (io.scan.en && !enabled) {
    printf(s"""|[info] $name enabled
               |[info]   - mask: 0x%x
               |[info]   - value: 0x%x
               |""".stripMargin, mask, value)
  }

  when (io.scan.en && enabled) {
    printf(s"[info] $name disabled\n")
  }
}

class StuckAtInjectorWithId(bitWidth: Int, val scanId: String) extends StuckAtInjector(bitWidth) with ChiffreInjector
