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
import chisel3.util._
import chiffre.scan._

import chiffre.{ScanField, InjectorInfo, ProbabilityBind}

case class Seed(width: Int) extends ScanField
case class Difficulty(width: Int) extends ScanField with ProbabilityBind

case class LfsrInjectorInfo(bitWidth: Int, lfsrWidth: Int) extends InjectorInfo {
  val name = s"lfsr$lfsrWidth"
  val fields = Seq.fill(bitWidth)(Seq(Seed(lfsrWidth), Difficulty(lfsrWidth))).flatten
}

sealed class LfsrInjector(val lfsrWidth: Int) extends Injector(1) {
  val difficulty = RegInit(0.U(lfsrWidth.W))
  val seed = RegInit(1.U(lfsrWidth.W))
  lazy val info = LfsrInjectorInfo(1, lfsrWidth)

  val lfsr = Module(new perfect.random.Lfsr(lfsrWidth))
  lfsr.io.seed.valid := io.scan.en
  lfsr.io.seed.bits := seed

  val fire = enabled && (lfsr.io.y <= difficulty)
  io.out := Mux(fire, ~io.in, io.in)

  when (io.scan.clk) {
    enabled := false.B
    seed := io.scan.in ## (seed >> 1)
    difficulty := seed(0) ## (difficulty >> 1)
  }

  io.scan.out := difficulty(0)

  when (enabled && RegNext(!enabled)) {
    printf(s"""|[info] $name enabled
               |[info]   - seed: 0x%x
               |[info]   - difficulty: 0x%x
               |""".stripMargin, seed, difficulty)
  }

  when (!enabled && RegNext(enabled)) {
    printf(s"[info] $name disabled\n")
  }
}

class LfsrInjectorN(bitWidth: Int, val lfsrWidth: Int, val scanId: String) extends
    InjectorBitwise(bitWidth, new LfsrInjector(bitWidth)) with ChiffreInjector {
  lazy val info = LfsrInjectorInfo(bitWidth, lfsrWidth)
}

class LfsrInjector32(bitWidth: Int, scanId: String) extends LfsrInjectorN(bitWidth, 32, scanId)
