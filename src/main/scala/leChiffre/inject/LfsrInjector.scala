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

class LfsrInjector(lfsrWidth: Int, id: String) extends OneBitInjector(id) {
  val difficulty = Reg(init = 0.U(lfsrWidth.W))
  val seed = Reg(init = 1.U(lfsrWidth.W))
  lazy val info = LfsrInjectorInfo(1, lfsrWidth)

  val lfsr = Module(new perfect.random.Lfsr(lfsrWidth))
  lfsr.io.seed.valid := io.scan.en
  lfsr.io.seed.bits := seed

  val fire = enabled && (lfsr.io.y < difficulty)
  io.out := Mux(fire, ~io.in, io.in)

  when (io.scan.clk) {
    enabled := false.B
    seed := io.scan.in ## (seed >> 1)
    difficulty := seed(0) ## (difficulty >> 1)
  }

  io.scan.out := difficulty(0)

  when (fire) {
    printf(s"[info] $name fire\n")
  }

  when (io.scan.en && !enabled) {
    printf(s"""|[info] $name enabled
               |[info]   - seed: 0x%x
               |[info]   - difficulty: 0x%x
               |""".stripMargin, seed, difficulty)
  }

  when (io.scan.en && enabled) {
    printf(s"[info] $name disabled\n")
  }
}

class LfsrInjector32(n: Int, id: String)
    extends InjectorBitwise(n, id, new LfsrInjector(32, id)) {
  lazy val info = LfsrInjectorInfo(n, 32)
}
