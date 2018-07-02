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
package chiffre.inject

import chisel3._
import chisel3.util._
import chisel3.core.BaseModule
import chisel3.experimental.{ChiselAnnotation, annotate, RunFirrtlTransform}
import chiffre._
import chiffre.passes._
import chiffre.scan._

/** An injector interface */
sealed class InjectorIo(val n: Int) extends Bundle {
  val scan = new ScanIo
  val in = Input(UInt(n.W))
  val out = Output(UInt(n.W))
}

/** The sketch of an injector module */
abstract class Injector(n: Int) extends Module with HasScanState {
  val io = IO(new InjectorIo(n))
  val enabled = RegInit(false.B)
  when (io.scan.en) { enabled := ~enabled }
}

trait ChiffreInjector { this: Injector =>
  val scanId: String
  experimental.annotate {
    val x = this
    new ChiselAnnotation with RunFirrtlTransform {
      def toFirrtl = ScanChainDescriptionAnnotation(x.toNamed, scanId, info)
      def transformClass = classOf[ScanChainTransform]
    }}
}

/** An injector composed of individual single-bit injectors */
abstract class InjectorBitwise(bitWidth: Int, gen: => Injector) extends Injector(bitWidth) {
  lazy val injectors = Seq.fill(bitWidth)(Module(gen))

  var scanLast = io.scan.in
  injectors
    .zipWithIndex
    .map{ case (injector, i) =>
      injector.io.in := io.in(i)

      injector.io.scan.clk := io.scan.clk
      injector.io.scan.en := io.scan.en
      injector.io.scan.in := scanLast
      scanLast = injector.io.scan.out
    }
  io.scan.out := scanLast
  io.out := Cat(injectors.reverse.map(_.io.out))
}
