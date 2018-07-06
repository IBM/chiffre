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
sealed abstract class InjectorLike(n: Int, id: String) extends Module
    with AddsScanState {
  val io = IO(new InjectorIo(n))
  val enabled = RegInit(false.B)
  when (io.scan.en) { enabled := ~enabled }
}

/** An injector that does not add its bits to the scan chain. Extend
  * this to make smaller injectors wrapped by a bigger [[Injector]]
  * that adds this module's bits to the chain, cf.
  * [[InjectorBitwise]].
  */
abstract class InjectorPrimitive(n: Int, id: String) extends InjectorLike(n, id)

/** An injector that adds bits to the scan chain */
abstract class Injector(n: Int, id: String) extends InjectorLike(n, id) {
  if (info == null) { // scalastyle:off
    throw new FaultInstrumentationException(
      "Children of Injector must use a `lazy val` for abstract member `info`") }
}

/** A one-bit injector primmitive */
abstract class OneBitInjector(id: String) extends InjectorPrimitive(1, id)

/** An injector composed of individual single-bit injectors */
abstract class InjectorBitwise(n: Int, id: String, gen: => OneBitInjector)
    extends Injector(n, id) {
  lazy val injectors = Seq.fill(n)(Module(gen))

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
