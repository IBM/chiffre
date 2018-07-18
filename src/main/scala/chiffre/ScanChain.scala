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
package chiffre

import chisel3._
import chisel3.core.BaseModule

class ScanIo extends Bundle {
  val clk = Input(Bool())
  val en = Input(Bool())
  val in = Input(Bool())
  val out = Output(Bool())
}

trait HasScanState { this: BaseModule =>
  val info: InjectorInfo
}

case class FaultyComponent(name: String, injector: InjectorInfo) {
  def serialize(indent: String): String =
    s"""|${indent}- $name:
        |${injector.serialize(indent + "  ")}"""
      .stripMargin

  def toBits(): String = injector.toBits()
}
