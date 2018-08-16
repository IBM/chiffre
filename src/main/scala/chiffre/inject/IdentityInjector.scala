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

import chiffre.{InjectorInfo, ChiffreInjector}

import chisel3._

case object NoInjectorInfo extends InjectorInfo {
  val fields = Seq.empty
}

class IdentityInjector(bitWidth: Int, val scanId: String) extends Injector(bitWidth: Int) with ChiffreInjector {
  val info = NoInjectorInfo
  io.out := io.in
  io.scan.out := io.scan.in
}
