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
package freechips.rocketchip.system

import chisel3._
import chiffre.{ChiffreParameters, LeChiffre, BuildChiffre}
import freechips.rocketchip.config.{Config, Parameters}
import freechips.rocketchip.tile.{BuildRoCC, OpcodeSet}
import freechips.rocketchip.subsystem.RocketTilesKey
import freechips.rocketchip.diplomacy.LazyModule

class WithLeChiffre extends Config (
  (site, here, up) => {
    case BuildChiffre => ChiffreParameters()
    case BuildRoCC => List(
      (p: Parameters) => {
        val chiffre = LazyModule(new LeChiffre(OpcodeSet.custom2, "main")(p))
        chiffre })
  })

class LeChiffreConfig extends Config(new WithLeChiffre ++ new DefaultConfig)
