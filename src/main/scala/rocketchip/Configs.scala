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
import leChiffre._
import freechips.rocketchip.config._
import freechips.rocketchip.tile.{RoCCParams, OpcodeSet}
import freechips.rocketchip.coreplex.RocketTilesKey
import freechips.rocketchip.system.DefaultConfig
import freechips.rocketchip.diplomacy.LazyModule

class WithLeChiffre extends Config ((site, here, up) => {
  case BuildChiffre => ChiffreParameters()
  case RocketTilesKey => up(RocketTilesKey, site) map { r =>
    r.copy(rocc = Seq(
      RoCCParams(
        opcodes = OpcodeSet.custom2,
        generator = (p: Parameters) =>  {
          val leChiffre = LazyModule(new LeChiffre()(p))
          leChiffre})
    ))
  }
})

class LeChiffreConfig extends Config(new WithLeChiffre ++ new DefaultConfig)
