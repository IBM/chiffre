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
package rocketchip

import chisel3._
import rocket._
import cde._
import leChiffre._

class WithLeChiffre extends Config (
  (pname,site,here) => pname match {
    case BuildChiffre => ChiffreParameters
    case BuildRoCC => Seq(
      RoccParameters(
        opcodes = OpcodeSet.custom2,
        generator = (p: Parameters) =>  Module(new LeChiffre()(p)))
    )
    case RoccMaxTaggedMemXacts => 1
    case _ => throw new CDEMatchError
  })

class LeChiffreConfig extends Config(new WithLeChiffre ++ new DefaultConfig)
