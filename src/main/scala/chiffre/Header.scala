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

import chisel3.util.isPow2
import freechips.rocketchip.config.{Field, Parameters}

case object BuildChiffre extends Field[ChiffreParameters]

// scalastyle:off magic.number
case class ChiffreParameters (
  checksumWidth: Int = 32,
  cycleWidth: Int = 32
)

trait LeChiffreH {
  implicit val p: Parameters

  val f_ECHO  = 0
  val f_CYCLE = 1
  val f_ENABLE = 2

  val cycleWidth = p(BuildChiffre).cycleWidth
  val checksumWidth = p(BuildChiffre).checksumWidth

  require(isPow2(checksumWidth),
          "checksumWidth BuildChiffre parameter must be a power of 2")
}
// scalastyle:on magic.number
