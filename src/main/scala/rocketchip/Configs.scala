// See LICENSE.ibm for license details.
package rocketchip

import chisel3._
import rocket._
import cde._
import leChiffre._

class WithLeChiffre extends Config (
  (pname,site,here) => pname match {
    case BuildRoCC => Seq(
      RoccParameters(
        opcodes = OpcodeSet.custom0,
        generator = (p: Parameters) =>  Module(new LeChiffre()(p)))
    )
    case RoccMaxTaggedMemXacts => 1
    case _ => throw new CDEMatchError
  })

class LeChiffreConfig extends Config(new WithLeChiffre ++ new DefaultConfig)
