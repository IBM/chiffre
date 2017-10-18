// See LICENSE for license details.
package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.annotations._

case class FaultInstrumentationException(msg: String) extends PassException(msg)

sealed trait FaultInfo
final case class FaultLfsr(width: Int) extends FaultInfo

case class FaultInstrumentationInfo(comp: ComponentName, fault: FaultInfo)

class FaultInstrumentation(faults: Seq[FaultInstrumentationInfo]) extends Pass {
  def run(c: Circuit): Circuit = {
    faults.map( f =>
      println(s"[info] Fault: module: ${f.comp.module.name}, signal: ${f.comp.name}"))
    ToWorkingIR.run(c)
  }

  def instrument(c: Circuit, info: FaultInstrumentationInfo): Circuit = {
    c
  }

  def onModule(m: DefModule): DefModule = {
    m
  }
}
