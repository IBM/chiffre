// See LICENSE for license details.
package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.annotations._

case class FaultInstrumentationException(msg: String) extends PassException(msg)

sealed trait FaultInfo
final case class FaultLfsr(width: Int) extends FaultInfo

case class FaultInstrumentationInfo(comp: ComponentName, fault: FaultInfo)

class FaultInstrumentation(faultMap: Map[String, Seq[FaultInstrumentationInfo]]) extends Pass {
  def run(c: Circuit): Circuit = {
    logger.info(s"  [info] run")
    faultMap.map{ case (k, v) => {
      logger.info(s"  [info] - module: ${k}")
      v.map(x => logger.info(s"  [info]   - comp: ${x.comp.name}"))
    }}
    ToWorkingIR.run(c)
  }

  def instrument(c: Circuit, info: FaultInstrumentationInfo): Circuit = {
    c
  }

  def onModule(faultMap: Map[String, Seq[FaultInstrumentationInfo]])(m: DefModule): DefModule = {
    faultMap.get(m.name) match {
      case None => m
      case Some(l) =>
        logger.info(s"  [info] Found module $m.name")
    }
    m
  }
}
