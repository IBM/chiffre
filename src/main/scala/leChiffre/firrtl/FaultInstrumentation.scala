// See LICENSE for license details.
package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.annotations._

case class FaultInstrumentationException(msg: String) extends PassException(msg)

case class FaultInstrumentationInfo(orig: ComponentName, repl: ComponentName)

class FaultInstrumentation(faultMap: Map[String, Seq[FaultInstrumentationInfo]]) extends Pass {
  def run(c: Circuit): Circuit = {
    logger.info(s"  [info] run")
    faultMap.map{ case (k, v) => {
      logger.info(s"  [info] - module: ${k}")
      v.map(x => logger.info(s"""  [info]   - orig: ${x.orig.name}
  [info]     - repl: ${x.repl.name}"""))
    }}

    logger.info(s"  [info] Instrumenting modules:")
    val cx = c.copy(modules = c.modules map onModule(faultMap))
    ToWorkingIR.run(cx)
  }

  def onModule(faultMap: Map[String, Seq[FaultInstrumentationInfo]])(m: DefModule): DefModule = {
    faultMap.get(m.name) match {
      case None => m
      case Some(l) =>
        m match {
          case x: Module =>
            logger.info(s"  [info]   - module: ${m.name}")
            val stmtsx = EmptyStmt
            faultMap(x.name).foldLeft(x) { (old, f) =>
              val ns = Namespace(m)
              val stmtsx = Seq(
                EmptyStmt
              )
              old.copy(body = Block(stmtsx :+ (old.body mapStmt onStmt(f)))) }
          case x: ExtModule =>
            throw new FaultInstrumentationException("Tried to instrument an ExtModule")
          case _ => m
        }
    }
  }

  def onStmt(fault: FaultInstrumentationInfo)(s: Statement): Statement = {
    s mapStmt onStmt(fault) match {
      case x: Connect =>
        // logger.info(s"  [info]     - statement: ${x}")
        x
      case _ => s
    }
  }
}
