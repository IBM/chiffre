// See LICENSE for license details.
package leChiffre.passes

import firrtl._
import firrtl.ir._
import firrtl.passes._
import firrtl.annotations._
import firrtl.annotations.AnnotationUtils._
import scala.collection.mutable

case class FaultInstrumentationException(msg: String) extends PassException(msg)

case class FaultInstrumentationInfo(orig: ComponentName, conn: ComponentName,
  repl: ComponentName)

case class Modifications(
  defines: Seq[Statement] = Seq.empty,
  connects: Seq[Statement] = Seq.empty,
  renames: Map[String, String] = Map.empty) {

  override def toString: String = serialize("")

  def serialize(indent: String): String = s"""${indent}defines:${defines.map(a => s"$indent  - $a").foldLeft("")(_+"\n"+_)}
${indent}connects:${connects.map(a => s"$indent  - $a").foldLeft("")(_+"\n"+_)}
${indent}renames:${renames.map{case (a, b) => s"$indent  - $a: $b"}.foldLeft("")(_+"\n"+_)}"""
}

class FaultInstrumentation(faultMap: Map[String, Seq[FaultInstrumentationInfo]]) extends Pass {
  def run(c: Circuit): Circuit = {
    // val cx = c.copy(modules = c.modules map onModule(faultMap))
    val modifications = analyze(c)
    val mx = c.modules map onModule(modifications)
    val cx = c.copy(modules = mx)
    ToWorkingIR.run(cx)
  }

  private def analyze(c: Circuit): Map[String, Modifications] = {
    val mods = new mutable.HashMap[String, Modifications]
      .withDefaultValue(Modifications())

    c.modules
      .filter((m: DefModule) => faultMap.contains(m.name))
      .map(_ match {
        case m: Module =>
          val ns = Namespace(m)
          faultMap(m.name) map (f => {
            val rename = ns.newName(s"${f.orig.name}_fault")
            val x = mods(m.name)
            val t = passes.wiring.WiringUtils.getType(c, m.name, f.orig.name)
            mods(m.name) = x.copy(
              defines = x.defines :+ DefWire(NoInfo, rename, t),
              connects = x.connects ++ Seq(
                Connect(NoInfo,f.conn.expr.mapType(_=>t),f.orig.expr.mapType(_=>t)),
                Connect(NoInfo,toExp(rename).mapType(_=>t), f.repl.expr.mapType(_=>t))),
              renames = x.renames ++ Map(f.orig.name -> rename))
          })
        case m: ExtModule =>
          throw new FaultInstrumentationException("Tried to instrument an ExtModule")
      })

    mods.map{ case (k, v) => {
      logger.info(s"[info] $k")
      logger.info(v.serialize("[info]   "))
    }}

    mods.toMap
  }

  private def onModule(mods: Map[String, Modifications])(m: DefModule): DefModule = {
    mods.get(m.name) match {
      case None => m
      case Some(l) => m match {
        case m: Module =>
          val x = mods(m.name)
          val mx = m.copy(body = Block(
            (x.defines :+ (m.body mapStmt onStmt(x.renames))) ++ x.connects))
          // logger.info("[info] original:")
          // logger.info(s"[info] ----------------------------------------")
          // logger.info(m.serialize)
          // logger.info(s"[info] ----------------------------------------")
          // logger.info("[info] instrumented:")
          // logger.info(mx.serialize)
          // logger.info(s"[info] ----------------------------------------")
          mx
        case _ => m
      }
    }
  }

  private def onStmt(renames: Map[String, String])(s: Statement): Statement = {
    s mapStmt onStmt(renames) match {
      case Connect(i, l, e) => Connect(i, l, e mapExpr replace(renames))
      case PartialConnect(i, l, e) => PartialConnect(i, l, e mapExpr replace(renames))
      case s => s mapExpr replace(renames)
    }
  }

  private def replace(renames: Map[String, String])(e: Expression): Expression = {
    e match {
      case ex: WRef => ex.name match {
        case name if renames.contains(name) => ex.copy(name=renames(name))
        case _ => ex
      }
      case _ => e mapExpr replace(renames)
    }
  }
}
