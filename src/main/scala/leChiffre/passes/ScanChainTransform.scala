// See LICENSE for license details.
package leChiffre.passes

import firrtl._
import firrtl.passes._
import firrtl.annotations._
import scala.collection.mutable

object ScanChainAnnotation {
  def apply(comp: ComponentName, ctrl: String, dir: String, id: String): Annotation =
    Annotation(comp, classOf[ScanChainTransform], s"$ctrl:$dir:$id")
  val matcher = raw"(\w+):(\w+):(\w+)".r
  def unapply(a: Annotation): Option[(ComponentName, String, String, String)] = a match {
    case Annotation(ComponentName(n, m), _, matcher(ctrl, dir, id)) =>
      Some(ComponentName(n, m), ctrl, dir, id)
    case _ => None
  }
}

case class ScanChain(
  masterIn: ComponentName,
  masterOut: Option[ComponentName] = None,
  slaveIn: Seq[ComponentName] = Seq.empty,
  slaveOut: Map[String, ComponentName] = Map.empty) {
  def serialize(tab: String): String = s"""$tab master:
$tab   in: ${masterIn.name}
$tab   out: ${masterOut.getOrElse("none")}
$tab slave:${slaveIn.map(x=>s"$tab  - ${x.module.name}.${x.name}: ${slaveOut(x.module.name).name}").foldLeft("")(_+"\n"+_)}"""
}

class ScanChainTransform extends Transform {
  def inputForm = HighForm
  def outputForm = HighForm
  def execute(state: CircuitState): CircuitState = getMyAnnotations(state) match {
    case Nil => state
    case p =>
      val s = mutable.HashMap[String, ScanChain]()
      p.foreach {
        case ScanChainAnnotation(comp, ctrl, dir, id) => (ctrl, dir) match {
          case ("master", "in") => s(id) = ScanChain(masterIn = comp)
          case _ =>
        }}
      p.foreach {
        case ScanChainAnnotation(comp, ctrl, dir, id) => s(id) = (ctrl, dir) match {
          case ("master", "out") => s(id).copy(masterOut = Some(comp))
          case ("slave", "in") => s(id).copy(slaveIn = s(id).slaveIn :+ comp)
          case ("slave", "out") => s(id).copy(slaveOut = s(id).slaveOut ++ Map(comp.module.name -> comp))
          case _ => s(id)
        }}

      s.map{ case (k, v) => logger.info(s"""[info] scan chain:
[info]   name: ${k}
${v.serialize("[info]   ")}""") }

      throw new FaultInstrumentationException("halt")
      // [todo] add WiringTransform annotations
      state
  }
}
