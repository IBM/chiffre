// See LICENSE.IBM for license details.
package firrtl.passes

import firrtl._
import firrtl.passes._
import firrtl.annotations._
import scala.collection.mutable

object LfsrAnnotation {
  def apply(target: ComponentName, width: String): Annotation = Annotation(target,
    classOf[FaultInstrumentationTransform], s"lfsr$width")

  private val matcher = raw"lfsr(\d+)".r
  def unapply(a: Annotation): Option[(ComponentName, String)] = a match {
    case Annotation(ComponentName(n, m), _, matcher(width)) =>
      Some((ComponentName(n, m), width))
    case _ => None
  }
}

class FaultInstrumentationTransform extends Transform {
  def inputForm = MidForm
  def outputForm = HighForm
  def execute(state: CircuitState): CircuitState = getMyAnnotations(state) match {
    case Nil => state
    case p =>
      val faultMap = collection.mutable.Map[String, Seq[FaultInstrumentationInfo]]()
      p.foreach {
        case LfsrAnnotation(c, w) =>
          faultMap(c.module.name) = faultMap.getOrElse(c.module.name , Seq.empty) :+ FaultInstrumentationInfo(c, FaultLfsr(w.toInt))
        case _ => throw new
            FaultInstrumentationException("Unknown fault annotation type")}
      new FaultInstrumentation(faultMap.toMap).runTransform(state)
  }
}
