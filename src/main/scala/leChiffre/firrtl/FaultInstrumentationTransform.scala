// See LICENSE.IBM for license details.
package firrtl.passes

import firrtl._
import firrtl.passes._
import firrtl.annotations._
import scala.collection.mutable

object OriginalAnnotation {
  def apply(comp: ComponentName, id: Int): Annotation =
    Annotation(comp, classOf[FaultInstrumentationTransform], s"originalId:$id")
  val matcher = raw"originalId:(\d+)".r
  def unapply(a: Annotation): Option[(ComponentName, Int)] = a match {
    case Annotation(ComponentName(n, m), _, matcher(id)) =>
      Some(ComponentName(n, m), id.toInt)
    case _ => None
  }
}

object ConnectionAnnotation {
  def apply(comp: ComponentName, id: Int): Annotation =
    Annotation(comp, classOf[FaultInstrumentationTransform], s"connectionId:$id")
  val matcher = raw"connectionId:(\d+)".r
  def unapply(a: Annotation): Option[(ComponentName, Int)] = a match {
    case Annotation(ComponentName(n, m), _, matcher(id)) =>
      Some(ComponentName(n, m), id.toInt)
    case _ => None
  }
}

object ReplacementAnnotation {
  def apply(comp: ComponentName, id: Int): Annotation =
    Annotation(comp, classOf[FaultInstrumentationTransform], s"replacementId:$id")
  val matcher = raw"replacementId:(\d+)".r
  def unapply(a: Annotation): Option[(ComponentName, Int)] = a match {
    case Annotation(ComponentName(n, m), _, matcher(id)) =>
      Some(ComponentName(n, m), id.toInt)
    case _ => None
  }
}

class FaultInstrumentationTransform extends Transform {
  def inputForm = MidForm
  def outputForm = HighForm
  def execute(state: CircuitState): CircuitState = getMyAnnotations(state) match {
    case Nil => state
    case p =>
      val orig = mutable.HashMap[String, Seq[(Int, ComponentName)]]()
      val conn = mutable.HashMap[String, Map[Int, ComponentName]]()
      val repl = mutable.HashMap[String, Map[Int, ComponentName]]()
      p.foreach {
        case OriginalAnnotation(c, id) =>
          orig(c.module.name) = orig.getOrElse(c.module.name, Seq.empty) :+ (id, c)
        case ConnectionAnnotation(c, id) =>
          conn(c.module.name) = conn.getOrElse(c.module.name, Map.empty) ++ Map(id -> c)
        case ReplacementAnnotation(c, id) =>
          repl(c.module.name) = repl.getOrElse(c.module.name, Map.empty) ++ Map(id -> c)
        case _ => throw new
            FaultInstrumentationException("Unknown fault annotation type")}
      val faultMap = mutable.HashMap[String, Seq[FaultInstrumentationInfo]]()
      orig.map{ case (name, x) => x.map{ case (id, c) =>
        faultMap(name) = faultMap.getOrElse(name, Seq.empty) :+ (
          FaultInstrumentationInfo(c, conn(name)(id), repl(name)(id)) )}}

      new FaultInstrumentation(faultMap.toMap).runTransform(state)
  }
}
