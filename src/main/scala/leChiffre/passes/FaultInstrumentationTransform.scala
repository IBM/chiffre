// See LICENSE.IBM for license details.
package leChiffre.passes

import firrtl._
import firrtl.passes._
import firrtl.annotations._
import scala.collection.mutable

object FaultInjectionAnnotation {
  def apply(comp: ComponentName, injector: String, params: Seq[String]): Annotation = {
    val raw_params = params.foldLeft("")(_ + ":" + _)
    Annotation(comp, classOf[FaultInstrumentationTransform], s"$injector$raw_params")
  }

  private def extractParams(x: String): Seq[String] = {
    val matcher = raw"(.+?)(:(.*))?".r
    x match {
      case matcher(car, _, null) => Seq(car)
      case matcher(car, _, cdr) => Seq(car) ++ extractParams(cdr)
      case _ => Seq()
    }
  }

  val matcher = raw"injector:(.+)".r
  def unapply(a: Annotation): Option[(ComponentName, String, Seq[String])] = a match {
    case Annotation(ComponentName(n, m), _, matcher(raw_params)) => {
      val injector :: params = extractParams(raw_params)
      Some(ComponentName(n, m), injector, params)
    }
    case _ => None
  }
}

class FaultInstrumentationTransform extends Transform {
  def inputForm = MidForm
  def outputForm = MidForm
  def transforms(compMap: Map[String, Seq[(ComponentName, String, Seq[String])]]):
      Seq[Transform] = Seq(
    new FaultInstrumentation(compMap),
    new firrtl.passes.wiring.WiringTransform,
    new ScanChainTransform
  )
  def execute(state: CircuitState): CircuitState = getMyAnnotations(state) match {
    case Nil => state
    case p =>
      val orig = mutable.HashMap[String, Seq[(Int, ComponentName)]]()
      val conn = mutable.HashMap[String, Map[Int, ComponentName]]()
      val repl = mutable.HashMap[String, Map[Int, ComponentName]]()
      val comp = mutable.HashMap[String, Seq[(ComponentName, String, Seq[String])]]()
      p.foreach {
        case FaultInjectionAnnotation(c, i, p) =>
          comp(c.module.name) = comp.getOrElse(c.module.name, Seq.empty) :+ (c, i, p)
        case _ => throw new
            FaultInstrumentationException("Unknown fault annotation type")}

      comp.foreach{ case (k, v) =>
        logger.info(s"[info] $k")
        v.foreach( a => logger.info(s"[info]   - ${a._1.name}: ${a._2}: ${a._3}") )}
      transforms(comp.toMap).foldLeft(state){ (s, x) => x.runTransform(s) }
  }
}
