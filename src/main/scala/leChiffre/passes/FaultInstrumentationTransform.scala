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
package leChiffre.passes

import firrtl._
import firrtl.passes._
import firrtl.annotations._
import scala.collection.mutable

object FaultInjectionAnnotation {
  def apply(comp: ComponentName, id: String, injector: String): Annotation = {
    Annotation(comp, classOf[FaultInstrumentationTransform], s"$injector:$id:$injector")
  }

  val matcher = raw"injector:(.+?):(.+)".r
  def unapply(a: Annotation): Option[(ComponentName, String, String)] = a match {
    case Annotation(ComponentName(n, m), _, matcher(id, injector)) =>
      Some(ComponentName(n, m), id, injector)
    case _ => None
  }
}

class FaultInstrumentationTransform extends Transform {
  def inputForm = MidForm
  def outputForm = MidForm
  def transforms(compMap: Map[String, Seq[(ComponentName, String, String)]]):
      Seq[Transform] = Seq(
    new FaultInstrumentation(compMap),
    ToWorkingIR,
    Uniquify,
    ExpandWhens,
    InferTypes,
    ResolveKinds,
    ResolveGenders,
    new ScanChainTransform
  )
  def execute(state: CircuitState): CircuitState = getMyAnnotations(state) match {
    case Nil => state
    case p =>
      val orig = mutable.HashMap[String, Seq[(Int, ComponentName)]]()
      val conn = mutable.HashMap[String, Map[Int, ComponentName]]()
      val repl = mutable.HashMap[String, Map[Int, ComponentName]]()
      val comp = mutable.HashMap[String, Seq[(ComponentName, String, String)]]()
      p.foreach {
        case FaultInjectionAnnotation(c, i, j) =>
          comp(c.module.name) = comp.getOrElse(c.module.name, Seq.empty) :+ (c, i, j)
        case _ => throw new
            FaultInstrumentationException("Unknown fault annotation type")}

      comp.foreach{ case (k, v) =>
        logger.info(s"[info] $k")
        v.foreach( a => logger.info(s"[info]   - ${a._1.name}: ${a._2}: ${a._3}") )}
      transforms(comp.toMap).foldLeft(state){ (s, x) => x.runTransform(s) }
  }
}
