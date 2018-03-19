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
package chiffre.passes

import firrtl._
import firrtl.passes._
import firrtl.annotations._
import scala.collection.mutable

sealed trait FaultAnnos

case class FaultInjectionAnnotation
  (target: ComponentName, id: String, injector: String) extends
    SingleTargetAnnotation[ComponentName] with FaultAnnos {
  def duplicate(x: ComponentName): FaultInjectionAnnotation =
    this.copy(target = x)
}

class FaultInstrumentationTransform extends Transform {
  def inputForm: CircuitForm = MidForm
  def outputForm: CircuitForm = HighForm
  def transforms(compMap: Map[String, Seq[(ComponentName, String, String)]]):
      Seq[Transform] = Seq(
    new FaultInstrumentation(compMap),
    /* After FaultInstrumentation, the inline compilation needs to be cleaned
     * up. this massive list is what is helping with that. Assumedly, this
     * can be done better if we can directly get at the WIR from the
     * inline compilation and */
    ToWorkingIR,
    InferTypes,
    Uniquify,
    ExpandWhens,
    CheckInitialization,
    InferTypes,
    ResolveKinds,
    ResolveGenders,
    CheckTypes,
    new ScanChainTransform )
  def execute(state: CircuitState): CircuitState = {
    val myAnnos = state.annotations.collect { case a: FaultAnnos => a }
    myAnnos match {
      case Nil => state
      case p =>
        val orig = mutable.HashMap[String, Seq[(Int, ComponentName)]]()
        val conn = mutable.HashMap[String, Map[Int, ComponentName]]()
        val repl = mutable.HashMap[String, Map[Int, ComponentName]]()
        val comp = mutable.HashMap[String, Seq[(ComponentName, String, String)]]()
        p.foreach {
          case FaultInjectionAnnotation(c, i, j) => comp(c.module.name) =
            comp.getOrElse(c.module.name, Seq.empty) :+ (c, i, j) }

        comp.foreach{ case (k, v) =>
          logger.info(s"[info] $k")
          v.foreach( a =>
            logger.info(s"[info]   - ${a._1.name}: ${a._2}: ${a._3}") )}

        transforms(comp.toMap).foldLeft(state)((old, x) => x.runTransform(old))
          .copy(annotations = (state.annotations.toSet -- myAnnos.toSet).toSeq)
    }
  }
}
