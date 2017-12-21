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
import firrtl.ir._
import firrtl.passes._
import firrtl.annotations._
import firrtl.annotations.AnnotationUtils._
import scala.collection.mutable
import java.io.FileWriter
import leChiffre.scan._
import ScanChainProtocol._
import net.jcazevedo.moultingyaml._

case class ScanChainException(msg: String) extends PassException(msg)

case class ScanChainInfo(
  masterIn: ComponentName,
  masterOut: Option[ComponentName] = None,
  /* Everything here is keyed by the injector component */
  slaveIn: Map[ComponentName, ComponentName] = Map.empty,
  slaveOut: Map[ComponentName, ComponentName] = Map.empty,
  injectors: Map[ComponentName, ModuleName] = Map.empty,
  /* This is keyed by the injector module name */
  description: Map[ModuleName, InjectorInfo] = Map.empty) {
  // scalastyle:off line.size.limit
  def serialize(tab: String): String =
    s"""|${tab}master:
        |${tab}  in: ${masterIn.name}
        |${tab}  out: ${masterOut.getOrElse("none")}
        |${tab}slaves:
        |${injectors.map{ case (k, v) => s"${tab}  ${v.name}, ${k.module.name}, ${slaveIn(k).module.name}.${slaveIn(k).name}, ${slaveOut(k).module.name}.${slaveOut(k).name}"}.mkString("\n")}
        |${tab}description:
        |${description.map{case(k,v)=>s"${tab}  ${k.name}: $v"}.mkString("\n")}"""
      .stripMargin
  // scalastyle:on line.size.limit

  def toScanChain(name: String): ScanChain = {
    val components: Seq[FaultyComponent] = injectors.map{ case(c, m) =>
      val id = s"${c.module.circuit.name}.${c.module.name}.${c.name}"
      FaultyComponent(id, description(m))
    }.toSeq
    Map(name -> components)
  }
}

object ScanChainAnnotation {
  def apply(comp: ComponentName, ctrl: String, dir: String, id: String,
            key: Option[ComponentName]): Annotation =
    Annotation(
      comp, classOf[ScanChainTransform],
      s"""$ctrl:$dir:$id:${if (key.isEmpty) "" else key.get.serialize}""")
  val re = raw"(master|slave):(.+):(.+):((.+?)\.(.+?)\.(.+?))?$$".r
  def unapply(a: Annotation): Option[(ComponentName, String, String, String,
                                      Option[ComponentName])] = a match {
    case Annotation(ComponentName(n, m), _, re(ctrl, dir, id, key, a,b,c)) =>
      val k =
        if (key == null) { // scalastyle:off
          None
        } else {
          ComponentName(c, ModuleName(b, CircuitName(a))) match {
            case x: ComponentName => Some(x)
            case _ => None
          }
        }
      Some((ComponentName(n, m), ctrl, dir, id, k))
    case _ => None
  }
}

object ScanChainInjector {
  def apply(comp: ComponentName, instanceName: String,
            moduleName: String): Annotation =
    Annotation(comp, classOf[ScanChainTransform],
               s"injector:$instanceName:$moduleName")
  val re = raw"injector:(.+):(.+):(.+)".r
  def unapply(a: Annotation):
      Option[(ComponentName, String, String, String)] = a match {
    case Annotation(ComponentName(n, m), _, re(id, instanceName, moduleName)) =>
      Some((ComponentName(n, m), id, instanceName, moduleName))
    case _ => None
  }
}

object ScanChainDescription {
  def apply(mod: ModuleName, id: String, d: InjectorInfo): Annotation =
    Annotation(mod, classOf[ScanChainTransform],
               s"description:$id:" + (d.toYaml.prettyPrint).mkString )

  val re = raw"(?s)description:(.+?):(.+)".r
  def unapply(a: Annotation):
      Option[(ModuleName, String, InjectorInfo)] = a match {
    case Annotation(ModuleName(m, c), _, re(id, raw)) =>
      Some((ModuleName(m, c), id, raw.parseYaml.convertTo[InjectorInfo]))
    case _ => None
  }
}

class ScanChainTransform extends Transform {
  def inputForm: CircuitForm = MidForm
  def outputForm: CircuitForm = MidForm
  def transforms: Seq[Transform] = Seq(
    new firrtl.passes.wiring.WiringTransform
  )

  def analyze(circuit: Circuit, annos: Seq[Annotation]):
      Map[String, ScanChainInfo] = {
    val s = mutable.HashMap[String, ScanChainInfo]()
    annos.foreach {
      case ScanChainAnnotation(comp, ctrl, dir, id, key) => (ctrl, dir) match {
        case ("master", "in") => s(id) = ScanChainInfo(masterIn = comp)
        case _ =>
      }
      case _ =>
    }
    annos.foreach {
      case ScanChainAnnotation(comp, ctrl, dir, id, key) => s(id) = (ctrl, dir) match {
        case ("master", "out") => s(id).copy(masterOut = Some(comp))
        case ("slave", "in") => s(id).copy(slaveIn = s(id).slaveIn ++
                                             Map(key.get -> comp))
        case ("slave", "out") => s(id).copy(slaveOut = s(id).slaveOut ++
                                              Map(key.get -> comp))
        case _ => s(id)
      }
      case ScanChainInjector(comp, id, inst, mod) => s(id) = s(id)
          .copy(injectors = s(id).injectors ++
                  Map(comp -> ModuleName(mod, comp.module.circuit)))
      case _ =>
    }
    annos.foreach {
      case ScanChainDescription(mod, id, d) => {
        s(id) = s(id)
          .copy(description = s(id).description ++
                  Map(ModuleName(mod.name, CircuitName(circuit.main)) -> d))
      }
      case _ =>
    }
    s.toMap
  }

  def execute(state: CircuitState): CircuitState = getMyAnnotations(state) match {
    case Nil => state
    case p =>
      val s = analyze(state.circuit, p)

      s.foreach{ case (k, v) => logger.info(
                  s"""|[info] scan chain:
                      |[info]   name: ${k}
                      |${v.serialize("[info]   ")}""".stripMargin) }

      // [todo] Order the scan chain based on distance

      // [todo] Set the emitted directory and file name
      val scanFile = "scan-chain.yaml"
      val w = new FileWriter(scanFile)
      val sc = s.map{ case(k, v) => v.toScanChain(k) }
        .reduce(_ ++ _)

      import ScanChainProtocol._
      import net.jcazevedo.moultingyaml._

      w.write(sc.toYaml.prettyPrint)

      w.close()

      val ax = s.foldLeft(Seq[Annotation]()){ case (a, (name, v)) =>
        // [todo] This is not deterministic
        val chain: Seq[ComponentName] =
          (v.masterOut.get +:
             v.injectors.flatMap{ case (k, _) =>
               Seq(v.slaveIn(k), v.slaveOut(k)) }.toSeq :+ v.masterIn  )

        val annotations = chain
          .grouped(2).zipWithIndex
          .flatMap{ case (Seq(l, r), i) =>
            Seq(Annotation(l,
                           classOf[firrtl.passes.wiring.WiringTransform],
                           s"source scan_${name}_$i"),
                Annotation(r,
                           classOf[firrtl.passes.wiring.WiringTransform],
                           s"sink scan_${name}_$i") )}
        a ++ annotations
      }

      val sx = state.copy(
        annotations = Some(
          AnnotationMap(state.annotations.get.annotations ++ ax)))

      transforms.foldLeft(sx){ (s, x) => x.runTransform(s) }
  }
}
