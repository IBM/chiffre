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
import firrtl.ir._
import firrtl.passes._
import firrtl.passes.wiring.{SinkAnnotation, SourceAnnotation}
import firrtl.annotations.{ComponentName, ModuleName, CircuitName,
  SingleTargetAnnotation, Annotation}
import firrtl.annotations.AnnotationUtils._
import scala.collection.mutable
import java.io.FileWriter
import chiffre.scan.{ScanChain, InjectorInfo, FaultyComponent, JsonProtocol}

case class ScanChainException(msg: String) extends PassException(msg)

case class ScanChainInfo(
  masterScan: ComponentName,
  /* Everything here is keyed by the injector component */
  slaveIn: Map[ComponentName, ComponentName] = Map.empty,
  slaveOut: Map[ComponentName, ComponentName] = Map.empty,
  injectors: Map[ComponentName, ModuleName] = Map.empty,
  /* This is keyed by the injector module name */
  description: Map[ModuleName, InjectorInfo] = Map.empty) {
  // scalastyle:off line.size.limit
  def serialize(tab: String): String =
    s"""|${tab}master:
        |${tab}  scan: ${masterScan}
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

sealed trait ScanAnnos

case class ScanChainAnnotation(
  target: ComponentName,
  ctrl: String,
  dir: String,
  id: String,
  key: Option[ComponentName]) extends SingleTargetAnnotation[ComponentName]
    with ScanAnnos {
  def duplicate(x: ComponentName): ScanChainAnnotation = this.copy(target = x)
}

case class ScanChainInjectorAnnotation(
  target: ComponentName,
  id: String,
  instanceName: String,
  moduleName: String) extends SingleTargetAnnotation[ComponentName]
    with ScanAnnos {
  def duplicate(x: ComponentName): ScanChainInjectorAnnotation =
    this.copy(target = x)
}

case class ScanChainDescriptionAnnotation(
  target: ModuleName,
  id: String,
  d: InjectorInfo) extends SingleTargetAnnotation[ModuleName]
    with ScanAnnos {
  def duplicate(x: ModuleName): ScanChainDescriptionAnnotation =
    this.copy(target = x)
}

class ScanChainTransform extends Transform {
  def inputForm: CircuitForm = MidForm
  def outputForm: CircuitForm = HighForm
  def transforms: Seq[Transform] = Seq(
    new firrtl.passes.wiring.WiringTransform
  )

  // scalastyle:off cyclomatic.complexity
  def analyze(circuit: Circuit, annos: Seq[Annotation]):
      Map[String, ScanChainInfo] = {
    val s = mutable.HashMap[String, ScanChainInfo]()
    annos.foreach {
      case ScanChainAnnotation(comp, ctrl, dir, id, key) => (ctrl, dir) match {
        case ("master", "scan") => s(id) = ScanChainInfo(masterScan = comp)
        case _ =>
      }
      case _ =>
    }
    def exceptionIfUnknownId(id: String): Unit = if (!s.contains(id)) {
      throw new ScanChainException(s"No known scan chain master '$id' (did you misspell it?)") }
    annos.foreach {
      case ScanChainAnnotation(comp, ctrl, dir, id, key) =>
        exceptionIfUnknownId(id)
        s(id) =
          (ctrl, dir) match {
            case ("slave", "in")  => s(id).copy(slaveIn  = s(id).slaveIn  ++ Map(key.get -> comp))
            case ("slave", "out") => s(id).copy(slaveOut = s(id).slaveOut ++ Map(key.get -> comp))
            case _                => s(id)
          }
      case ScanChainInjectorAnnotation(comp, id, inst, mod) =>
        exceptionIfUnknownId(id)
        s(id) = s(id).copy(injectors = s(id).injectors ++ Map(comp -> ModuleName(mod, comp.module.circuit)))
      case _ =>
    }
    annos.foreach {
      case ScanChainDescriptionAnnotation(mod, id, d) =>
        exceptionIfUnknownId(id)
        s(id) = s(id).copy(description = s(id).description ++ Map(ModuleName(mod.name, CircuitName(circuit.main)) -> d))
      case _ =>
    }
    s.toMap
  } // scalastyle:on cyclomatic.complexity

  def execute(state: CircuitState): CircuitState = {
    val myAnnos = state.annotations.collect { case a: ScanAnnos => a }
    myAnnos match {
      case Nil => state
      case p =>
        val s = analyze(state.circuit, p)

        s.foreach{ case (k, v) => logger.info(
                    s"""|[info] scan chain:
                        |[info]   name: ${k}
                        |${v.serialize("[info]   ")}""".stripMargin) }

        // [todo] Order the scan chain based on distance

        // [todo] Set the emitted directory and file name
        val sc = s.flatMap{ case(k, v) => v.toScanChain(k) }

        val jsonFile = new FileWriter("scan-chain.json")
        jsonFile.write(JsonProtocol.serialize(sc))
        jsonFile.close()

        val ax = s.foldLeft(Seq[Annotation]()){ case (a, (name, v)) =>
          val masterIn = v.masterScan.copy(name=v.masterScan.name + ".in")
          val masterOut = v.masterScan.copy(name=v.masterScan.name + ".out")
          val masterClk = v.masterScan.copy(name=v.masterScan.name + ".clk")
          val masterEn = v.masterScan.copy(name=v.masterScan.name + ".en")

          val masterAnnotations = Seq(
            SourceAnnotation(masterClk, "scan_clk"),
            SourceAnnotation(masterEn, "scan_en"))

          // [todo] This is not deterministic
          val chain: Seq[ComponentName] = masterOut +: v.injectors
            .flatMap{ case (k, _) => Seq(v.slaveIn(k), v.slaveOut(k)) }
            .toSeq :+ masterIn

          val annotations = chain
            .grouped(2).zipWithIndex
            .flatMap{ case (Seq(l, r), i) =>
              Seq(SourceAnnotation(l, s"scan_${name}_$i"),
                  SinkAnnotation(r, s"scan_${name}_$i")) }

          a ++ masterAnnotations ++ annotations
        }

        val sx = state.copy(annotations = AnnotationSeq(state.annotations.toSeq ++ ax))

        transforms.foldLeft(sx){ (s, x) => x.runTransform(s) }
          .copy(annotations = (state.annotations.toSet -- myAnnos.toSet).toSeq)
    }
  }
}
