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

import chiffre.inject.Injector
import chiffre.util.removeZeroWidth
import chisel3.experimental.{annotate, ChiselAnnotation}
import chisel3.stage.{ChiselGeneratorAnnotation, CircuitSerializationAnnotation}
import firrtl._
import firrtl.ir._
import firrtl.passes.{PassException, ToWorkingIR}
import firrtl.passes.wiring.SinkAnnotation
import firrtl.stage.FirrtlCircuitAnnotation
import firrtl.annotations.{Annotation, ComponentName, ModuleName, CircuitName}
import firrtl.annotations.AnnotationUtils._
import scala.collection.mutable

case class FaultInstrumentationException(msg: String) extends PassException(msg)

case class FaultInstrumentationInfo(orig: ComponentName, conn: ComponentName, repl: ComponentName)

case class Modifications(
  defines: Seq[Statement] = Seq.empty,
  connects: Seq[Statement] = Seq.empty,
  modules: Seq[DefModule] = Seq.empty,
  annotations: Seq[Annotation] = Seq.empty,
  renames: Map[String, String] = Map.empty) {

  override def toString: String = serialize("")

  def serialize(indent: String): String =
    s"""|${indent}defines:
        |${defines.map(a => s"$indent  - ${a.serialize}").mkString("\n")}
        |${indent}connects:
        |${connects.map(a => s"$indent  - ${a.serialize}").mkString("\n")}
        |${indent}modules:
        |${modules.map(a => s"$indent  - ${a.name}").mkString("\n")}
        |${indent}annotations:
        |${annotations.map(a => s"$indent  - ${a.serialize}").mkString("\n")}
        |${indent}renames:
        |${renames.map{case (a, b) => s"$indent  - $a: $b"}.mkString("\n")}"""
      .stripMargin

  def serializeInMemory(indent: String): String =
    s"""|${indent}defines:
        |${defines.map(a => s"$indent  - $a").mkString("\n")}
        |${indent}connects:
        |${connects.map(a => s"$indent  - $a").mkString("\n")}
        |${indent}modules:
        |${modules.map(a => s"$indent  - ${a.name}").mkString("\n")}
        |${indent}annotations:
        |${annotations.map(a => s"$indent  - $a").mkString("\n")}
        |${indent}renames:
        |${renames.map{case (a, b) => s"$indent  - $a: $b"}.mkString("\n")}"""
      .stripMargin
}

class FaultInstrumentation(compMap: Map[String, Seq[(ComponentName, String, Class[_ <: Injector])]]) extends Transform {
  def inputForm: CircuitForm = MidForm
  def outputForm: CircuitForm = MidForm
  def execute(state: CircuitState): CircuitState = {
    val modifications = analyze(state.circuit)

    val (mxx, ax) = modifications
      .foldLeft((Seq[DefModule](), Seq[Annotation]())){
        case ((m, a), (_, Modifications(_,_,mm,aa,_))) => (m ++ mm, a ++ aa) }

    val mx = state.circuit.modules map onModule(modifications)
    val cx = ToWorkingIR.run(state.circuit.copy(modules = mxx ++ mx))

    val inAnno: Seq[Annotation] = state.annotations.toSeq
    state.copy(circuit = cx,
               annotations = AnnotationSeq(inAnno ++ ax))
  }

  private def inlineCompile(gen: () => chisel3.Module, ns: Option[Namespace] = None): CircuitState = {
    def genName(name: String, n: Option[Namespace] = None): String = n.map(_.newName(name)).getOrElse(name)

    val (chirrtl: firrtl.ir.Circuit, inlineAnnos: AnnotationSeq) = {
      val outputAnnotations = (new chisel3.stage.ChiselStage).execute(
        args = Array("--no-run-firrtl"),
        annotations = Seq(ChiselGeneratorAnnotation(gen))
      )
      val (circuitAnnos, otherAnnos) = outputAnnotations.partition {
        case _: FirrtlCircuitAnnotation => true
        case _ => false
      }
      require(circuitAnnos.size == 1)
      (circuitAnnos.head.asInstanceOf[FirrtlCircuitAnnotation].circuit, AnnotationSeq(otherAnnos))
    }
    val midFirrtl = (new MiddleFirrtlCompiler)
      .compileAndEmit(CircuitState(chirrtl, ChirrtlForm))
      .circuit
      .mapModule(
        _ match {
          case m: Module    => m.copy(name = genName(m.name, ns))
          case m: ExtModule => m.copy(name = genName(m.name, ns))
        })
    CircuitState(
      circuit = midFirrtl,
      form = MidForm,
      annotations = AnnotationSeq(inlineAnnos))
  }

  private def analyze(c: Circuit): Map[String, Modifications] = {
    val mods = new mutable.HashMap[String, Modifications].withDefaultValue(Modifications())
    val cmods = new mutable.HashMap[ComponentName, CircuitState]()
    val circuitNamespace = Namespace(c)

    c.modules
      .filter((m: DefModule) => compMap.contains(m.name))
      .foreach {
        case m: Module =>
          val moduleNamespace = Namespace(m)
          var scanIn: Option[String] = None
          var scanOut: String = ""
          compMap(m.name) map { case (comp, id, gen)  =>
            val t = passes.wiring.WiringUtils.getType(c, m.name, comp.name)
            val width = bitWidth(t)
            if (width == 0)
              throw new FaultInstrumentationException("Cannot instrument zero-width signals")

            val args = Array[AnyRef](new java.lang.Integer(width.toInt), id)
            val dut = () => {
              val injector = gen.getConstructors()(0)
                .newInstance(args: _*)
                .asInstanceOf[Injector]
              annotate(new ChiselAnnotation {
                def toFirrtl: ScanChainDescriptionAnnotation = ScanChainDescriptionAnnotation(comp, id, injector.info)
              })
              injector
            }
            val (subcir, defms, annosx) = if (cmods.contains(comp)) {
              (cmods(comp).circuit, Seq.empty, Seq.empty)
            } else {
              try {
                cmods(comp) = inlineCompile(dut, Some(circuitNamespace))
              } catch {
                case e: java.lang.IllegalArgumentException => throw new FaultInstrumentationException(
                  s"Did not find '(Int, String)' constructor for injector '${gen.getName}' (Did you forget to specify it?)")
              }
              (cmods(comp).circuit, cmods(comp).circuit.modules,
               if (cmods(comp).annotations.isEmpty) { Seq.empty }
               else { cmods(comp).annotations.toSeq   } )
            }
            val injector = defms.last // the top-level module (the injector) should be the last module of the subcircuit
            val inst = DefInstance(NoInfo, moduleNamespace.newName(s"${comp.name}_injector"), injector.name)
            val rename = moduleNamespace.newName(s"${comp.name}_fault")

            val Seq(scanEn, scanClk, scanIn, scanOut) =
              Seq("en", "clk", "in", "out").map( s =>
                ComponentName(s"${inst.name}.io.scan.$s",
                              ModuleName(m.name, CircuitName(c.main))) )

            val faulty = DefWire(NoInfo, rename, t)
            val data = fromBits(WRef(faulty).mapType(removeZeroWidth.apply), toExp(s"${inst.name}.io.out")) match {
              case Block(stmts: Seq[Statement]) =>
                stmts :+ Connect(NoInfo, toExp(s"${inst.name}.io.in"), toBits(WRef(comp.name, t, RegKind, UnknownFlow).mapType(removeZeroWidth.apply)))
            }
            val x = mods(m.name)
            mods(m.name) = x.copy(
              defines = inst +: x.defines :+ faulty,
              connects = x.connects ++ Seq(
                Connect(NoInfo, toExp(s"${inst.name}.clock"), toExp(s"clock")),
                Connect(NoInfo, toExp(s"${inst.name}.reset"), toExp(s"reset")),
                IsInvalid(NoInfo, toExp(s"${inst.name}.io.scan.en")),
                IsInvalid(NoInfo, toExp(s"${inst.name}.io.scan.clk")),
                IsInvalid(NoInfo, toExp(s"${inst.name}.io.scan.in"))
              ) ++ data,
              modules = x.modules ++ defms,
              annotations = x.annotations ++ annosx ++ Seq(
                SinkAnnotation(scanEn, "scan_en"),
                SinkAnnotation(scanClk, "scan_clk"),
                ScanChainInjectorAnnotation(comp, id, injector.name),
                ScanChainAnnotation(scanIn, "slave", "in", id, Some(comp)),
                ScanChainAnnotation(scanOut, "slave", "out", id, Some(comp))
              ),
              renames = x.renames ++ Map(comp.name -> rename)
            )
          }
        case m: ExtModule =>
          throw new FaultInstrumentationException(
            "Tried to instrument an ExtModule")
      }

    mods.map{ case (k, v) =>
      logger.info(s"[info] $k")
      logger.info(v.serialize("[info]   "))
    }

    mods.toMap
  }

  private def onModule(mods: Map[String, Modifications])
                      (m: DefModule): DefModule = {
    mods.get(m.name) match {
      case None => m
      case Some(l) => m match {
        case m: Module =>
          val x = mods(m.name)
          val mx = m.copy(
            body = Block(
              (x.defines :+ (m.body mapStmt onStmt(x.renames))) ++ x.connects))
          mx
        case _ => m
      }
    }
  }

  private def onStmt(renames: Map[String, String])(s: Statement): Statement = {
    s mapStmt onStmt(renames) match {
      case Connect(i, l, e) =>
        Connect(i, l, replace(renames)(e))
      case PartialConnect(i, l, e) =>
        PartialConnect(i, l, replace(renames)(e))
      case s => s mapExpr replace(renames)
    }
  }

  private def replace(renames: Map[String, String])
                     (e: Expression): Expression = {
    e match {
      case ex: WRef => ex.name match {
        case name if renames.contains(name) => ex.copy(name=renames(name))
        case _ => ex
      }
      case ex: Reference => ex.name match {
        case name if renames.contains(name) => ex.copy(name=renames(name))
        case _ => ex
      }
      case _ => e mapExpr replace(renames)
    }
  }
}
