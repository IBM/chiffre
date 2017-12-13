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

class FaultInstrumentation(
  compMap: Map[String, Seq[(ComponentName, String, String)]])
    extends Transform {
  def inputForm = MidForm
  def outputForm = MidForm
  def execute(state: CircuitState): CircuitState = {
    val modifications = analyze(state.circuit)

    val (mxx, ax) = modifications
      .foldLeft((Seq[DefModule](), Seq[Annotation]())){
        case ((m, a), (_, Modifications(_,_,mm,aa,_))) => (m ++ mm, a ++ aa) }

    // mxx.foreach{ m =>
    //   logger.info("[info] Added Module:")
    //   logger.info(s"[info] ----------------------------------------")
    //   logger.info(m.serialize)
    //   logger.info(s"[info] ----------------------------------------")
    // }

    val mx = state.circuit.modules map onModule(modifications)
    val cx = ToWorkingIR.run(state.circuit.copy(modules = mxx ++ mx))

    val inAnno = state.annotations.getOrElse(AnnotationMap(Seq.empty)).annotations
    state.copy(circuit = cx,
               annotations = Some(AnnotationMap(inAnno ++ ax)))
  }

  private def inlineCompile(name: String, width: Int, id: String,
                            ns: Option[Namespace] = None): CircuitState = {
    def genName(name: String, n: Option[Namespace]): String = n match {
      case Some(nn) => nn.newName(name)
      case _ => name
    }

    val args = Array[AnyRef](new java.lang.Integer(width), id)
    val gen = () => Class.forName(name)
      .getConstructors()(0)
      .newInstance(args:_*)
      .asInstanceOf[chisel3.Module]
    val options =
      new ExecutionOptionsManager("Fault Instrumentation Inline")
          with HasFirrtlOptions
          with chisel3.HasChiselExecutionOptions {
        chiselOptions = new chisel3.ChiselExecutionOptions(
          runFirrtlCompiler = false
        )
      }
    val (chirrtl, inlineAnnos) =  chisel3.Driver
      .execute(options, gen) match {
        case chisel3.ChiselExecutionSuccess(
          Some(chisel3.internal.firrtl.Circuit(_,_,annos)),ast,_) =>
          (Parser.parse(ast), annos)
        case chisel3.ChiselExecutionFailure(m) =>
          throw new FaultInstrumentationException(
            s"Chisel inline compilation failed with '$m'")
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
      annotations = Some(AnnotationMap(inlineAnnos)))
  }

  private def analyze(c: Circuit): Map[String, Modifications] = {
    val mods = new mutable.HashMap[String, Modifications]
      .withDefaultValue(Modifications())
    val cmods = new mutable.HashMap[String, CircuitState]()
    val circuitNamespace = Namespace(c)

    c.modules
      .filter((m: DefModule) => compMap.contains(m.name))
      .foreach {
        case m: Module =>
          val moduleNamespace = Namespace(m)
          var scanIn: Option[String] = None
          var scanOut: String = ""
          compMap(m.name) map { case (comp, id, injector)  =>
            val t = passes.wiring.WiringUtils.getType(c, m.name, comp.name)
            t match {
              case _: GroundType =>
              case _ => throw new FaultInstrumentationException(
                "[todo] Only able to currently instrument GroundType components")
            }
            val width = getWidth(t)
            val numBits = width match { case IntWidth(x) => x.toInt }
            val tx = UIntType(width)

            val (subcircuit, defms, annosx) = if (cmods.contains(injector)) {
              (cmods(injector).circuit, Seq.empty, Seq.empty)
            } else {
              cmods(injector) = inlineCompile(injector, numBits, id,
                                              Some(circuitNamespace))
              (cmods(injector).circuit,
               cmods(injector).circuit.modules,
               if (cmods(injector).annotations.isEmpty) { Seq.empty }
               else { cmods(injector).annotations.get.annotations   } )
            }
            val defi = moduleNamespace.newName(subcircuit.main)
            val rename = moduleNamespace.newName(s"${comp.name}_fault")

            val conn = if (scanIn.isEmpty) {
              scanIn = Some(s"$defi.io.scan.in")
              EmptyStmt
            } else {
              Connect(NoInfo,toExp(scanIn.get),
                      toExp(s"$defi.io.scan.out"))
            }

            val x = mods(m.name)
            mods(m.name) = x.copy(
              defines = Seq(
                DefInstance(NoInfo, defi, subcircuit.main)
              ) ++ x.defines ++ Seq(
                DefWire(NoInfo, rename, t)
              ),
              connects = x.connects ++ Seq(
                Connect(NoInfo, toExp(s"$defi.clock"),
                        toExp(s"clock")),
                Connect(NoInfo, toExp(s"$defi.reset"),
                        toExp(s"reset")),
                Connect(NoInfo, toExp(s"$defi.io.in"),
                        castRhs(tx, comp.expr)),
                Connect(NoInfo, toExp(rename),
                        castRhs(t, toExp(s"$defi.io.out"))),
                conn
              ),
              modules = x.modules ++ defms,
              annotations = x.annotations ++ annosx ++ Seq(
                Annotation(ComponentName(s"$defi.io.scan.en",
                                         ModuleName(m.name,
                                                    CircuitName(c.main))),
                           classOf[firrtl.passes.wiring.WiringTransform],
                           "sink scan_en"),
                Annotation(ComponentName(s"$defi.io.scan.clk",
                                         ModuleName(m.name,
                                                    CircuitName(c.main))),
                           classOf[firrtl.passes.wiring.WiringTransform],
                           "sink scan_clk"),
                Annotation(comp,
                           classOf[leChiffre.passes.ScanChainTransform],
                           s"injector:$id:$defi:${subcircuit.main}")),
              renames = x.renames ++ Map(comp.name -> rename)
            )
            scanOut = s"$defi.io.scan.out"
          }

          // [todo] How should the scan chain ID be used here? (It
          // shouldn't always be "main")
          val x = mods(m.name)
          mods(m.name) = x.copy(
            annotations = x.annotations ++ Seq(
              Annotation(ComponentName(scanIn.get,
                                       ModuleName(m.name,
                                                  CircuitName(c.main))),
                         classOf[leChiffre.passes.ScanChainTransform],
                         s"slave:in:main"),
              Annotation(ComponentName(scanOut,
                                       ModuleName(m.name,
                                                  CircuitName(c.main))),
                         classOf[leChiffre.passes.ScanChainTransform],
                         s"slave:out:main")
            )
          )
        case m: ExtModule =>
          throw new FaultInstrumentationException("Tried to instrument an ExtModule")
      }

    mods.map{ case (k, v) =>
      logger.info(s"[info] $k")
      logger.info(v.serialize("[info]   "))
      // logger.info(v.serializeInMemory("[info]   "))
    }

    mods.toMap
  }

  private def onModule(mods: Map[String, Modifications])(m: DefModule): DefModule = {
    mods.get(m.name) match {
      case None => m
      case Some(l) => m match {
        case m: Module =>
          val x = mods(m.name)
          val mx = m.copy(
            body = Block(
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
