// See LICENSE for license details.
package leChiffre.passes

import firrtl._
import firrtl.ir._
import firrtl.passes._
import firrtl.annotations._
import firrtl.annotations.AnnotationUtils._
import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

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

  def serialize(indent: String): String = s"""${indent}defines:${defines.map(a => s"$indent  - ${a.serialize}").foldLeft("")(_+"\n"+_)}
${indent}connects:${connects.map(a => s"$indent  - ${a.serialize}").foldLeft("")(_+"\n"+_)}
${indent}modules:${modules.map(a => s"$indent  - ${a.name}").foldLeft("")(_+"\n"+_)}
${indent}annotations:${annotations.map(a => s"$indent  - ${a.serialize}").foldLeft("")(_+"\n"+_)}
${indent}renames:${renames.map{case (a, b) => s"$indent  - $a: $b"}.foldLeft("")(_+"\n"+_)}"""

  def serializeInMemory(indent: String): String = s"""${indent}defines:${defines.map(a => s"$indent  - $a").foldLeft("")(_+"\n"+_)}
${indent}connects:${connects.map(a => s"$indent  - $a").foldLeft("")(_+"\n"+_)}
${indent}modules:${modules.map(a => s"$indent  - ${a.name}").foldLeft("")(_+"\n"+_)}
${indent}annotations:${annotations.map(a => s"$indent  - $a").foldLeft("")(_+"\n"+_)}
${indent}renames:${renames.map{case (a, b) => s"$indent  - $a: $b"}.foldLeft("")(_+"\n"+_)}"""
}

class FaultInstrumentation(compMap: Map[String, Seq[(ComponentName, String, Seq[String])]])
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

  private def emitModules(name: String, width: Int,
                          ns: Option[Namespace] = None): Circuit = {
    def genName(name: String, n: Option[Namespace]): String = n match {
      case Some(nn) => nn.newName(name)
      case _ => name
    }

    val gen = () => Class.forName(name)
      .getConstructors()(0)
      .newInstance(new java.lang.Integer(width))
      .asInstanceOf[chisel3.Module]
    val chirrtl = Parser.parse(chisel3.Driver emit gen)
    val midFirrtl = (new MiddleFirrtlCompiler)
      .compileAndEmit(CircuitState(chirrtl, ChirrtlForm))
      .circuit
      .mapModule(
        _ match {
          case m: Module    => m.copy(name = genName(m.name, ns))
          case m: ExtModule => m.copy(name = genName(m.name, ns))
        })
    midFirrtl
  }

  private def analyze(c: Circuit): Map[String, Modifications] = {
    val mods = new mutable.HashMap[String, Modifications]
      .withDefaultValue(Modifications())
    val cmods = new mutable.HashMap[String, Circuit]()
    val circuitNamespace = Namespace(c)

    c.modules
      .filter((m: DefModule) => compMap.contains(m.name))
      .foreach {
        case m: Module =>
          val moduleNamespace = Namespace(m)
          var scanIn: Option[String] = None
          var scanOut: String = ""
          compMap(m.name) map { case (comp, gen_s, gen_params)  =>
            val width: Int = gen_params.head.toInt

            val (subcircuit, defms: Seq[DefModule]) = if (cmods.contains(gen_s)) {
              (cmods(gen_s), Seq.empty)
            } else {
              cmods(gen_s) = emitModules(gen_s, width, Some(circuitNamespace))
              (cmods(gen_s), cmods(gen_s).modules)
            }
            val defi = moduleNamespace.newName(subcircuit.main)
            val rename = moduleNamespace.newName(s"${comp.name}_fault")

            val conn = if (scanIn.isEmpty) {
              scanIn = Some(s"$defi.io.scan.in")
              EmptyStmt
            } else {
              Connect(NoInfo,toExp(scanIn.get),
                      toExp(s"$defi.io.scan.out"))
              throw new FaultInstrumentationException(
                "[todo] Add types to multiple injectors within the same module")
            }

            val x = mods(m.name)
            val t = passes.wiring.WiringUtils.getType(c, m.name, comp.name)
            val tx = UIntType(getWidth(t))
            mods(m.name) = x.copy(
              defines = Seq(
                DefInstance(NoInfo, defi, subcircuit.main)
              ) ++ x.defines ++ Seq(
                DefWire(NoInfo, rename, t)
              ),
              connects = x.connects ++ Seq(
                Connect(NoInfo,toExp(s"$defi.io.in"),
                        castRhs(tx, comp.expr)),
                Connect(NoInfo,toExp(rename),
                        castRhs(t, toExp(s"$defi.io.out"))),
                conn
              ),
              modules = x.modules ++ defms,
              annotations = x.annotations ++ Seq(
                Annotation(ComponentName(s"$defi.io.scan.en",
                                         ModuleName(m.name,
                                                    CircuitName(c.main))),
                           classOf[firrtl.passes.wiring.WiringTransform],
                           "sink scan_en"),
                Annotation(ComponentName(s"$defi.io.scan.clk",
                                         ModuleName(m.name,
                                                    CircuitName(c.main))),
                           classOf[firrtl.passes.wiring.WiringTransform],
                           "sink scan_clk")),
              renames = x.renames ++ Map(comp.name -> rename)
            )
            scanOut = s"$defi.io.scan.out"
          }

          val x = mods(m.name)
          mods(m.name) = x.copy(
            annotations = x.annotations ++ Seq(
              Annotation(ComponentName(scanIn.get,
                                       ModuleName(m.name,
                                                  CircuitName(c.main))),
                         classOf[leChiffre.passes.ScanChainTransform],
                         "slave:in:main"),
              Annotation(ComponentName(scanOut,
                                       ModuleName(m.name,
                                                  CircuitName(c.main))),
                         classOf[leChiffre.passes.ScanChainTransform],
                         "slave:out:main")
            )
          )
        case m: ExtModule =>
          throw new FaultInstrumentationException("Tried to instrument an ExtModule")
      }

    mods.map{ case (k, v) =>
      logger.info(s"[info] $k")
      logger.info(v.serialize("[info]   "))
      logger.info(v.serializeInMemory("[info]   "))
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
