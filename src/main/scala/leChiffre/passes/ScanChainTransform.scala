// See LICENSE for license details.
package leChiffre.passes

import firrtl._
import firrtl.passes._
import firrtl.annotations._
import scala.collection.mutable
import java.io.FileWriter
import leChiffre.scan._
import ScanChainProtocol._
import net.jcazevedo.moultingyaml._

case class ScanChainException(msg: String) extends PassException(msg)

case class ScanChainInfo(
  masterIn: ComponentName,
  masterOut: Option[ComponentName] = None,
  slaveIn: Seq[ComponentName] = Seq.empty,
  slaveOut: Map[String, ComponentName] = Map.empty,
  injectors: Map[ComponentName, ModuleName] = Map.empty,
  description: Map[ModuleName, Seq[ScanField]] = Map.empty) {
  def serialize(tab: String): String =
    s"""|$tab master:
        |$tab   in: ${masterIn.name}
        |$tab   out: ${masterOut.getOrElse("none")}
        |$tab slaves:${slaveIn.map( x=> s"$tab  ${x.module.name}\n$tab    ${x.name}: ${slaveOut(x.module.name).name}").mkString("\n")}
        |$tab injectors:${injectors.map{case (k,v)=>s"$tab  ${k.module.name + "." + k.name}: ${v.name}"}.mkString("\n")}
        |$tab description:${description.map{case(k,v)=>s"$tab  ${k.name}: $v"}.mkString("\n")}""".stripMargin

  def toScanChain(name: String): ScanChain = {
    val components: Seq[Component] = slaveIn.flatMap{ s =>
      injectors
        .filter{ case (ComponentName(_, m), _) => m == s.module }
        .map{ case (f, mm) =>
          val id = s"${f.module.circuit.name}.${f.module.name}.${f.name}"
          Component(id, description(mm))
        }
    }
    Map(name -> components)
  }
}

object ScanChainAnnotation {
  def apply(comp: ComponentName, ctrl: String, dir: String, id: String): Annotation =
    Annotation(comp, classOf[ScanChainTransform], s"$ctrl:$dir:$id")
  val matcher = raw"(master|slave):(\w+):(\w+)".r
  def unapply(a: Annotation): Option[(ComponentName, String, String, String)] = a match {
    case Annotation(ComponentName(n, m), _, matcher(ctrl, dir, id)) =>
      Some(ComponentName(n, m), ctrl, dir, id)
    case _ => None
  }
}

object ScanChainInjector {
  def apply(comp: ComponentName, instanceName: String,
            moduleName: String): Annotation =
    Annotation(comp, classOf[ScanChainTransform],
               s"injector:$instanceName:$moduleName")
  val matcher = raw"injector:(.+):(.+):(.+)".r
  def unapply(a: Annotation): Option[(ComponentName, String, String, String)] = a match {
    case Annotation(ComponentName(n, m), _, matcher(id, instanceName, moduleName)) =>
      Some((ComponentName(n, m), id, instanceName, moduleName))
    case _ => None
  }
}

object ScanChainDescription {
  def apply(mod: ModuleName, id: String, d: Seq[ScanField]): Annotation =
    Annotation(mod, classOf[ScanChainTransform],
               s"description:$id:" +
                 d.map(_.toYaml.prettyPrint).mkString )

  val matcher = raw"(?s)description:(.+?):(.+)".r
  def unapply(a: Annotation):
      Option[(ModuleName, String, Seq[ScanField])] = a match {
    case Annotation(ModuleName(m, c), _, matcher(id, raw)) =>
      Some((ModuleName(m, c), id, raw.parseYaml.convertTo[Seq[ScanField]]))
    case _ => None
  }
}

class ScanChainTransform extends Transform {
  def inputForm = MidForm
  def outputForm = MidForm
  def transforms = Seq(
    new firrtl.passes.wiring.WiringTransform
  )
  def execute(state: CircuitState): CircuitState = getMyAnnotations(state) match {
    case Nil => state
    case p =>
      val s = mutable.HashMap[String, ScanChainInfo]()
      p.foreach {
        case ScanChainAnnotation(comp, ctrl, dir, id) => (ctrl, dir) match {
          case ("master", "in") => s(id) = ScanChainInfo(masterIn = comp)
          case _ =>
        }
        case _ =>
      }
      p.foreach {
        case ScanChainAnnotation(comp, ctrl, dir, id) => s(id) = (ctrl, dir) match {
          case ("master", "out") => s(id).copy(masterOut = Some(comp))
          case ("slave", "in") => s(id).copy(slaveIn = s(id).slaveIn :+ comp)
          case ("slave", "out") => s(id).copy(slaveOut = s(id).slaveOut ++ Map(comp.module.name -> comp))
          case _ => s(id)
        }
        case ScanChainInjector(comp, id, inst, mod) => s(id) = s(id)
            .copy(injectors = s(id).injectors ++
                    Map(comp -> ModuleName(mod, comp.module.circuit)))
        case _ =>
      }
      p.foreach {
        case ScanChainDescription(mod, id, d) => {
          println(s"[ingo] description is: $d")
          s(id) = s(id)
            .copy(description = s(id).description ++
                    Map(ModuleName(mod.name, CircuitName(state.circuit.main)) -> d))
        }
        case _ =>
      }

      s.map{ case (k, v) =>logger.info(
              s"""|[info] scan chain:
                  |[info]   name: ${k}
                  |${v.serialize("[info]   ")}""".stripMargin) }

      // [todo] Order the scan chain to minimize distance. Roughly,
      // this should start from each source ("scan out") and connect
      // to it's closest sink ("scan in"). Distance is determined via
      // BFS. This should then be `O(n * m)` for `n` scan chain nodes
      // in a circuit with `m` instances.

      // [todo] Set the emitted directory and file name (via another
      // annotation?)
      val scanFile = "generated-src/scan-chain.yaml"
      val w = new FileWriter(scanFile)
      val sc = s.map{ case(k, v) => v.toScanChain(k) }
        .reduce(_ ++ _)

      import ScanChainProtocol._
      import net.jcazevedo.moultingyaml._

      println(sc)
      w.write(sc.toYaml.prettyPrint)

      w.close()

      val ax = s.foldLeft(Seq[Annotation]()){ case (a, (name, v)) =>
        val chain = (v.masterOut.get +:
                       v.slaveIn.flatMap(l => Seq(l, v.slaveOut(l.module.name))) :+
                       v.masterIn)

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
        annotations = Some(AnnotationMap(state.annotations.get.annotations ++ ax)))

      transforms.foldLeft(sx){ (s, x) => x.runTransform(s) }
  }
}
