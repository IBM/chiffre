// See LICENSE.IBM for license details.

package leChiffre

import firrtl._
import firrtl.ir._
import firrtl.annotations._
import firrtl.Mappers._

import scala.collection._

case class ModuleInstance(name: String, module: String)

case class ModulePath(name: String, signal: ChiffreSignal, path: Seq[ModuleInstance])
case class PathWire(baseName: String, width: Int, var uniqueName: Option[String] = None)

case class ChiffreSignal(tpe: String, dir: String, name: String,
  var width: Option[Int] = None)

object ChiffreSignal {
  def apply(raw: String): ChiffreSignal = {
    val args = raw.split(',')
    val (tpe, dir, name) = (args(0), args(1), args(2))
    val width = if (args.size > 3) Some(args(3).toInt) else None
    ChiffreSignal(tpe, dir, name, width)
  }
}

class ChiffreInjectionTransform extends firrtl.Transform {
  override def inputForm: CircuitForm = LowForm
  override def outputForm: CircuitForm = LowForm
  val info = FileInfo(FIRRTLStringLitHandler.unescape("BoostTransform"))

  override def execute(state: CircuitState): CircuitState = {
    getMyAnnotations(state) match {
      case Nil => state
      case a => val modules = a
          .collect {
            case ChiffreAnnotation(ModuleName(m, c), t) =>
              (m, ChiffreSignal(t)) }
          .distinct

        CircuitState(run(state.circuit, modules), state.form)
    }
  }

  private def broadcastInstrument(
    c: Circuit,
    port: Port,
    source: String,
    sinks: Seq[String]):
      Circuit = {

    object Color extends Enumeration { val Grey, White, Red, Green = Value }
    case class Data(var module: DefModule, var color: Color.Value,
      val subColors: mutable.ArrayBuffer[Color.Value] = mutable.ArrayBuffer())
    case class GreenPorts(module: DefModule, port: Port)
    val state = mutable.Map[String, Data](
      c.modules.map((m) => m.name -> Data(m, Color.Grey)): _*)
    val queue = mutable.MutableList[GreenPorts]()

    println(s"""[info] broadcastInstrument
[info]   - circuit: ${c.main}
[info]   - port:    ${port.name}
[info]   - source:  ${source}
[info]   - sinks:   ${sinks}""")

    def flip(p: Port): Port = {
      p
      // p copy (
      //   direction = p.direction match {
      //     case i: firrtl.ir.Input  => Output
      //     case o: firrtl.ir.Output => Input })
    }

    def addPort(m: DefModule, p: Port): DefModule = {
      if (m.ports contains p) { return m }
      m match { case m: Module => m copy (
        info = m.info ++ info,
        ports = m.ports :+ p) }
    }

    def traverse(modName: String, parentName: Option[String] = None): Unit = {
      // [todo]
      //   * Handle sink below sink
      //   * Handle sink below source

      def dfs(modName: String)(s: Statement): Statement = {
        s map dfs(modName) match {
          case WDefInstance(_, _, module, _) =>
            traverse(modName = module, parentName = Some(modName))
          case _ =>
        }
        s
      }

      println(s"[info] Traversing: ${modName}")
      // Exit if we've already visited this module. Otherwise,
      // preemptively set the color to white (unvisited).
      if (state(modName).color != Color.Grey) {
        if (parentName.nonEmpty) {
          state(parentName.get).subColors += state(modName).color }
        println(s"[info]   - exiting because 'already visited'")
        return
      }

      // This is a source:
      //   * Add a port if they do not exist
      //   * Color this module GREEN (on the source path)
      //   * Return as no sinks can exist beneath a source (currently)
      if (source == modName) {
        state(modName).color = Color.Green
        queue += GreenPorts(state(modName).module, port)
        if (parentName.nonEmpty) {
          state(parentName.get).subColors += Color.Green }
        println(s"[info]   - exiting because 'is source'")
        return
      }

      // This is a sink:
      //   * Add a port if it does not exist
      //   * Color this module RED (on the sink path)
      if (sinks contains modName) {
        state(modName) = Data(addPort(state(modName).module, flip(port)), Color.Red)
        if (parentName.nonEmpty) {
          state(parentName.get).subColors += Color.Red }
        println(s"[info]   - exiting because 'is sink'")
        return
      }

      // Recurse to compute the colors of all submodules
      state(modName).module map dfs(modName)

      // Determine the coloring of this module base on submodule colors
      state(modName).color = {
        val subColors = state(modName).subColors
        if ((subColors contains Color.Green) & (subColors contains Color.Red)) {
          // Clear queue by connecting everything in the queue
          queue.clear
          queue += GreenPorts(state(modName).module, port)
          Color.Green
        }
        else if ((subColors contains Color.Green) & !(subColors contains Color.Red)) {
          queue += GreenPorts(state(modName).module, port)
          Color.Green
        }
        else if (subColors contains Color.Red) {
          addPort(state(modName).module, port)
          Color.Red
        }
        else {
          Color.White
        }}
      if (parentName.nonEmpty) {
        state(parentName.get).subColors += Color.Green }
    }

    traverse(c.main)

    // [todo] copy over all the modules into the new circuit
    c
  }

  private def run(c: Circuit, modules: Seq[(String, ChiffreSignal)]): Circuit = {
    print(s"[info] in 'run' with ${modules}\n")

    case class ChiffreSignal(tpe: String, dir: String, name: String,
      var width: Option[Int] = None)

    val broadcastSources: Seq[(Port, String)] = modules
      .filter{ case (_, s) => s.tpe == "broadcast" && s.dir == "source" }
      .map{ case(modName, port) =>
        (Port(
          info = info,
          name = port.name,
          direction = Output,
          tpe = UIntType(IntWidth(port.width.get))
        ),
          modName) }
    println(s"[info] broadcastSources: ${broadcastSources}")

    val broadcastSinks: Map[String, Seq[String]] = modules
      .filter{ case (_, s) => s.tpe == "broadcast" && s.dir == "sink" }
      .map{ case(modName, port) => (port.name, modName) }
      .groupBy(_._1)
      .map{ case(k, v) => (k, v.map(_._2)) }
    println(s"[info] broadcastSinks: ${broadcastSinks}")

    broadcastSources
      .foldLeft(c){ case (circuit, (port, modName)) =>
        broadcastInstrument(circuit, port, modName, broadcastSinks(port.name)) }
  }
}
