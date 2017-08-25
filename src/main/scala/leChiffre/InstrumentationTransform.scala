// See LICENSE.IBM for license details.

package leChiffre

import firrtl._
import firrtl.ir._
import firrtl.annotations._
import firrtl.Mappers._

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

  private def unimplemented(): Unit = { throw AnnotationException("unimplemented") }

  private def findPath(moduleMap: Map[String, DefModule],
    root: String, leaf: String): Seq[ModuleInstance] = {
    val path = collection.mutable.ArrayBuffer[ModuleInstance]()

    def dfs(c: ModuleInstance, leaf: String,
      m: Map[String, DefModule],
      p: collection.mutable.ArrayBuffer[ModuleInstance],
      indent: String): Unit = {

      def f(
        m: Map[String, DefModule],
        p: collection.mutable.ArrayBuffer[ModuleInstance],
        l: String,
        indent: String)(s: Statement): Statement = {

        s map f(m, p, l, indent) match {
          case WDefInstance(info, name, module, tpe) => {
            if (p.size == 0) { dfs(ModuleInstance(name, module), l, m, p, indent) } }
          case _ => }
        s
      }

      if (c.module == leaf) { p += c }
      else {
        m(c.module) map f(m, p, leaf, indent + "  ")
        if (p.size > 0) { p += c } }
    }

    dfs(ModuleInstance(root, root), leaf, moduleMap, path, "")

    if (path.size == 0) {
      throw AnnotationException(s"Found empty path from '${root}' -> '${leaf}' (likely due to deduplication)\n")
    }

    path.reverse
  }

  private def drill(signal: ChiffreSignal, source: Seq[ModuleInstance],
    map: Map[String, DefModule], visited: Map[String, Port]):
      (Map[String, DefModule], Option[Port], Map[String, Port]) = {
    println(s"[info]     - drill '${source.head}' for signal '${signal}'")

    val thisModule = map(source.head.module)
    lazy val n = Namespace(thisModule)

    val dir = if (signal.dir == "source") Output else Input
    val (mapx, stmtx, portx, newModule, visitedx) = if (source.tail.nonEmpty) {
      val (m, subPort, v) = drill(signal, source.tail, map, visited)
      val p = Port(info,
        n.newName(signal.name),
        dir,
        UIntType(IntWidth(signal.width.get)))
      if (visited.contains(thisModule.name)) {
        val s = if (dir == Output) {
          Connect(info,
            WRef(p.name, p.tpe, ExpKind, UNKNOWNGENDER),
            WSubField(
              WRef(source.tail(0).name, UnknownType, InstanceKind, UNKNOWNGENDER),
              subPort.get.name, p.tpe, UNKNOWNGENDER))
        } else {
          Connect(info,
            WSubField(
              WRef(source.tail(0).name, UnknownType, InstanceKind, UNKNOWNGENDER),
              subPort.get.name, p.tpe, UNKNOWNGENDER),
            WRef(p.name, p.tpe, ExpKind, UNKNOWNGENDER))
        }
        val nm = thisModule match {
          case m: Module => m copy (
            info = m.info ++ info,
            ports = m.ports :+ p,
            body = Block(m.body +: Seq(s))) }
        (m, s, Some(p), nm, v)
      } else {
        if (!subPort.isEmpty) {
          val s = Connect(info,
            WSubField(
              WRef(source.tail(0).name, UnknownType, InstanceKind, UNKNOWNGENDER),
              subPort.get.name, p.tpe, UNKNOWNGENDER),
            WRef(p.name, p.tpe, ExpKind, UNKNOWNGENDER))
          (m, s, Some(p), thisModule, v)
        } else {
          (m, EmptyStmt, None, thisModule, v)
        }
      }
    } else if (source.tail.isEmpty && dir == Input) {
      val p = Port(info,
        n.newName(signal.name),
        dir,
        UIntType(IntWidth(signal.width.get)))
      val nm = thisModule match {
        case m: Module => m copy (
          info = m.info ++ info,
          ports = m.ports :+ p) }
      (map, EmptyStmt, Some(p), nm, Map())
    } else {
      val p = thisModule.ports.filter(_.name == signal.name)
      if (p.isEmpty) {
        throw AnnotationException(s"Broadcast Source Module ${thisModule.name} does not contain port ${signal.name}") }
      (map, EmptyStmt, Some(p.head), thisModule, Map())
    }

    (mapx ++ Map(newModule.name -> newModule), portx,
      visited ++ Map(thisModule.name -> portx) ++ visitedx)
  }

  private def broadcastConnect(
    c: Circuit,
    source: ModulePath,
    sink: ModulePath,
    visited: Map[String, Port] = Map()):
      (Circuit, Map[String, Port]) = {
    val map = (c.modules map (m => m.name -> m)).toMap

    println(s"[info] broadcastConnect '${source.signal.name}': ${source.name} -> ${sink.name}")

    val (sourcePath, sinkPath) = {
      val lcaIdx = (source.path
        .zip(sink.path)
        .map { case (l, r) => l != r }
        .indexOf(true)) - 1
      (source.path.drop(lcaIdx), sink.path.drop(lcaIdx)) }
    println(s"[info]   - LCA:        ${sourcePath(0)}")
    println(s"[info]   - sourcePath: ${sourcePath}")
    println(s"[info]   - sinkPath:   ${sinkPath}")
    println(s"[info]   - visited (pre drill):   ${visited}")

    // Drill out the source path
    val (mx, _, vx) = drill(source.signal, sourcePath.tail, map, visited)

    // Drill out the sink path
    val (mxx, _, vxx) = drill(sink.signal, sinkPath.tail, mx, vx)

    // Connect the source path to the sink path
    // [todo]

    println(s"[info]   - visited (post drill):   ${vx}")

    (c copy(modules = (mxx values).toSeq), vxx)
  }

  private def broadcastInstrument(
    c: Circuit,
    port: Port,
    source: String,
    sinks: Seq[String]):
      Circuit = {

    object Color extends Enumeration { val Grey, White, Red, Green = Value }
    case class Data(module: DefModule, color: Color)
    case class GreenPorts(module: DefModule, port: Port)
    val state = collection.mutable.Map[String, Data](
      c.modules.map((m) => m.name -> Data(m, Color.Grey)): _*)
    val queue = collection.mutalbe.MutableList[GreenPorts]()

    def flip(p: Port): Port = {
      p copy (
        direction = p.direction match {
          case i: Input  => Output
          case o: Output => Input })
    }

    def addPorts(m: DefModule, p: Port): DefModule = {
      if (m.ports contains p) { return m }
      m copy (
        info = m.info ++ info,
        ports = m.ports :+ p)
    }

    def connect() = {
    }

    def traverse(modName: String): Color = {
      println(s"[info] Traversing: ${modname}")
      // Exit if we've already visited this module. Otherwise,
      // preemptively set the color to white (unvisited).
      if (state(modName).color != Color.Grey) { return state(modName).color }

      // This is a source:
      //   * Add a port if they do not exist
      //   * Color this module GREEN (on the source path)
      //   * Return as no sinks can exist beneath a source (currently)
      if (source == modName) {
        state(modName).color = Color.Green
        queue += GreenPorts(state(modName), port)
        return color.Green }

      // This is a sink:
      //   * Add a port if it does not exist
      //   * Color this module RED (on the sink path)
      if (sinks contains modName) {
        state(modName) = (addPort(state(modName).module, flip(port)), Color.Red) }

      // Recurse to compute the colors of all instances then take
      // action based on the instance colors
      //   * Green and not Red: This may be the least common ancestor
      //     (LCA). However, we can't be sure until the full DFS
      //     completes. This module is pushed onto the queue.
      //   * Green (and Red): Connect all reds and push this onto the
      //     queue.
      //   * Red: Connect all reds and add ports going upwards
      val colors = state(modName).module map ((s: Statement) => s match {
        case i: DefInstance => i.map(traverse(_.module)) })
      state(modName).color = colors map (c =>
        if (c contains Color.Green & c contains Color.Red) {
          // Clear queue by connecting everything in the queue
          queue.clear
          queue += GreenPorts(state(modName).module, port)
          Color.Green
        }
        else if (c contains Color.Green & !c contains Color.Red) {
          queue += GreenPorts(state(modName), port)
          Color.Green
        }
        else if (c contains Color.Red) {
          addPorts(state(modName).module, port)
          Color.Red
        }
        else {
          Color.White
        })

      state(modName).color
    }

    traverse(c.main.name)
  }

  private def run(c: Circuit, modules: Seq[(String, ChiffreSignal)]): Circuit = {
    print(s"[info] in 'run' with ${modules}\n")

    // val map = (c.modules map (m => m.name -> m)).toMap

    // val paths = modules
    //   .map { case (name, signal) =>
    //     ModulePath(name, signal, findPath(map, c.main, name)) }

    // val broadcastSources = paths.filter {
    //   case ModulePath(n, s, p) => s.tpe == "broadcast" && s.dir == "source" }
    // val broadcastSinks = paths.filter {
    //   case ModulePath(n, s, p) => s.tpe == "broadcast" && s.dir == "sink" }

    // println(s"[info] Broadcast Sources:")
    // for (x <- broadcastSources) { println(s"[info]   - ${x}") }
    // println(s"[info] Broadcast Sinks:")
    // for (x <- broadcastSinks)   { println(s"[info]   - ${x}") }

    // broadcastSources
    //   .foldLeft(c) {
    //     case (circuit, sourcePath) => {
    //       val ModulePath(_, ChiffreSignal(_, _, sourceName, width), _) = sourcePath
    //       val sinks = broadcastSinks.filter {
    //         case ModulePath(_, ChiffreSignal(_, _, n, _), _) => n == sourceName }
    //       require (sinks.size != 0, s"Found zero sinks for source ${sourceName}")
    //       sinks.map(_.signal.width = width)
    //       val (c, _) = sinks.foldLeft(circuit, Map[String, Port]()){
    //         case ((c, v), sinkPath) => broadcastConnect(c, sourcePath, sinkPath, v) }
    //       c
    //     }}

    case class ChiffreSignal(tpe: String, dir: String, name: String,
      var width: Option[Int] = None)

    val broadcastSources: Seq[(Port, String)] = modules
      .filter{ case (_, s) => s.tpe == "broadcast" && s.dir == "source" }
      .map{ case(modName, ChiffreSignal(_, _, portName, portWidth)) =>
        (Port(
          info = info,
          name = portName,
          direction = Output,
          tpe = UIntType(IntWidth(portWidth.get))
        ),
          modName) }
    println(s"[info] broadcastSources: ${broadcastSources}")

    val broadcastSinks: Map[String, Seq[String]] = modules
      .filter{ case (_, s) => s.tpe == "broadcast" && s.dir == "sink" }
      .map{ case(modName, ChiffreSignal(_, _, portName, portWidth)) =>
        (portName, modName) }
      .groupBy(_._1)
      .map{ case(k, v) => (k, v.map(_._2)) }
    println(s"[info] broadcastSinks: ${broadcastSinks}")

    broadcastSources
      .foldLeft(c)
      .map{ case (port, modName) =>
        broadcastInstrument(c, port, modName, broadcastSinks(port.name)) }
  }
}
