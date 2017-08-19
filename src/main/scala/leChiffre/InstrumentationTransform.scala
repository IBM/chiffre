// See LICENSE.IBM for license details.

package leChiffre

import firrtl._
import firrtl.ir._
import firrtl.annotations._
import firrtl.Mappers._

case class ModuleInstance(name: String, module: String)
case class ModulePath(name: String, tpe: String, path: Seq[ModuleInstance])

class ChiffreInjectionTransform extends firrtl.Transform {
  override def inputForm: CircuitForm = LowForm
  override def outputForm: CircuitForm = LowForm
  val info = FileInfo(FIRRTLStringLitHandler.unescape("BoostTransform"))

  override def execute(state: CircuitState): CircuitState = {
    getMyAnnotations(state) match {
      case Nil => state
      case a => val modules = a.
          collect { case ChiffreAnnotation(ModuleName(m, c), t) => (m, t) }.
          distinct

        CircuitState(run(state.circuit, modules), state.form)
    }
  }

  private def addIo(m: DefModule): DefModule = {
    lazy val n = Namespace(m)

    def wRefZero(name: String, tpe: Type): Statement = {
      tpe match {
        case t: GroundType =>
          Connect(info,
            WRef(name, t, ExpKind, UNKNOWNGENDER),
            UIntLiteral(0, t.width))
        case _ =>
          throw AnnotationException("`wRefZero` needs a GroundType")
      }}

    val portsx = Seq(
      Port(info, n.newName("io_SCAN_clk"), Input,  UIntType(IntWidth(1))),
      Port(info, n.newName("io_SCAN_en"),  Input,  UIntType(IntWidth(1))),
      Port(info, n.newName("io_SCAN_in"),  Input,  UIntType(IntWidth(1))),
      Port(info, n.newName("io_SCAN_out"), Output, UIntType(IntWidth(1)))  )

    val stmtsx = portsx map { case Port(_, name, dir, tpe) =>
      dir match {
        case Input => EmptyStmt
        case Output => wRefZero(name, tpe)
        case _ =>
          throw AnnotationException("`addIo` saw unspecified direction on port")
    }}

    m match {
      case m: Module => m copy (
        info = m.info ++ info,
        ports = m.ports ++ portsx,
        body = Block(m.body +: stmtsx))
      case _ =>
        throw AnnotationException("`addIo` received something that was not a Module")
    }
  }

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
      throw AnnotationException(s"Found empty path from '${root}' -> '${leaf}' (likely due to deduplication, try `connectBoost(<c>, <v>, true)`)\n")
    }

    path.reverse
  }

  private def connect(c: Circuit, source: ModulePath, sink: ModulePath):
      Circuit = {
    print(s"[info] connecting: ${source.name} -> ${sink.name}\n")
    val i = (source.path zip sink.path map { case (l, r) => l != r } indexOf(true)) - 1
    val (sourcePath, sinkPath) = (source.path.drop(i), sink.path.drop(i))
    print(s"[info]   - LCS: ${sourcePath(0)}")

    c
  }

  private def run(c: Circuit, modules: Seq[(String, String)]): Circuit = {
    val map = (c.modules map (m => m.name -> m)).toMap

    val modulesx = modules map { case (a, b) => addIo(map(a)) }
    val mx = map ++ (modulesx map (i => i.name -> i)).toMap

    val paths = modules.
      map {case (a, b) => ModulePath(name, b, findPath(map, c.main, a)) }

    val injectees = paths.filter{ case ModulePath(n, t, p) => t == "injectee" }
    val injectors = paths.filter{ case ModulePath(n, t, p) => t == "injector" }

    require(injectors.size == 1, s"Number of CHIFFRE injectors must be 1")

    val chain = injectors +: injectees :+ injectors

    for (p <- chain) { print(s"${p}\n") }

    val cx = c copy(modules = (mx values).toSeq)
    chain.sliding(2).foldLeft(c){case (c, List(source, sink)) => connect(c, source, sink)}
  }
}
