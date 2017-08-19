// See LICENSE.IBM for license details.

package leChiffre

import firrtl._
import firrtl.ir._
import firrtl.annotations._

class ChiffreInjectionTransform extends firrtl.Transform {
  override def inputForm: CircuitForm = LowForm
  override def outputForm: CircuitForm = LowForm
  val info = FileInfo(FIRRTLStringLitHandler.unescape("BoostTransform"))

  override def execute(state: CircuitState): CircuitState = {
    getMyAnnotations(state) match {
      case Nil => state
      case a =>
        print(s"[info] in ${this.getClass.getSimpleName}: ${a}\n")

        val modules = a.collect {
          case ChiffreAnnotation(ModuleName(m, c), t) => (m, t) }

        CircuitState(run(state.circuit, modules), state.form)
    }
  }

  def addIo(m: DefModule): DefModule = {
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

  def run(c: Circuit, modules: Seq[(String, String)]): Circuit = {
    val map = (c.modules map (m => m.name -> m)).toMap

    val modulesx = modules map { case (a, b) => addIo(map(a)) }

    val mx = map ++ (modulesx map (i => i.name -> i)).toMap
    c copy(modules = (mx values).toSeq)
  }
}
