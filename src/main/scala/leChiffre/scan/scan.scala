// See LICENSE for license details.

package leChiffre

package object scan {
  /* A configurable region of the scan chain */
  sealed trait ScanField {
    def name = this.getClass.getSimpleName

    def width: Int

    def value: Option[BigInt]

    def toBits(): String = s"%${width}s"
      .format(value.getOrElse(BigInt(0)).toString(2))
      .replace(' ', '0')

    def serialize(indent: String = ""): String = {
      s"""|${indent}name: $name
          |${indent}  width: $width
          |${indent}  value: ${toBits}"""
        .stripMargin
    }
  }

  case class Cycle(width: Int, value: Option[BigInt] = None) extends ScanField
  case class Mask(width: Int, value : Option[BigInt] = None) extends ScanField
  case class Difficulty(width: Int, probability: Option[Double] = None)
      extends ScanField {
    val value = if (probability.isEmpty) None
    else Some(
      BigDecimal((math.pow(2, width) - 1) * probability.getOrElse(0.0)).toBigInt)
  }
  case class Seed(width: Int, value: Option[BigInt] = None) extends ScanField

  sealed abstract class InjectorInfo {
    def tpe: String
    def width: Int

    /* All configurable fields for this specific injector */
    var fields: Seq[ScanField] = Seq()

    /* The width of this injector's scan chain configuration */
    def getWidth(): Int = fields.foldLeft(0)( (l, r) => l + r.width )

    /* Prety print */
    def serialize(indent: String = ""): String = {
      s"""|${indent}type: $tpe
          |${indent}width: $width
          |${fields.map(a => s"${a.serialize(indent + "  ")}").mkString("\n")}"""
        .stripMargin
    }

    def toBits(): String = fields.map(_.toBits()).mkString
  }

  /* [todo] You need to rethink this. There needs to be some way of
   * doing a copy of this to update the fields. Assumedly the fields
   * should then be part of the actual case class and not a val
   * inside. */
  case class LfsrInjectorInfo(width: Int, lfsrWidth: Int) extends InjectorInfo {
    val tpe = s"lfsr$lfsrWidth"
    fields = Seq.fill(width)(Seq(Difficulty(lfsrWidth), Seed(lfsrWidth)))
      .flatten
  }

  case class CycleInjectorInfo(width: Int, cycleWidth: Int) extends InjectorInfo {
    val tpe = s"cycle$cycleWidth"
    fields = Seq(Cycle(cycleWidth), Mask(width))
  }

  case class StuckAtInjectorInfo(width: Int) extends InjectorInfo {
    val tpe = "stuckAt"
    fields = Seq(Mask(width))
  }

  /* The name of a signal and it's associated fault injector */
  case class FaultyComponent(name: String, injector: InjectorInfo) {
    def serialize(indent: String): String =
      s"""|${indent}$name:
          |${injector.serialize(indent + "  ")}"""
        .stripMargin

    def toBits(): String = injector.toBits()
  }
  type ScanChain = Map[String, Seq[FaultyComponent]]

  import net.jcazevedo.moultingyaml._
  import net.jcazevedo.moultingyaml.DefaultYamlProtocol
  object ScanChainProtocol extends DefaultYamlProtocol {
    implicit object scanFieldFormat extends YamlFormat[InjectorInfo] {
      def write(f: InjectorInfo) = f match {
        case x => YamlObject(
          YamlString("tpe") -> YamlString(x.tpe),
          YamlString("width") -> YamlNumber(x.width))
      }
      val matcher = raw"(\w+?)(\d+)".r
      def read(v: YamlValue) = v.asYamlObject.getFields(
        YamlString("tpe"),
        YamlString("width")) match {
        case Seq(YamlString(matcher(name, num)),
                 YamlNumber(w)) => name match {
          case "lfsr" => new LfsrInjectorInfo(w.toInt, num.toInt)
          case "cycle" => new CycleInjectorInfo(w.toInt, num.toInt) }
        case Seq(YamlString("stuckAt"),
                 YamlNumber(w)) => new StuckAtInjectorInfo(w.toInt)
        case _ => throw new Exception(
          "Failed to parse Yaml in ScanChainProtocol")
      }
    }

    implicit val componentFormat = yamlFormat2(FaultyComponent)
  }
}
