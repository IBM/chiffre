// See LICENSE for license details.

package leChiffre

package object scan {
  sealed trait ScanField {
    val tpe: String
    val width: Int
  }

  case class Seed(width: Int) extends ScanField {
    val tpe = "seed"
  }
  case class Difficulty(width: Int) extends ScanField {
    val tpe = "difficulty"
  }
  case class Cycle(width: Int) extends ScanField {
    val tpe = "cycle"
  }
  case class Mask(width: Int) extends ScanField {
    val tpe = "mask"
  }
  case class Component(name: String, fields: Seq[ScanField])

  type ScanChain = Map[String, Seq[Component]]

  import net.jcazevedo.moultingyaml._
  import net.jcazevedo.moultingyaml.DefaultYamlProtocol
  object ScanChainProtocol extends DefaultYamlProtocol {
    implicit object scanFieldFormat extends YamlFormat[ScanField] {
      def write(f: ScanField) = f match {
        case x => YamlObject(
          YamlString("tpe") -> YamlString(x.tpe),
          YamlString("width") -> YamlNumber(x.width))
      }
      def read(v: YamlValue) = v.asYamlObject.getFields(
        YamlString("tpe"),
        YamlString("width")) match {
        case Seq(YamlString("seed"),
                 YamlNumber(w)) => new Seed(w.toInt)
        case Seq(YamlString("difficulty"),
                 YamlNumber(w)) => new Difficulty(w.toInt)
        case Seq(YamlString("cycle"),
                 YamlNumber(w)) => new Cycle(w.toInt)
        case Seq(YamlString("mask"),
                 YamlNumber(w)) => new Mask(w.toInt)
      }
    }

    implicit val componentFormat = yamlFormat2(Component)
  }
}
