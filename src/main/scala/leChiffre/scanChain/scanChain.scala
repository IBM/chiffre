// See LICENSE for license details.

package leChiffre

package object scanChain {
  abstract trait ScanField {
    def tpe: String
  }
  case class Seed(width: Int) extends ScanField {
    val tpe = "seed"
  }
  case class Difficulty(width: Int) extends ScanField {
    val tpe = "difficulty"
  }
  case class Component(name: String, fields: Seq[ScanField])

  import net.jcazevedo.moultingyaml._
  import net.jcazevedo.moultingyaml.DefaultYamlProtocol
  object ScanChainProtocol extends DefaultYamlProtocol {
    implicit object scanFieldFormat extends YamlFormat[ScanField] {
      def write(f: ScanField) = f match {
        case x: Seed => YamlObject(
          YamlString("tpe") -> YamlString(x.tpe),
          YamlString("width") -> YamlNumber(x.width))
        case x: Difficulty => YamlObject(
          YamlString("tpe") -> YamlString(x.tpe),
          YamlString("width") -> YamlNumber(x.width))
      }
      def read(v: YamlValue) = v.asYamlObject.getFields(
        YamlString("tpe"),
        YamlString("width")) match {
        case Seq(
          YamlString("seed"),
          YamlNumber(w)) => new Seed(w.toInt)
        case Seq(
          YamlString("difficulty"),
          YamlNumber(w)) => new Difficulty(w.toInt)
      }
    }

    implicit val componentFormat = yamlFormat2(Component)
  }

  type ScanChain = Map[String, Seq[Component]]
}
