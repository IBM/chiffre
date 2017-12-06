// See LICENSE for license details.

package leChiffre

package object scan {
  sealed trait InjectorInfo {
    val tpe: String
    val width: Int
  }

  case class LfsrInjectorInfo(width: Int, lfsrWidth: Int) extends InjectorInfo {
    val tpe = s"lfsr$lfsrWidth"
  }

  case class CycleInjectorInfo(width: Int, cycleWidth: Int) extends InjectorInfo {
    val tpe = s"cycle$cycleWidth"
  }

  case class StuckAtInjectorInfo(width: Int) extends InjectorInfo {
    val tpe = "stuckAt"
  }

  /* The name of a signal and it's associated fault injector */
  case class FaultyComponent(name: String, injector: InjectorInfo)
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
