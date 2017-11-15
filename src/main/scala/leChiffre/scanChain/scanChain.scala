// See LICENSE for license details.

package leChiffre

package object scanChain {
  case class Component(name: String, fields: Seq[(String, Int)])

  import net.jcazevedo.moultingyaml.DefaultYamlProtocol
  object ScanChainProtocol extends DefaultYamlProtocol {
    implicit val componentFormat = yamlFormat2(Component)
  }

  type ScanChain = Map[String, Seq[Component]]
}
