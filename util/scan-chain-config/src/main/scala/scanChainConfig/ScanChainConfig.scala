// See LICENSE for license details.
package scanChainConfig

import scopt._
import java.io.File
import scala.io.Source
import leChiffre.scanChain._

import ScanChainProtocol._
import net.jcazevedo.moultingyaml._

case class Arguments(
  scanChainFileName: File = new File(".")
)

object Main extends App {
  val parser = new scopt.OptionParser[Arguments]("scopt") {
    help("help").text("prints this usage text")

    arg[File]("scan.yaml")
      .required()
      .action( (x, c) => c.copy(scanChainFileName = x) )
      .text("A YAML description of the scan chain")
  }

  val Some(opt) = parser.parse(args, Arguments())

  val yaml = Source.fromFile(opt.scanChainFileName)
    .mkString
    .parseYaml
    .convertTo[ScanChain]

  yaml.foreach{ case (k, v) => println(s"  [info] Found scan chain '$k'") }
}
