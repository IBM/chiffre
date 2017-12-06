// See LICENSE for license details.
package scanChainConfig

import scopt._
import java.io.File
import scala.io.Source
import leChiffre.scan._

import ScanChainProtocol._
import net.jcazevedo.moultingyaml._

case class Arguments(
  scanChainFileName: File = new File("."),
  verbose: Boolean = false
)

object ScanChainUtils {
  def getLength(s: Seq[FaultyComponent]): Int = s
    .foldLeft(0) { case (lenC, f) => lenC +
      (f.injector match {
         case LfsrInjectorInfo(width, lfsrWidth) => width * lfsrWidth
         case CycleInjectorInfo(width, cycleWidth) => width + cycleWidth
         case StuckAtInjectorInfo(width) => width
         case _ => 0
       })
    }

  def getComponentNames(s: Seq[FaultyComponent]): Seq[String] = s
    .map(_.name)
}

object Main extends App {
  val parser = new scopt.OptionParser[Arguments]("ScanChainConfig") {
    help("help").text("prints this usage text")

    opt[Unit]("verbose")
      .action( (_, c) => c.copy(verbose = true) )
      .text("Enable verbose output")
    arg[File]("scan.yaml")
      .required()
      .action( (x, c) => c.copy(scanChainFileName = x) )
      .text("A YAML description of the scan chain")
  }

  val Some(opt) = parser.parse(args, Arguments())

  val chains = Source.fromFile(opt.scanChainFileName)
    .mkString
    .parseYaml
    .convertTo[ScanChain]

  if (opt.verbose) {
    chains.foreach{ case (k, v) =>
      println(
        s"  [info] Found scan chain '$k' of length ${ScanChainUtils.getLength(v)}b")
      println(
        ScanChainUtils.getComponentNames(v).foreach{ n =>
          println(s"  [info]   - $n") })
    }
  }
}
