// See LICENSE for license details.
package scanChainConfig

import scopt._
import java.io.File
import scala.io.Source
import leChiffre.scan._

import ScanChainProtocol._
import net.jcazevedo.moultingyaml._

case class ScanChainException(msg: String) extends Exception(msg)

case class Arguments(
  scanChainFileName: File = new File("."),
  verbose: Boolean = false,
  probability: Option[Double] = None,
  seed: Option[Int] = None
)

class ScanChainUtils(implicit opt: Arguments) {
  val rand =
    if (!opt.seed.isEmpty) new scala.util.Random(opt.seed.get)
    else new scala.util.Random()

  def getLength(s: Seq[FaultyComponent]): Int = s
    .foldLeft(0) { case (lenC, f) => lenC + f.injector.getWidth }

  def getComponentNames(s: Seq[FaultyComponent]): Seq[String] = s
    .map(_.name)

  def bind(f: ScanField, name: String)(implicit opt: Arguments): ScanField =
    f match {
      case x: Seed => x.copy(value = Some(BigInt(x.width, rand)))
      case x: Difficulty => x
          .copy(probability =
                  Some(opt.probability.getOrElse(
                         throw new Exception(
                           s"Unable to determine ScanField value for $f from arguments" )
                       )))
      case _ => throw new Exception(s"Unimplemented binding for ScanField $f")
    }

  def bind(i: InjectorInfo, name: String)(implicit opt: Arguments): InjectorInfo = {
    i.fields = i.fields.map(bind(_, name))
    i
  }

  def bind(f: FaultyComponent)(implicit opt: Arguments): FaultyComponent = {
    f.copy(injector = bind(f.injector, f.name))
  }

  def bind(s: ScanChain)(implicit opt: Arguments): ScanChain = {
    s.map {
      case (name, chain) => { chain.map { bind(_) } }
        (name, chain)
    }
  }

  def toString(chain: ScanChain, indent: String = ""): String = {
    chain
      .map{case(k, v) =>
        s"""|${indent}${k}:
            |${v.map(_.serialize(indent + "  ")).mkString("\n")}
            |${indent}  raw: ${v.map(_.toBits()).mkString}"""
          .stripMargin}
      .mkString("\n")
  }
}

object Main extends App {
  val parser = new scopt.OptionParser[Arguments]("ScanChainConfig") {
    help("help").text("prints this usage text")

    opt[Unit]("verbose")
      .action( (_, c) => c.copy(verbose = true) )
      .text("Enable verbose output")
    opt[Double]('p', "probability")
      .action( (x, c) => c.copy(probability = Some(x)) )
      .validate( x => if (x >= 0 && x <= 1) success
                else failure("probability <value> must be on domain [0, 1]") )
      .text("Default bit flip probability")
    opt[Int]('s', "seed")
      .action( (x, c) => c.copy(seed = Some(x)) )
      .validate( x => if (x >= 0) success
                else failure("the seed <value> must be greater than or equal to 0") )
      .text("Randome number seed")
    arg[File]("scan.yaml")
      .required()
      .action( (x, c) => c.copy(scanChainFileName = x) )
      .text("A YAML description of the scan chain")
  }

  parser.parse(args, Arguments()) match {
    case Some(x) =>
      implicit val opt = x
      val util = new ScanChainUtils
      val chains = Source.fromFile(opt.scanChainFileName)
        .mkString
        .parseYaml
        .convertTo[ScanChain]

      if (opt.verbose) {
        chains.foreach{ case (name, c) =>
          println(
            s"  [info] Found scan chain '$name' of length ${util.getLength(c)}b")
          println(
            util.getComponentNames(c).foreach{ n =>
              println(s"  [info]   - $n") })
        }
      }

      util.bind(chains)
      println(util.toString(chains))
    case None =>
  }
}
