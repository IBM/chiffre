// See LICENSE for license details.
package scanChainConfig

import scopt._
import java.io.File
import java.io.FileOutputStream
import scala.io.Source
import leChiffre.scan._

import ScanChainProtocol._
import net.jcazevedo.moultingyaml._

case class ScanChainException(msg: String) extends Exception(msg)

case class Arguments(
  scanChainFileName: File = new File("."),
  scanChainDir: Option[File] = None,
  verbose: Boolean = false,
  probability: Option[Double] = None,
  seed: Option[Int] = None,
  mask: Option[BigInt] = None,
  stuckAt: Option[BigInt] = None,
  cycle: Option[BigInt] = None,
  cycleInject: Option[BigInt] = None
)

class ScanChainUtils(implicit opt: Arguments) {
  val rand =
    if (!opt.seed.isEmpty) new scala.util.Random(opt.seed.get)
    else new scala.util.Random()

  def getLength(s: Seq[FaultyComponent]): Int = s
    .foldLeft(0) { case (lenC, f) => lenC + f.injector.getWidth }

  def getComponentNames(s: Seq[FaultyComponent]): Seq[String] = s
    .map(_.name)

  private def bind(f: ScanField, name: String)(implicit opt: Arguments): ScanField =
    f match {
      case x: ScanField if x.value.nonEmpty => throw new ScanChainException(
        s"Tried to rebind already bound ScanField $f (name: $name)")
      case x: Seed => x.copy(value = Some(BigInt(x.width, rand)))
      case x: Difficulty => x
          .copy(probability =
                  Some(opt.probability.getOrElse(
                         throw new ScanChainException(
                           s"Unable to determine ScanField value for $f from arguments" )
                       )))
      case x: Mask => x
          .copy(value =
                  Some(opt.mask.getOrElse(
                         throw new ScanChainException(
                           s"Unable to bind mask for $f from arguments"))) )
      case x: StuckAt => x
          .copy(value =
                  Some(opt.stuckAt.getOrElse(
                         throw new ScanChainException(
                           s"Unable to bind value for $f from arguments"))) )
      case x: Cycle => x
          .copy(value =
                  Some(opt.cycle.getOrElse(
                         throw new ScanChainException(
                           s"Unable to bind value for $f from arguments"))) )
      case x: CycleInject => x
          .copy(value =
                  Some(opt.cycleInject.getOrElse(
                         throw new ScanChainException(
                           s"Unable to bind value for $f from arguments"))) )
      case _ => throw new ScanChainException(s"Unimplemented binding for ScanField $f")
    }

  private def bind(i: InjectorInfo, name: String)(implicit opt: Arguments): Unit =
    i.fields = i.fields.map(bind(_, name))

  private def bind(f: FaultyComponent)(implicit opt: Arguments): Unit =
    bind(f.injector, f.name)

  /* Consume command line options to populate scan chain fields */
  def bind(s: ScanChain)(implicit opt: Arguments): Unit = s.foreach {
    case (name, chain) => { chain.foreach { bind(_) } }
  }

  /* Convert a Scan Chain description to a bit string */
  def bitString(c: Seq[FaultyComponent]): String = c.map(_.toBits()).mkString

  /* Pretty-print all scan chains */
  def serialize(chains: ScanChain, indent: String = ""): String = {
    chains
      .map{case(name, c) =>
        s"""|${indent}${name}:
            |${indent}  length: ${getLength(c)}b
            |${indent}  chain:
            |${c.map(_.serialize(indent + "    ")).mkString("\n")}
            |${indent}  raw: ${bitString(c)}"""
          .stripMargin}
      .mkString("\n")
  }

  def fletcher(x: String, length: Int, n: Int = 32): BigInt = {
    var a: BigInt = 0
    var b: BigInt = 0
    val toDrop = (x.size - length) / (n/2)
    x.grouped(n/2).toList.reverse.dropRight(toDrop).foreach { xx =>
      val word = BigInt(xx, 2)
      a = (a + word) % (1 << n/2)
      b = (a + b) % (1 << n/2)
    }
    (b << n/2) + a
  }

  def padding(x: BigInt, width: Int): String = s"%${width}s"
    .format(x.toString(2))
    .replace(' ', '0')

  def toBinary(chain: Seq[FaultyComponent]): Array[Byte] = {
    val length = getLength(chain)
    val pad = Seq.fill(((-length - 31) % 32) + 31)('0').mkString
    val bits = (pad ++ bitString(chain)).mkString
    println(s"  [info]   pad: $bits")
    val checksum = fletcher(bits, length, n=32)
    println(s"  [info] checksum: $checksum (0b${padding(checksum, 32)})")
    println(s"  [info] length: $length (0b${padding(length, 32)})")
    val raw = bits ++ padding(checksum, 32) ++ padding(length, 32)
    println(s"""  [info] raw: ${raw.grouped(8).mkString(" ")}""")
    println(s"""  [info] raw: ${raw.grouped(4).toList.map(BigInt(_, 2).toString(16)).mkString.grouped(16).mkString(" ")}""")
    println(s"  [info] bytes: ${raw.grouped(8).toArray.map(BigInt(_, 2).toByte).mkString(" ")}")
    require(raw.size % 32 == 0)
    raw.grouped(8).toArray.reverse.map(BigInt(_, 2).toByte)
  }
}

object Main extends App {
  val parser = new scopt.OptionParser[Arguments]("ScanChainConfig") {
    help("help").text("prints this usage text")

    opt[Unit]("verbose")
      .action( (_, c) => c.copy(verbose = true) )
      .text("Enable verbose output")
    opt[File]('o', "output-dir")
      .action( (x, c) => c.copy(scanChainDir = Some(x)) )
      .text("Output directory for scan chain binaries")
    opt[Double]('p', "probability")
      .action( (x, c) => c.copy(probability = Some(x)) )
      .validate( x => if (x >= 0 && x <= 1) success
                else failure("probability <value> must be on domain [0, 1]") )
      .text("Default bit flip probability")
    opt[Int]('s', "seed")
      .action( (x, c) => c.copy(seed = Some(x)) )
      .validate( x => if (x >= 0) success
                else failure("the seed <value> must be greater than or equal to 0") )
      .text("Random number seed")
    opt[String]("mask")
      .action( (x, c) => c.copy(mask = Some(BigInt(x, 16))) )
      .text("A fault mask (bits that will be given a 'stuck-at' value")
    opt[String]("stuck-at")
      .action( (x, c) => c.copy(stuckAt = Some(BigInt(x, 16))) )
      .text("Stuck at bits to apply")
    opt[BigInt]("cycle")
      .action( (x, c) => c.copy(cycle = Some(x) ))
      .validate( x => if (x >= 0) success
                else failure("the cycle <value> must be greater than or equal to 0") )
      .text("The cycle to inject faults")
    opt[String]("cycle-inject")
      .action( (x, c) => c.copy(cycleInject = Some(BigInt(x, 16))) )
      .text("Bit string to inject at <cycle>")
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

      util.bind(chains)

      if (opt.scanChainDir.nonEmpty) {
        val dir = opt.scanChainDir.get
        if (!dir.exists) dir.mkdir()
        chains.foreach { case (name, c) =>
          val w = new FileOutputStream(new File(dir, s"${name}.bin"))
          val bytes = util.toBinary(c)
          println(bytes)
          bytes.foreach(w.write(_))
          w.close()
        }
      }

      if (opt.verbose)
        println(util.serialize(chains, "  [info] "))

      chains.foreach{ case (name, c) => util.toBinary(c) }

    case None =>
  }
}
