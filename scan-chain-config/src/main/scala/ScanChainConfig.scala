// See LICENSE for license details.
import scopt.OptionParser
import java.io.File
import java.io.FileOutputStream
import scala.io.Source
import chiffre.scan._

case class ScanChainException(msg: String) extends Exception(msg)

case class Arguments(
  scanChainFileName: File = new File("."),
  scanChainDir: Option[File] = None,
  verbose: Boolean = false
)

class ScanChainUtils(implicit opt: Arguments) {
  def getLength(s: Seq[FaultyComponent]): Int = s
    .foldLeft(0) { case (lenC, f) => lenC + f.injector.getWidth }

  def getComponentNames(s: Seq[FaultyComponent]): Seq[String] = s
    .map(_.name)

  /* Convert a Scan Chain description to a bit string */
  def bitString(c: Seq[FaultyComponent]): String = c.map(_.toBits()).mkString

  /* Pretty-print all scan chains */
  def serialize(chains: ScanChain, indent: String = ""): String = {
    chains
      .map{case(name, c) =>
        s"""|${indent}${name}:
            |${indent}  length: ${getLength(c)}b
            |${indent}  chain:
            |${c.map(_.serialize(indent + "  ")).mkString("\n")}
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
      println(s"[info] fletcher: a: 0x${a.toString(16)}, b: 0x${b.toString(16)}, word: 0x${word.toString(16)}")
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
    println(s"[info]   pad: $bits")
    val checksum = fletcher(bits, length, n=32)
    println(s"[info] checksum: $checksum (0b${padding(checksum, 32)})")
    println(s"[info] length: $length (0b${padding(length, 32)})")
    val raw = bits ++ padding(checksum, 32) ++ padding(length, 32)
    println(s"""[info] raw: ${raw.grouped(8).mkString(" ")}""")
    println(s"""[info] raw: ${raw.grouped(4).toList.map(BigInt(_, 2).toString(16)).mkString.grouped(16).mkString(" ")}""")
    println(s"[info] bytes: ${raw.grouped(8).toArray.map(BigInt(_, 2).toByte).mkString(" ")}")
    require(raw.size % 32 == 0)
    raw.grouped(8).toArray.reverse.map(BigInt(_, 2).toByte)
  }
}

object Main extends App {
  val parser = new OptionParser[Arguments]("ScanChainConfig") {
    help("help").text("prints this usage text")

    opt[Unit]("verbose")
      .action( (_, c) => c.copy(verbose = true) )
      .text("Enable verbose output")
    opt[File]('o', "output-dir")
      .action( (x, c) => c.copy(scanChainDir = Some(x)) )
      .text("Output directory for scan chain binaries")
    arg[File]("scan.yaml")
      .required()
      .action( (x, c) => c.copy(scanChainFileName = x) )
      .text("A YAML description of the scan chain")
  }

  parser.parse(args, Arguments()) match {
    case Some(x) =>
      implicit val opt = x
      val util = new ScanChainUtils
      val chains = JsonProtocol.deserialize(opt.scanChainFileName)

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
        println(util.serialize(chains, "[info] "))

      chains.foreach{ case (name, c) => util.toBinary(c) }

    case None =>
  }
}
