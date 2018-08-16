// Copyright 2018 IBM
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
package chiffre.util

import chiffre.{ScanField, FaultyComponent, InjectorInfo, ScanFieldUnboundException}
import chiffre.scan.{ScanChain, JsonProtocol}
import chiffre.inject.{Seed, Difficulty, Mask, StuckAt, Cycle, CycleInject}
import scopt.OptionParser
import java.io.{File, FileOutputStream, PrintWriter}
import scala.io.Source

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
  cycleInject: Option[BigInt] = None,
  errorIfBound: Boolean = false
)

class ScanChainUtils(implicit opt: Arguments) {
  val rand =
    if (!opt.seed.isEmpty) { new scala.util.Random(opt.seed.get) }
    else                   { new scala.util.Random()             }

  def getLength(s: Seq[FaultyComponent]): Int = s
    .foldLeft(0) { case (lenC, f) => lenC + f.injector.width }

  def getComponentNames(s: Seq[FaultyComponent]): Seq[String] = s
    .map(_.name)

  private def bind(f: ScanField, name: String)(implicit opt: Arguments): ScanField = try {
    f match {
      case x: ScanField if x.value.nonEmpty =>
        if (opt.errorIfBound) {
          throw new ScanChainException(s"Tried to rebind already bound ScanField $f (name: $name)") }
        x
      case x: Seed        => x.bind(BigInt(x.width, rand))
      case x: Difficulty  => x.bind(opt.probability)
      case x: Mask        => x.bind(opt.mask)
      case x: StuckAt     => x.bind(opt.stuckAt)
      case x: Cycle       => x.bind(opt.cycle)
      case x: CycleInject => x.bind(opt.cycleInject)
      case _              => throw new ScanChainException(s"Unimplemented binding for ScanField $f")
    }
  } catch {
    case _: ScanFieldUnboundException =>
      throw new ScanChainException(s"Cannot bind ScanField $f based on available arguments")
  }

  private def bind(i: InjectorInfo, name: String)(implicit opt: Arguments): Unit =
    i.fields.foreach(bind(_, name))

  private def bind(f: FaultyComponent)(implicit opt: Arguments): Unit = {
    bind(f.injector, f.name)
    val bound = f.injector.fields.foldLeft(true)( (notEmpty, x) => { notEmpty & x.value.nonEmpty } )
    if (!bound) {
      throw new ScanChainException(s"Unable to bind ${f.injector} based on command line options") }
  }

  /* Consume command line options to populate scan chain fields */
  def bind(s: ScanChain)(implicit opt: Arguments): Unit = s.foreach {
    case (name, chain) => { chain.foreach { bind(_) } }
  }

  /* Convert a Scan Chain description to a bit string */
  def bitString(c: Seq[FaultyComponent]): String = c.map(_.toBits()).mkString

  /* Pretty-print all scan chains */
  def serialize(chains: ScanChain, indent: String = ""): String = JsonProtocol.serialize(chains)
    .split("\n").map(indent + _).mkString("\n")

  // scalastyle:off magic.number
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
  } // scalastyle:on magic.number

  def padding(x: BigInt, width: Int): String = s"%${width}s"
    .format(x.toString(2))
    .replace(' ', '0')

  // scalastyle:off magic.number
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
    println(
      s"""[info] raw: ${raw.grouped(4).toList.map(BigInt(_, 2).toString(16)).mkString.grouped(16).mkString(" ")}""")
    println(s"[info] bytes: ${raw.grouped(8).toArray.map(BigInt(_, 2).toByte).mkString(" ")}")
    require(raw.size % 32 == 0)
    raw.grouped(8).toArray.reverse.map(BigInt(_, 2).toByte)
  }
  // scalastyle:on magic.number
}

object Driver {
  val parser = new OptionParser[Arguments]("ScanChainConfig") {
    help("help").text("prints this usage text")

    opt[Unit]("verbose")
      .action( (_, c) => c.copy(verbose = true) )
      .text("Enable verbose output")
    opt[File]('o', "output-dir")
      .action( (x, c) => c.copy(scanChainDir = Some(x)) )
      .text("Output directory for scan chain binaries")
    opt[Double]('p', "probability")
      .action( (x, c) => c.copy(probability = Some(x)) )
      .validate( x =>
        if (x >= 0 && x <= 1) { success                                                 }
        else                  { failure("probability <value> must be on domain [0, 1]") } )
      .text("Default bit flip probability")
    opt[Int]('s', "seed")
      .action( (x, c) => c.copy(seed = Some(x)) )
      .validate( x =>
        if (x >= 0) { success                                                        }
        else        { failure("the seed <value> must be greater than or equal to 0") } )
      .text("Random number seed")
    opt[String]("mask")
      .action( (x, c) => c.copy(mask = Some(BigInt(x, 16))) ) // scalastyle:ignore magic.number
      .text("A fault mask (bits that will be given a 'stuck-at' value")
    opt[String]("stuck-at")
      .action( (x, c) => c.copy(stuckAt = Some(BigInt(x, 16))) ) // scalastyle:ignore magic.number
      .text("Stuck at bits to apply")
    opt[BigInt]("cycle")
      .action( (x, c) => c.copy(cycle = Some(x) ))
      .validate( x =>
        if (x >= 0) { success                                                         }
        else        { failure("the cycle <value> must be greater than or equal to 0") } )
      .text("The cycle to inject faults")
    opt[String]("cycle-inject")
      .action( (x, c) => c.copy(cycleInject = Some(BigInt(x, 16))) ) // scalastyle:ignore magic.number
      .text("Bit string to inject at <cycle>")
    opt[Unit]("error-if-bound")
      .action( (_, c) => c.copy(errorIfBound = true) )
      .text("Error if a command line argument would bind an already bound value")
    arg[File]("scan.json")
      .required()
      .action( (x, c) => c.copy(scanChainFileName = x) )
      .text("A JSON description of the scan chain")
  }

  def main(args: Array[String]): Unit = parser.parse(args, Arguments()) match {
    case Some(x) =>
      implicit val opt = x
      val util = new ScanChainUtils
      val chains = JsonProtocol.deserialize(opt.scanChainFileName)

      println(util.serialize(chains, "[info] "))

      util.bind(chains)

      if (opt.scanChainDir.nonEmpty) {
        val dir = opt.scanChainDir.get
        if (!dir.exists) dir.mkdir()
        val boundJson = JsonProtocol.serialize(chains)
        val w = new PrintWriter(new File(dir, s"bound.json"))
        w.write(boundJson)
        w.close()
        chains.foreach { case (name, c) =>
          val w = new FileOutputStream(new File(dir, s"${name}.bin"))
          val bytes = util.toBinary(c)
          println(bytes)
          bytes.foreach(w.write(_))
          w.close()
        }
      }

      if (opt.verbose) { println(util.serialize(chains, "[info] ")) }

      chains.foreach{ case (name, c) => util.toBinary(c) }

    case None =>
  }
}
