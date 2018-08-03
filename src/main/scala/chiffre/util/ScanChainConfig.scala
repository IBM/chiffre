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
import java.io.File
import java.io.FileOutputStream
import scala.io.Source

case class ScanChainException(msg: String) extends Exception(msg)

case class Arguments(
  scanChainFileName: File = new File("."),
  scanChainDir: Option[File] = None,
  verbose: Boolean = false
)

class ScanChainUtils(implicit opt: Arguments) {
  def getLength(s: Seq[FaultyComponent]): Int = s
    .foldLeft(0) { case (lenC, f) => lenC + f.injector.width }

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
    arg[File]("scan.json")
      .required()
      .action( (x, c) => c.copy(scanChainFileName = x) )
      .text("A JSON description of the scan chain")
  }

  def main(args: Array[String]): Unit = parser.parse(args, Arguments()) match {
    case Some(x) =>
      implicit val opt = x
      val util = new ScanChainUtils
      try {
        val chains = JsonProtocol.deserialize(opt.scanChainFileName)

        chains.foreach{ case (id, components) => components.foreach{ case FaultyComponent(name, info) => {
          info.fields.foreach(f => if (!f.isBound) throw new ScanChainException(s"Unbound field: ${name}:${info.getClass.getSimpleName}.${f.name}"))
        }}}

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

        if (opt.verbose) { println(util.serialize(chains, "[info] ")) }

        chains.foreach{ case (name, c) => util.toBinary(c) }
      } catch {
        case org.json4s.MappingException(_, cause) => cause match {
          case e: java.lang.reflect.InvocationTargetException => throw e.getCause()
          case e => throw e
        }
        case e: Exception => throw e
      }

    case None =>
  }
}
