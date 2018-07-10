// Copyright 2017 IBM
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
package chiffre.scan

/* A configurable region of the scan chain */
sealed trait ScanField {
  def name: String = this.getClass.getSimpleName

  def width: Int

  def value: Option[BigInt]

  def toBits(): String = s"%${width}s"
    .format(value.getOrElse(BigInt(0)).toString(2))
    .replace(' ', '0')

  def serialize(indent: String = ""): String = {
    s"""|${indent}name: $name
        |${indent}  width: $width
        |${indent}  value: ${toBits}"""
      .stripMargin
  }
}

case class Cycle(width: Int, value: Option[BigInt] = None) extends ScanField
case class CycleInject(width: Int, value: Option[BigInt] = None)
    extends ScanField
case class Mask(width: Int, value: Option[BigInt] = None) extends ScanField
case class StuckAt(width: Int, value: Option[BigInt] = None) extends ScanField
case class Difficulty(width: Int, probability: Option[Double] = None)
    extends ScanField {
  val value = if (probability.isEmpty) { None }
  else { Some(BigDecimal((math.pow(2, width) - 1) *
                           probability.getOrElse(0.0)).toBigInt) }
}
case class Seed(width: Int, value: Option[BigInt] = None) extends ScanField

sealed abstract class InjectorInfo {
  def tpe: String
  def width: Int

  /* All configurable fields for this specific injector */
  var fields: Seq[ScanField] = Seq()

  /* The width of this injector's scan chain configuration */
  def getWidth(): Int = fields.foldLeft(0)( (l, r) => l + r.width )

  /* Prety print */
  def serialize(tab: String = ""): String = {
    s"""|${tab}type: $tpe
        |${tab}width: $width
        |${fields.map(a => s"${a.serialize(tab + "  ")}").mkString("\n")}"""
      .stripMargin
  }

  def toBits(): String = fields.map(_.toBits()).mkString
}

/* [todo] You need to rethink this. There needs to be some way of
 * doing a copy of this to update the fields. Assumedly the fields
 * should then be part of the actual case class and not a val
 * inside. */
case class LfsrInjectorInfo(width: Int, lfsrWidth: Int,
                            seed: Option[Seq[BigInt]], probability: Option[Seq[Double]])
    extends InjectorInfo {
  val tpe = s"lfsr$lfsrWidth"
  fields = seed.getOrElse(Seq.fill(width)(BigInt(0))).zip(probability.getOrElse(Seq.fill(width)(0.0)))
    .map{case (s: BigInt, p: Double) => Seq(Seed(lfsrWidth, Some(s)), Difficulty(lfsrWidth, Some(p)))}
    .flatten
}

case class CycleInjectorInfo(width: Int, cycleWidth: Int,
                             cycle: Option[BigInt], value: Option[BigInt])
    extends InjectorInfo {
  val tpe = s"cycle$cycleWidth"
  fields = Seq(Cycle(cycleWidth, cycle), CycleInject(width, value))
}

case class StuckAtInjectorInfo(width: Int,
                               mask: Option[BigInt], value: Option[BigInt])
    extends InjectorInfo {
  val tpe = "stuckAt"
  fields = Seq(Mask(width, mask), StuckAt(width, value))
}

/* The name of a signal and it's associated fault injector */
case class FaultyComponent(name: String, injector: InjectorInfo) {
  def serialize(indent: String): String =
    s"""|${indent}- $name:
        |${injector.serialize(indent + "  ")}"""
      .stripMargin

  def toBits(): String = injector.toBits()
}
