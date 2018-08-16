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
package chiffre

case class InjectorInfoBindingException(msg: String) extends Exception(msg)

trait InjectorInfo extends HasName with HasWidth with Equals {
  /* All configurable fields for this specific injector */
  val fields: Seq[ScanField]

  val name: String = this.getClass.getSimpleName

  /* The width of this injector's scan chain configuration */
  lazy val width: Int = fields.foldLeft(0)( (l, r) => l + r.width )

  /* Prety print */
  def serialize(tab: String = ""): String = {
    s"""|${tab}class: $name
        |${tab}width: $width
        |${fields.map(a => s"${a.serialize(tab + "  ")}").mkString("\n")}"""
      .stripMargin
  }

  def toBits(): String = fields.map(_.toBits()).mkString

  def bind(in: Seq[ScanField]): InjectorInfo = {
    val widthIn = in.foldLeft(0)( (l, r) => l + r.width )
    if (width != widthIn) {
      throw new InjectorInfoBindingException(s"Unable to bind, widths differ ($width vs. $widthIn) between $name and $in") }
    if (!(fields.zip(in).map{ case (l, r) => l canEqual r }).foldLeft(true)( (l, r) => l & r )) {
      throw new InjectorInfoBindingException(s"Unable to bind, types differ between $name and $in") }
    fields.zip(in).map{ case (l, r) => l.bind(r.value) }
    this
  }

  def isBound(): Boolean = fields.map(_.isBound).reduceOption(_ && _).getOrElse(true)

  def unbind(): InjectorInfo = {
    fields.map(_.unbind)
    this
  }

  override def equals(that: Any): Boolean = that match {
    case that: InjectorInfo => (this canEqual that) &&
        width == that.width &&
        fields.zip(that.fields).foldLeft(true){ case (x, (l, r)) => x & (l == r) }
    case _ => false
  }

  override def hashCode = fields.hashCode
}
