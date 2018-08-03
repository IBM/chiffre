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

abstract class InjectorInfo(val fields: Seq[ScanField], values: Option[Seq[BigInt]] = None) extends HasWidth {
  values.foreach(v => {
    if (v.size != 2)
      throw new ScanFieldException(s"${getClass.getSimpleName}: Wrong number of values (found ${v.size}; required ${fields.size})")
    fields zip v map { case (f, vx) => f.bind(vx) }
  })

  /* The width of this injector's scan chain configuration */
  lazy val width: Int = fields.foldLeft(0)( (l, r) => l + r.width )

  /* Prety print */
  def serialize(tab: String = ""): String = {
    s"""|${tab}class: ${this.getClass.getSimpleName}
        |${tab}width: $width
        |${fields.map(a => s"${a.serialize(tab + "  ")}").mkString("\n")}"""
      .stripMargin
  }

  def toBits(): String = fields.map(_.toBits()).mkString

  def isBound(): Boolean = fields.map(_.isBound).reduceOption(_ && _).getOrElse(true)
}
