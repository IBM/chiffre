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
package chiffre

case class ScanFieldException(msg: String) extends Exception(msg)

trait HasName {
  def name: String = this.getClass.getSimpleName
}

/* A configurable region of the scan chain */
trait ScanField extends HasName {
  val width: Int
  val value: Option[BigInt]

  if (width < 1) {
    throw new ScanFieldException(s"ScanField '$name' width must be greater than 0") }
  if (value.nonEmpty && (value.get < 0 || value.get > maxValue)) {
    throw new ScanFieldException(s"ScanField '$name' value must be on domain [0, $maxValue], but was ${value.get}") }

  lazy val maxValue = BigInt(2).pow(width) - 1

  def toBits(): String = s"%${width}s"
    .format(value.getOrElse(BigInt(0)).toString(2))
    .replace(' ', '0')

  def serialize(indent: String = ""): String = {
    s"""|${indent}name: $name
        |${indent}  width: $width
        |${indent}  value: ${toBits}"""
      .stripMargin
  }

  def isBound(): Boolean = value.nonEmpty
}
