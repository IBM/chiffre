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

case class ScanFieldException(msg: String) extends Exception(msg)

trait HasWidth {
  val width: Int
}

trait HasName {
  val name: String
}

/** A configurable field of the scan chain */
trait ScanField extends HasName with HasWidth {
  var value: Option[BigInt] = None

  val name: String = this.getClass.getSimpleName

  if (width < 1) {
    throw new ScanFieldException(s"ScanField '$name' width must be greater than 0") }

  def bind(in: BigInt): ScanField = {
    if (in < 0 || in > maxValue) {
      throw new ScanFieldException(
        s"Cannot bind ScanField '$name' to value $in must be on domain [0, $maxValue], but would be ${in}")
    }
    value = Some(in)
    this
  }

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

  def unbind(): ScanField = {
    value = None
    this
  }

  def isBound(): Boolean = value.nonEmpty

  def ==(that: ScanField): Boolean = this.hashCode == that.hashCode

  def !=(that: ScanField): Boolean = this.hashCode != that.hashCode

  override def hashCode: Int = this.serialize().hashCode
}

abstract class SimpleScanField extends ScanField
