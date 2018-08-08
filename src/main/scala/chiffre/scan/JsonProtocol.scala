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
package chiffre.scan

import chiffre.{InjectorInfo, ScanField}

import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write, writePretty}

object JsonProtocol {
  val classId: String = "class"

  private val baseFormat = DefaultFormats
    .preservingEmptyValues
    .withTypeHintFieldName(classId)

  /* Collect all classes in a scan chain  */
  private def getClasses(scanChain: ScanChain): List[Class[_]] = scanChain
    .map { case (k, v) => v.map(fc =>
            fc.injector.getClass +: fc.injector.fields.map(_.getClass)).foldLeft(List[Class[_]]())(_++_) }
    .reduce(_++_)
    .toList.distinct

  /* Collect all classes in some JSON */
  private def getClasses(json: JValue, classId: String = classId): List[Class[_]] =
    (for { JString(name) <- json \\ classId } yield name)
      .distinct
      .map(Class.forName(_).asInstanceOf[Class[_ <: InjectorInfo]])

  class ScanFieldSerializer(formats: Formats) extends CustomSerializer[ScanField](
    format => {
      implicit val f = formats

      def des: PartialFunction[JValue, ScanField] = { case obj: JValue =>
        val value = (obj  \ "value").extract[Option[BigInt]]
        obj.extract[ScanField].bind(value)
      }

      def ser: PartialFunction[Any, JValue] = { case field: ScanField =>
        Extraction.decompose(field) match {
          case JObject(list) => JObject(
            list :+ ("value", Extraction.decompose(field.value) match {
                       case JNothing => JNull
                       case other => other
                     } ))
        }
      }

      (des, ser)
    })

  class InjectorInfoSerializer(formats: Formats) extends CustomSerializer[InjectorInfo](
    format => {
      implicit val f = formats + new ScanFieldSerializer(formats)

      def des: PartialFunction[JValue, InjectorInfo] = { case obj: JObject =>
        val extracted = obj.extract[InjectorInfo]
        val fields = (obj \ "fields").extract[Seq[JObject]]

        val scanFields = fields.map(_.extract[ScanField])

        extracted.fields.zip(scanFields).map{ case (a, b) => a.bind(b.value) }

        extracted
      }

      def ser: PartialFunction[Any, JValue] = { case info: InjectorInfo =>
        Extraction.decompose(info) match {
          case JObject(list) => JObject(
            list :+ ("fields", Extraction.decompose(info.fields)))
        }
      }

      (des, ser)
    })

  def serialize(s: ScanChain): String = {
    implicit val formats = {
      implicit val f = baseFormat + FullTypeHints(getClasses(s))
      f + new InjectorInfoSerializer(f)
    }
    writePretty(s)
  }

  def deserialize(in: JsonInput): ScanChain = {
    def throwError() =
      throw new Exception("Bad scan chain input for deserialization")

    implicit val formats = {
      implicit val f = baseFormat + FullTypeHints(getClasses(parse(in)))
      f + new InjectorInfoSerializer(f)
    }
    read[ScanChain](in)
  }
}
