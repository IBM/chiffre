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

/** An exception related to JSON serialization
  * @param msg an exception message
  */
case class JsonSerializationException(msg: String) extends Exception(msg)

object JsonProtocol {
  /** The name/key to use for the name--value pair holding a Scala class
    *
    * @note this matches what FIRRTL does
    */
  val classId: String = "class"

  // A base serialization format
  private val baseFormat = DefaultFormats
    .preservingEmptyValues
    .withTypeHintFieldName(classId)

  // Find all the classes in a ScanChain
  private def getClasses(scanChain: ScanChain): List[Class[_]] = scanChain
    .flatMap { case (k, v) => v.map(fc =>
            fc.injector.getClass +: fc.injector.fields.map(_.getClass)).foldLeft(List[Class[_]]())(_++_) }
    .toList
    .distinct

  // Find all the classes in some JSON
  private def getClasses(json: JValue): List[Class[_]] =
    // For comprehensions to extract all fields matching classId
    (for { JString(name) <- json \\ classId } yield name)
      .distinct
      .map(Class.forName(_).asInstanceOf[Class[_ <: InjectorInfo]])

  /** A JSON serializer/deserializer of a [[ScanField]]
    *
    * @param formats a base [[org.json4s.Formats]] to implicitly pass to
    * extractors
    */
  class ScanFieldSerializer(formats: Formats) extends CustomSerializer[ScanField](
    format => {
      implicit val f = formats

      def des: PartialFunction[JValue, ScanField] = { case obj: JValue =>
        val value = (obj  \ "value").extract[Option[BigInt]]
        obj.extract[ScanField].bind(value)
      }

      def ser: PartialFunction[Any, JValue] = { case field: ScanField =>
        Extraction.decompose(field) match {
          case JObject(list) =>
            JObject(list :+ ("value",
                             Extraction.decompose(field.value) match {
                               case JNothing => JNull
                               case other => other
                             } ))
          case json =>
            throw new JsonSerializationException(s"Unexpected JSON $json when trying to serialize a ScanField")
        }
      }

      (des, ser)
    })

  /** A JSON serializer/deserializer of an [[InjectorInfo]]
    *
    * @param formats a base [[org.json4s.Formats]] to implicitly pass to
    * extractors
    */
  class InjectorInfoSerializer(formats: Formats) extends CustomSerializer[InjectorInfo](
    format => {
      implicit val f = formats + new ScanFieldSerializer(formats)

      def des: PartialFunction[JValue, InjectorInfo] = { case obj: JObject =>
        val extracted = obj.extract[InjectorInfo]
        val scanFields = (obj \ "fields").extract[Seq[JObject]].map(_.extract[ScanField])
        extracted.fields.zip(scanFields).map{ case (a, b) => a.bind(b.value) }
        extracted
      }

      def ser: PartialFunction[Any, JValue] = { case info: InjectorInfo =>
        Extraction.decompose(info) match {
          case JObject(list) => JObject(list :+ ("fields", Extraction.decompose(info.fields)))
          case json =>
            throw new JsonSerializationException(s"Unexpected JSON $json when trying to serialize an InjectorInfo")
        }
      }

      (des, ser)
    })

  /** Convert a [[ScanChain]] to JSON
    *
    * @param scan a scan chain
    * @return some JSON
    */
  def serialize(scan: ScanChain): String = {
    implicit val formats = {
      implicit val f = baseFormat + FullTypeHints(getClasses(scan))
      f + new InjectorInfoSerializer(f)
    }
    writePretty(scan)
  }

  /** Convert [[org.json4s.JsonInput]] to a [[ScanChain]]
    *
    * @param json some JSON
    * @return a scan chain
    */
  def deserialize(json: JsonInput): ScanChain = {
    def throwError() =
      throw new Exception("Bad scan chain input for deserialization")

    implicit val formats = {
      implicit val f = baseFormat + FullTypeHints(getClasses(parse(json)))
      f + new InjectorInfoSerializer(f)
    }
    read[ScanChain](json)
  }
}
