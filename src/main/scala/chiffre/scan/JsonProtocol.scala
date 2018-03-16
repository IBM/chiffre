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

import org.json4s._
import org.json4s.native.JsonMethods.parse
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write, writePretty}

import chiffre.scan.InjectorInfo

object JsonProtocol {
  def getTags(s: scan.ScanChain): List[Class[_]] = {
    // Collect all classes that may exist in the scan chain. I'm using
    // map/reduce as flatMap is throwing a type error.
    val x = s.map{ case (k, v) => v.map(fc => fc.injector.getClass +:
                                          fc.injector.fields.map(_.getClass)).reduce(_++_) }
      .reduce(_++_)
      .toList.distinct

    // [todo] remove
    x.foreach(xx => println(s" getTags: $xx"))

    x
  }

  def serialize(s: scan.ScanChain): String = {
    implicit val formats = Serialization
      .formats(FullTypeHints(getTags(s)))
      .withTypeHintFieldName("class")
    writePretty(s)
  }

  def deserialize(in: JsonInput): scan.ScanChain = {
    def throwError() = throw new Exception("Bad scan chain input for deserialization")

    val classNames: List[String] = parse(in) match {
      case JObject(sc) => sc.flatMap {
        case (_, JArray(components)) => components.map {
          case JObject(_ :: ("injector", JObject(("class", JString(c)) :: _)) :: _) => c
          case _ => throwError()
        }
        case _ => throwError()
      }
      case _ => throwError()
    }
    val classes: List[Class[_ <: InjectorInfo]] = classNames.map(Class.forName(_).asInstanceOf[Class[_ <: InjectorInfo]])
    classes.foreach(println(_))

    implicit val formats = Serialization
      .formats(FullTypeHints(classes))
      .withTypeHintFieldName("class")
    read[chiffre.scan.ScanChain](in)
  }
}
