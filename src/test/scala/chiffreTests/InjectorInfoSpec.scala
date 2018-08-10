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
package chiffreTests

import chiffre.{InjectorInfo, ScanField, InjectorInfoBindingException}
import chisel3.iotesters.ChiselFlatSpec

import scala.util.{Try, Success, Failure}

case object EmptyInjectorInfo extends InjectorInfo { val fields = Seq.empty }

class InjectorInfoSpec extends ChiselFlatSpec {

  case class DummyField(width: Int) extends ScanField
  case class DummyInjectorInfo(fields: Seq[ScanField]) extends InjectorInfo

  case class F(i: Option[BigInt] = None, width: Int = 4) extends ScanField { bind(i) }
  case class G(i: Option[BigInt] = None, width: Int = 5) extends ScanField { bind(i) }

  def f(): F = F()
  def f(x: Int): F = F(Some(x))
  def g(): G = G()
  def g(x: Int): G = G(i = Some(x))

  behavior of "The InjectoInfo trait"

  it should "have width 0 and report bound if without fields" in {
    val x = EmptyInjectorInfo
    x.width should be (0)
    x.isBound should be (true)
  }

  it should "report the width as the sum of its fields" in {
    val widths = Range(1, 10, 2)
    val x = new DummyInjectorInfo(widths.map(DummyField(_)))
    x.width should be (widths.sum)
  }

  it should "bind and unbind fields to values" in {
    val widths = Range(1, 10, 2)
    val x = new DummyInjectorInfo(widths.map(DummyField(_)))

    x.fields.foreach( _.value should be (None) )
    x.fields.foreach( _.isBound should be (false) )

    val values = widths.map(BigInt(2).pow(_) - 1)
    x.fields.zip(values).foreach{ case (f, v) => f match { case f: DummyField => f.bind(v) } }
    x.fields.zip(values).foreach{ case (f, v) => f.value should be (Some(v)) }
    x.fields.foreach( _.isBound should be (true) )

    x.fields.foreach(_.unbind)
    x.fields.foreach( _.value should be (None) )
    x.fields.foreach( _.isBound should be (false) )
  }

  it should "bind a Seq[ScanField] correctly" in {
    val widths = Range(4, 10, 2)
    val c = DummyInjectorInfo(widths.map(DummyField(_)))

    val badW = widths.tail.map(DummyField(_))
    val badT = widths.map(w => F(width=w))

    val values = Range(0, widths.size).zipWithIndex
      .map{ case (a, i) => if (i % 2 == 0) { Some(BigInt(a)) } else { None } }
      .zip(widths)
      .map{ case (x, w) => DummyField(w).bind(x) }

    info("different widths throw an exception")
    (the [InjectorInfoBindingException] thrownBy{ c.bind(badW) }).msg should startWith ("Unable to bind, widths differ")

    info("different types throw an exception")
    (the [InjectorInfoBindingException] thrownBy{ c.bind(badT) }).msg should startWith ("Unable to bind, types differ")

    info("matching types bind a mix of bound and unbound")
    c.bind(values)
    c.fields.map(_.value) should be (values.map(_.value))
  }

  def equalityChecks( testName: String,
                      injectorInfo: (InjectorInfo, InjectorInfo),
                      tests: Map[(Seq[ScanField], Seq[ScanField]), Boolean] ): Unit = {
    def serialize(i: InjectorInfo): String = i.fields.map(_.value.getOrElse("u")).mkString("")
    it should testName in {
      val (a, b) = injectorInfo
      tests.map { case ((x, y), pass) =>
        a.bind(x)
        b.bind(y)
        val cmp = if (pass) { "==" } else { "!=" }
        info(s"${serialize(a)} $cmp ${serialize(b)}")
        a == b should be (pass)
      }
    }
  }

  val sameTypeSameWidth: Map[(Seq[ScanField], Seq[ScanField]), Boolean] = Map(
    (Seq(f,    g),    Seq(f,    g))    -> true,
    (Seq(f(1), g),    Seq(f,    g))    -> false,
    (Seq(f(1), g),    Seq(f(1), g))    -> true,
    (Seq(f(1), g(1)), Seq(f(1), g))    -> false,
    (Seq(f(1), g(1)), Seq(f(1), g(1))) -> true,
    (Seq(f(0), g(1)), Seq(f(1), g(1))) -> false,
    (Seq(f(0), g(1)), Seq(f(0), g(1))) -> true,
    (Seq(f,    g(1)), Seq(f(0), g(1))) -> false,
    (Seq(f,    g(1)), Seq(f(0), g(1))) -> false,
    (Seq(f,    g(1)), Seq(f,    g(1))) -> true,
  )
  equalityChecks( "compute equality correctly for InjectorInfo of same fields",
                  (DummyInjectorInfo(Seq(f, g)), DummyInjectorInfo(Seq(f, g))),
                  sameTypeSameWidth )

  val allFalse = sameTypeSameWidth.mapValues(_ => false)
  case class FooInjectorInfo(fields: Seq[ScanField]) extends InjectorInfo
  equalityChecks( "compute equality correctly for InjectorInfo of same structure, but different types",
                  (DummyInjectorInfo(Seq(f, g)), FooInjectorInfo(Seq(f, g))),
                  allFalse )

  val allFalseDiffType = allFalse.map{ case ((Seq(a, b), Seq(c, d)), v) => (Seq(a, b), Seq(c, f)) -> v }
  equalityChecks( "compute equality correctly for InjectorInfo of different structure, but same type",
                  (DummyInjectorInfo(Seq(f, g)), FooInjectorInfo(Seq(f, f))),
                  allFalseDiffType )
}
