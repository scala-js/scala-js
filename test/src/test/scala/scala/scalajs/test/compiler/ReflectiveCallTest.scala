/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package compiler

import scala.scalajs.js
import scala.scalajs.test.JasmineTest

import language.reflectiveCalls

object ReflectiveCallTest extends JasmineTest {

  describe("Reflective Calls") {
    it("should allow subtyping in return types") {
      class A { def x = 1 }
      class B extends A { override def x = 2 }

      object Generator {
        def generate(): B = new B
      }

      def f(x: { def generate(): A }) = x.generate

      expect(f(Generator).x).toEqual(2)
    }

    it("should allow this.type in return types") {
      type valueType = { def value: this.type }
      def f(x: valueType) = x.value

      class StringValue(x: String) {
        def value: this.type = this
        override def toString = s"StringValue($x)"
      }

      expect(f(new StringValue("foo")).toString).toEqual("StringValue(foo)")
    }

    it("should allow generic return types") {
      case class Tata(name: String)

      object Rec {
        def e(x: Tata) = new Tata("iei")
      }

      def m[T](r: Object { def e(x: Tata): T}) =
        r.e(new Tata("foo"))

      expect(m[Tata](Rec).toString).toEqual("Tata(iei)")
    }
  }
}
