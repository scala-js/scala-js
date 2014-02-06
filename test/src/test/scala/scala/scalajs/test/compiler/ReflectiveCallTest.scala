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

    it("should work with unary methods on primitive types") {
      def fInt(x: Any { def unary_- :Int }) = -x
      expect(fInt(1.toByte)).toEqual(-1)
      expect(fInt(1.toShort)).toEqual(-1)
      expect(fInt(1.toChar)).toEqual(-1)
      expect(fInt(1)).toEqual(-1)

      def fLong(x: Any { def unary_- :Long }) = -x
      expect(fLong(1L)).toEqual(-1L)

      def fFloat(x: Any { def unary_- :Float}) = -x
      expect(fFloat(1.5f)).toEqual(-1.5f)

      def fDouble(x: Any { def unary_- :Double }) = -x
      expect(fDouble(1.5)).toEqual(-1.5)

      def fBoolean(x: Any { def unary_! :Boolean }) = !x
      expect(fBoolean(false)).toBeTruthy
      expect(fBoolean(true)).toBeFalsy
    }

    it("should work with binary operators on primitive types") {
      def fLong(x: Any { def +(x: Long): Long }) = x + 5L
      expect(fLong(5.toByte)).toEqual(10L)
      expect(fLong(10.toShort)).toEqual(15L)
      expect(fLong(10.toChar)).toEqual(15L)
      expect(fLong(-1)).toEqual(4L)
      expect(fLong(17L)).toEqual(22L)

      def fFloat(x: Any { def %(x: Float): Float}) = x % 3.4f
      expect(fFloat(5.5f)).toEqual(2.1f)

      def fDouble(x: Any { def /(x: Double): Double }) = x / 1.4
      expect(fDouble(-1.5)).toEqual(-1.0714285714285714)

      def fBoolean(x: Any { def &&(x: Boolean): Boolean }) = x && true
      expect(fBoolean(false)).toBeFalsy
      expect(fBoolean(true)).toBeTruthy
    }

    it("should work with Arrays") {
      type UPD = { def update(i: Int, x: String): Unit }
      type APL = { def apply(i: Int): String }
      type LEN = { def length: Int }
      def upd(obj: UPD, i: Int, x: String) = obj.update(i,x)
      def apl(obj: APL, i: Int) = obj.apply(i)
      def len(obj: LEN) = obj.length

      val x = Array("asdf","foo","bar")

      expect(len(x)).toEqual(3)
      expect(apl(x,0)).toEqual("asdf")
      upd(x,1,"2foo")
      expect(x(1)).toEqual("2foo")
    }

    it("should work with Arrays of primitive values") {
      type UPD = { def update(i: Int, x: Int): Unit }
      type APL = { def apply(i: Int): Int}
      def upd(obj: UPD, i: Int, x: Int) = obj.update(i,x)
      def apl(obj: APL, i: Int) = obj.apply(i)

      val x = Array(5,2,8)

      expect(apl(x,0)).toEqual(5)
      upd(x,1,1000)
      expect(x(1)).toEqual(1000)
    }

    it("should work with Strings") {
      def get(obj: { def codePointAt(str: Int): Int }) =
        obj.codePointAt(1)
      expect(get("Hi")).toEqual('i'.toInt)
    }

  }
}
