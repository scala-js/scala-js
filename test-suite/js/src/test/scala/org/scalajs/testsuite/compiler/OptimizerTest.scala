/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import org.scalajs.jasminetest.JasmineTest

import scala.scalajs.js

object OptimizerTest extends JasmineTest {

  describe("Inlineable classes") {

    it("must update fields of `this` in the computation of other fields - #1153") {
      val foo = new InlineClassDependentFields(5)
      expect(foo.x).toEqual(5)
      expect(foo.b).toBeTruthy
      expect(foo.y).toEqual(11)
    }

    it("must not break code that assigns `this` to a field") {
      val foo = new InlineClassThisAlias(5)
      expect(foo.z).toEqual(5)
    }

  }

  describe("Optimizer regression tests") {

    it("must not break * (-1) for Int - #1453") {
      @noinline
      def start0: Int = (() => 10)()

      val start = start0
      val step = -1
      val numRangeElements = start - 1
      val lastElement = start + (numRangeElements - 1) * step
      expect(lastElement).toEqual(2)
    }

    it("must not break * (-1) for Float and Double - #1478") {
      @noinline
      def a: Float = (() => 5.0f)()
      expect(a * -1.0f).toEqual(-5.0f)

      @noinline
      def b: Double = (() => 7.0)()
      expect(b * -1.0).toEqual(-7.0)
    }

    it("must not break foreach on downward Range - #1453") {
      @noinline
      def start0: Int = (() => 10)()

      val elements = js.Array[Int]()
      for (i <- start0 to 2 by -1) {
        if (i < 0)
          sys.error("Going into infinite loop")
        elements.push(i)
      }
      expect(elements).toEqual(js.Array(10, 9, 8, 7, 6, 5, 4, 3, 2))
    }

    it("must not break classOf[T] == classOf[U] - #1658") {
      expect(classOf[String] == classOf[String]).toBeTruthy
      expect(classOf[Int] == classOf[Int]).toBeTruthy
      expect(classOf[Array[Int]] == classOf[Array[Int]]).toBeTruthy
      expect(classOf[Array[String]] == classOf[Array[String]]).toBeTruthy

      expect(classOf[String] == classOf[Int]).toBeFalsy
      expect(classOf[Seq[_]] == classOf[List[_]]).toBeFalsy
      expect(classOf[Array[Int]] == classOf[Array[Integer]]).toBeFalsy
      expect(classOf[Array[Object]] == classOf[Array[Integer]]).toBeFalsy
      expect(classOf[String] == classOf[Array[String]]).toBeFalsy
      expect(classOf[Array[Array[Object]]] == classOf[Array[Object]]).toBeFalsy
    }

  }

  describe("+[string] constant folding") {
    it("must not break when folding two constant strings") {
      @inline def str: String = "I am "
      expect(str + "constant").toEqual("I am constant")
    }

    it("must not break when folding the empty string when associated with a string") {
      @noinline def str: String = "hello"
      expect(str + "").toEqual("hello")
      expect("" + str).toEqual("hello")
    }

    it("must not break when folding 1.4f and a stringLit") {
      expect(1.4f + "hello").toEqual("1.399999976158142hello")
      expect("hello" + 1.4f).toEqual("hello1.399999976158142")
    }

    it("must not break when folding cascading +[string]") {
      @noinline def str: String = "awesome! 10/10"
      expect("Scala.js" + (" is " + str)).toEqual("Scala.js is awesome! 10/10")
      expect((str + " is ") + "Scala.js").toEqual("awesome! 10/10 is Scala.js")
    }

    it("must not break when folding a chain of +[string]") {
      @inline def b: String = "b"
      @inline def d: String = "d"
      @inline def f: String = "f"
      expect("a" + b + "c" + d + "e" + f + "g").toEqual("abcdefg")
    }

    it("must not break when folding integer in double and stringLit") {
      expect(1.0 + "hello").toEqual("1hello")
      expect("hello" + 1.0).toEqual("hello1")
    }

    it("must not break when folding zero and stringLit") {
      expect(0.0 + "hello").toEqual("0hello")
      expect("hello" + 0.0).toEqual("hello0")
      expect(-0.0 + "hello").toEqual("0hello")
      expect("hello" + (-0.0)).toEqual("hello0")
    }

    it("must not break when folding Infinities and stringLit") {
      expect(Double.PositiveInfinity + "hello").toEqual("Infinityhello")
      expect("hello" + Double.PositiveInfinity).toEqual("helloInfinity")
      expect(Double.NegativeInfinity + "hello").toEqual("-Infinityhello")
      expect("hello" + Double.NegativeInfinity).toEqual("hello-Infinity")
    }

    it("must not break when folding NaN and stringLit") {
      expect(Double.NaN + "hello").toEqual("NaNhello")
      expect("hello" + Double.NaN).toEqual("helloNaN")
    }

    unless("fullopt-stage").
    it("must not break when folding double with decimal and stringLit") {
      expect(1.2323919403474454E21 + "hello")
        .toEqual("1.2323919403474454e+21hello")
      expect("hello" + 1.2323919403474454E21)
        .toEqual("hello1.2323919403474454e+21")
    }

    unless("fullopt-stage").
    it("must not break when folding double that JVM would print in scientific notation and stringLit") {
      expect(123456789012345d + "hello")
        .toEqual("123456789012345hello")
      expect("hello" + 123456789012345d)
        .toEqual("hello123456789012345")
    }

    unless("fullopt-stage").
    it("must not break when folding doubles to String"){
      @noinline def toStringNoInline(v: Double): String = v.toString
      @inline def test(v: Double): Unit =
        expect(v.toString).toEqual(toStringNoInline(v))

      // Special cases
      test(0.0)
      test(-0.0)
      test(Double.NaN)
      test(Double.PositiveInfinity)
      test(Double.NegativeInfinity)

      // k <= n <= 21
      test(1.0)
      test(12.0)
      test(123.0)
      test(1234.0)
      test(12345.0)
      test(123456.0)
      test(1234567.0)
      test(12345678.0)
      test(123456789.0)
      test(1234567890.0)
      test(12345678901.0)
      test(123456789012.0)
      test(1234567890123.0)
      test(12345678901234.0)
      test(123456789012345.0)
      test(1234567890123456.0)
      test(12345678901234657.0)
      test(123456789012345678.0)
      test(1234567890123456789.0)
      test(12345678901234567890.0)
      test(123456789012345678901.0)

      // 0 < n <= 21
      test(1.42)
      test(12.42)
      test(123.42)
      test(1234.42)
      test(12345.42)
      test(123456.42)
      test(1234567.42)
      test(12345678.42)
      test(123456789.42)
      test(1234567890.42)
      test(12345678901.42)
      test(123456789012.42)
      test(1234567890123.42)
      test(12345678901234.42)
      test(123456789012345.42)
      test(1234567890123456.42)
      test(12345678901234657.42)
      test(123456789012345678.42)
      test(1234567890123456789.42)
      test(12345678901234567890.42)
      test(123456789012345678901.42)

      // -6 < n <= 0
      test(0.1)
      test(0.01)
      test(0.001)
      test(0.0001)
      test(0.00001)
      test(0.000001)

      // k == 1
      test(1e22)
      test(2e25)
      test(3e50)
      test(4e100)
      test(5e200)
      test(6e300)
      test(7e307)
      test(1e-22)
      test(2e-25)
      test(3e-50)
      test(4e-100)
      test(5e-200)
      test(6e-300)
      test(7e-307)

      // else
      test(1.42e22)
      test(2.42e25)
      test(3.42e50)
      test(4.42e100)
      test(5.42e200)
      test(6.42e300)
      test(7.42e307)
      test(1.42e-22)
      test(2.42e-25)
      test(3.42e-50)
      test(4.42e-100)
      test(5.42e-200)
      test(6.42e-300)
      test(7.42e-307)

      // special cases when ulp > 1
      test(18271179521433728.0)
      test(1.15292150460684685E18)
      test(1234567890123456770.0)
      test(2234567890123456770.0)
      test(4234567890123450000.0)
      test(149170297077708820000.0)
      test(296938164846899230000.0)
      test(607681513323520000000.0)
    }

    it("must not break when folding long and stringLit") {
      expect(1L + "hello").toEqual("1hello")
      expect("hello" + 1L).toEqual("hello1")
    }

    it("must not break when folding integer and stringLit") {
      expect(42 + "hello").toEqual("42hello")
      expect("hello" + 42).toEqual("hello42")
    }

    it("must not break when folding boolean and stringLit") {
      expect(false + " is not true").toEqual("false is not true")
      expect("false is not " + true).toEqual("false is not true")
    }

    it("must not break when folding unit and stringLit") {
      expect(() + " is undefined?").toEqual("undefined is undefined?")
      expect("undefined is " + ()).toEqual("undefined is undefined")
    }

    it("must not break when folding null and stringLit") {
      expect("Damien is not " + null).toEqual("Damien is not null")
    }

    it("must not break when folding char and stringLit") {
      expect('S' + "cala.js").toEqual("Scala.js")
      expect("Scala.j" + 's').toEqual("Scala.js")
    }

  }

  @inline
  class InlineClassDependentFields(val x: Int) {
    val b = x > 3
    val y = if (b) x + 6 else x-2
  }

  @inline
  class InlineClassThisAlias(val x: Int) {
    val t = this
    val y = x
    val z = t.y
  }

}
