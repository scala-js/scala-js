/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package jsinterop

import scala.scalajs.js
import scala.scalajs.test.JasmineTest

import js.annotation.JSExport

object DynamicTest extends JasmineTest {

  describe("scala.scalajs.js.Dynamic") {

    it("should workaround Scala 2.10 issue with implicit conversion for dynamic fields named x - #8") {
      class Point(val x: Int, val y: Int)

      def jsonToPoint(json: js.Dynamic) = {
        new Point(json.x.toString.toInt, json.y.toString.toInt)
      }

      val json = js.eval("var dynamicTestPoint = { x: 1, y: 2 }; dynamicTestPoint;")
      val point = jsonToPoint(json.asInstanceOf[js.Dynamic])

      expect(point.x).toEqual(1)
      expect(point.y).toEqual(2)
    }

    it("should allow to call functions with arguments named x") {
      class A {
        def a = 1
      }

      class B extends A {
        @JSExport
        def x(par: Int) = a + par // make sure `this` is bound correctly in JS
      }

      val b = (new B).asInstanceOf[js.Dynamic]

      expect(b.x(10)).toEqual(11)
    }

    it("should allow instanciating JS classes dynamically - #10") {
      js.eval("function DynamicTestClass(x) { this.x = x; };")
      val obj = js.Dynamic.newInstance(js.Dynamic.global.DynamicTestClass)("Scala.js")
      expect(obj.x).toEqual("Scala.js")
    }

    it("should provide an object literal construction") {
      import js.Dynamic.{ literal => obj }
      val x = obj(foo = 3, bar = "foobar")
      expect(x.foo).toEqual(3)
      expect(x.bar).toEqual("foobar")
      expect(x.unknown).toBeUndefined()

      val y = obj(
          inner = obj(name = "inner obj"),
          fun = { () => 42 }
      )
      expect(y.inner.name).toEqual("inner obj")
      expect(y.fun()).toEqual(42)

      expect(obj().anything).toBeUndefined()
    }

    it("should provide object literal construction with dynamic naming") {
      import js.Dynamic.{ literal => obj }
      val x = obj("foo" -> 3, "bar" -> "foobar")
      expect(x.foo).toEqual(3)
      expect(x.bar).toEqual("foobar")
      expect(x.unknown).toBeUndefined()

      val tup1 = ("hello1", 3: js.Any)
      val tup2 = ("hello2", 10: js.Any)

      val y = obj(tup1, tup2)
      expect(y.hello1).toEqual(3)
      expect(y.hello2).toEqual(10)

      var count = 0
      val z = obj({ count += 1; ("foo", "bar")})
      expect(z.foo).toEqual("bar")
      expect(count).toEqual(1)
    }

    it("should allow to create an empty object with the literal syntax") {
      import js.Dynamic.{ literal => obj }
      val x = obj()
      expect(x.isInstanceOf[js.Object]).toBeTruthy()
    }

    it("should properly encode object literal property names") {
      import js.Dynamic.{ literal => obj }

      val obj0 = obj("3-" -> 42)
      expect(obj0.`3-`).toEqual(42)

      val obj0Dict = obj0.asInstanceOf[js.Dictionary[js.Any]]
      expect(obj0Dict("3-")).toEqual(42)

      val checkEvilProperties = js.eval("""
        function dynamicLiteralNameEncoding_checkEvilProperties(x) {
          return x['.o[3√!|-pr()per7:3$];'] === ' such eval ';
        }
        dynamicLiteralNameEncoding_checkEvilProperties
      """).asInstanceOf[js.Function1[js.Any, Boolean]]
      val obj1 = obj(
          ".o[3√!|-pr()per7:3$];" -> " such eval ").asInstanceOf[js.Dictionary[js.Any]]
      expect(obj1(".o[3√!|-pr()per7:3$];")).toEqual(" such eval ")
      expect(checkEvilProperties(obj1)).toEqual(true)

      val checkQuotesProperty = js.eval("""
        function dynamicLiteralNameEncoding_quote(x) {
          return x["'" + '"'] === 7357;
        }
        dynamicLiteralNameEncoding_quote
      """).asInstanceOf[js.Function1[js.Any, Boolean]]

      val quote = '"'

      Seq(
        obj("'" + quote -> 7357),
        obj(s"'$quote" -> 7357),
        obj("'\"" -> 7357),
        obj("'" + quote -> 7357)
      ).foreach { o =>
        val dict = o.asInstanceOf[js.Dictionary[js.Any]]
        expect(dict("'\"")).toEqual(7357)
        expect(dict("'" + quote)).toEqual(7357)
        expect(dict(s"'$quote")).toEqual(7357)
        expect(checkQuotesProperty(o)).toEqual(true)
      }
    }
  }
}
