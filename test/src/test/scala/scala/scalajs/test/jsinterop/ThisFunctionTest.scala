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

object ThisFunctionTest extends JasmineTest {

  describe("scala.scalajs.js.ThisFunctionN") {

    it("should provide an implicit conversion from Scala function to js.ThisFunction") {
      val g = js.eval("""
          var g = function(f, x) { return f.call(x, 42, x.foo); }; g;
      """).asInstanceOf[js.Function2[js.ThisFunction2[
          js.Dynamic, Int, String, String], js.Dynamic, String]]

      val f = { (thiz: js.Dynamic, v: Int, u: String) =>
        expect(thiz).toBeTruthy()
        expect(thiz.foobar).toEqual("foobar")
        u + v
      }
      val obj = js.Object().asInstanceOf[js.Dynamic]
      obj.foo = "foo"
      obj.foobar = "foobar"
      expect(g(f, obj)).toEqual("foo42")
    }

    it("should accept a lambda where a js.ThisFunction is expected") {
      val g = js.eval("""
          var g = function(f, x) { return f.call(x, 42, x.foo); }; g;
      """).asInstanceOf[js.Function2[js.ThisFunction2[
          js.Dynamic, Int, String, String], js.Dynamic, String]]

      val obj = js.Object().asInstanceOf[js.Dynamic]
      obj.foo = "foo"
      obj.foobar = "foobar"
      expect(g({ (thiz: js.Dynamic, v: Int, u: String) =>
        expect(thiz).toBeTruthy()
        expect(thiz.foobar).toEqual("foobar")
        u + v
      }, obj)).toEqual("foo42")
    }

    it("should bind the first argument to this when applying js.ThisFunctionN") {
      val g = js.eval("""
          var g = function(x) { return this.foo + ":" + x; }; g;
      """).asInstanceOf[js.ThisFunction1[js.Dynamic, Int, String]]
      val obj = js.Object().asInstanceOf[js.Dynamic]
      obj.foo = "foo"
      expect(g(obj, 42)).toEqual("foo:42")
    }

    it("should provide an implicit conversion from js.ThisFunction to Scala function") {
      val g = js.eval("""
          var g = function(x) { return this.foo + ":" + x; }; g;
      """).asInstanceOf[js.ThisFunction1[js.Dynamic, Int, String]]
      val f: scala.Function2[js.Dynamic, Int, String] = g
      val obj = js.Object().asInstanceOf[js.Dynamic]
      obj.foo = "foo"
      expect(f(obj, 42)).toEqual("foo:42")
    }

  }
}
