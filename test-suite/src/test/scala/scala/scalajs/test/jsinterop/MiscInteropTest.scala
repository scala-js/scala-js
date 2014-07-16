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

object MiscInteropTest extends JasmineTest {

  describe("scala.scalajs.js.package") {

    it("should provide an equivalent to `typeof x`") {
      import js.typeOf
      expect(typeOf(5)).toEqual("number")
      expect(typeOf(false)).toEqual("boolean")
      expect(typeOf("hello")).toEqual("string")
      expect(typeOf(null)).toEqual("object")
      expect(typeOf(new js.Object)).toEqual("object")
      expect(typeOf(())).toEqual("undefined")
      expect(typeOf(() => 42)).toEqual("function")
    }
  }

  describe("scala.scalajs.js.Object") {

    it("should provide an equivalent to `p in o`") {
      import js.Object.{ hasProperty => hasProp }
      val o = js.Dynamic.literal(foo = 5, bar = "foobar").asInstanceOf[js.Object]
      expect(hasProp(o, "foo")).toBeTruthy
      expect(hasProp(o, "foobar")).toBeFalsy
      expect(hasProp(o, "toString")).toBeTruthy // in prototype
    }

    it("should respect evaluation order for `hasProperty`") {
      import js.Object.{ hasProperty => hasProp }
      var indicator = 3
      def o() = {
        indicator += 4
        js.Dynamic.literal(x = 5).asInstanceOf[js.Object]
      }
      def p() = {
        indicator *= 2
        "x"
      }
      expect(hasProp(o(), p())).toBeTruthy
      expect(indicator).toEqual(14)
    }

    it("should provide equivalent of JS for-in loop of {} - #13") {
      val obj = js.eval("var dictionaryTest13 = { a: 'Scala.js', b: 7357 }; dictionaryTest13;")
      val dict = obj.asInstanceOf[js.Dictionary[js.Any]]
      var propCount = 0
      var propString = ""

      for (prop <- js.Object.properties(dict)) {
        propCount += 1
        propString += dict(prop)
      }

      expect(propCount).toEqual(2)
      expect(propString).toEqual("Scala.js7357")
    }

    it("should provide equivalent of JS for-in loop of [] - #13") {
      val obj = js.eval("var arrayTest13 = [ 7, 3, 5, 7 ]; arrayTest13;")
      val array = obj.asInstanceOf[js.Dictionary[js.Any]]
      var propCount = 0
      var propString = ""

      for (prop <- js.Object.properties(array)) {
        propCount += 1
        propString += array(prop)
      }

      expect(propCount).toEqual(4)
      expect(propString).toEqual("7357")
    }

    it("should compile js.undefined") {
      expect(() => js.undefined.asInstanceOf[js.prim.Number].toString(10)).toThrow
    }
  }

}
