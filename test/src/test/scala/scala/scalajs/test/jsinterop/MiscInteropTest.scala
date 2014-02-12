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
  }

}
