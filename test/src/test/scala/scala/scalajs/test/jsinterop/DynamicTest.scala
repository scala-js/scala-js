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

    it("should allow instanciating JS classes dynamically - #10") {
      js.eval("function DynamicTestClass(x) { this.x = x; };")
      val obj = js.Dynamic.newInstance(js.Dynamic.global.DynamicTestClass)("Scala.js")
      expect(obj.x).toEqual("Scala.js")
    }
  }
}
