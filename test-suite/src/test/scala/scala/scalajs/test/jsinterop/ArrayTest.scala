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

object ArrayTest extends JasmineTest {

  describe("scala.scalajs.js.Array") {

    it("should provide implicit conversion from js.Array to ArrayOps - String") {
      var propCount = 0
      var propString = ""

      for (item <- js.Array("Sc", "ala", ".", "js")) {
        expect(item.isInstanceOf[String]).toBeTruthy
        propCount += 1
        propString += item
      }

      expect(propCount).toEqual(4)
      expect(propString).toEqual("Scala.js")
    }

    it("should provide implicit conversion from js.Array to ArrayOps - Int") {
      var propCount = 0
      var propString = ""

      for (item <- js.Array(7, 3, 5, 7)) {
        expect(item.isInstanceOf[Int]).toBeTruthy
        propCount += 1
        propString += item
      }

      expect(propCount).toEqual(4)
      expect(propString).toEqual("7357")
    }

    it("should provide implicit conversion from js.Array to ArrayOps - Char") {
      var propCount = 0
      var propString = ""

      for (item <- js.Array('S', 'c', 'a', 'l', 'a')) {
        expect(item.isInstanceOf[Char]).toBeTruthy
        propCount += 1
        propString += item
      }

      expect(propCount).toEqual(5)
      expect(propString).toEqual("Scala")
    }

    it("should provide implicit conversion from js.Array to ArrayOps - value class") {
      var propCount = 0
      var propString = ""

      for (item <- js.Array(new VC(5), new VC(-4))) {
        expect(item.isInstanceOf[VC]).toBeTruthy
        propCount += 1
        propString += item
      }

      expect(propCount).toEqual(2)
      expect(propString).toEqual("VC(5)VC(-4)")
    }

  }

  describe("scala.scalajs.js.JSConverters.JSRichGenTraversableOnce") {

    import js.JSConverters._

    it("should provide toJSArray") {
      expect(List("foo", "bar").toJSArray).toEqual(js.Array("foo", "bar"))
      expect(Iterator(1, 2, 3).toJSArray).toEqual(js.Array(1, 2, 3))
      expect(Array(0.3, 7.3, 8.9).toJSArray).toEqual(js.Array(0.3, 7.3, 8.9))
      expect(None.toJSArray).toEqual(js.Array())
      // The following fails on 2.10.x
      //expect(Some("Hello World").toJSArray).toEqual(js.Array("Hello World"))
    }

  }

  private class VC(val x: Int) extends AnyVal {
    override def toString(): String = s"VC($x)"
  }

}
