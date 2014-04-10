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

object DictionaryTest extends JasmineTest {

  describe("scala.scalajs.js.Dictionary") {

    it("should provide an equivalent of the JS delete keyword - #255") {
      val obj = js.Dictionary.empty[js.Any]
      obj("foo") = 42
      obj("bar") = "foobar"

      expect(obj("foo")).toEqual(42)
      expect(obj("bar")).toEqual("foobar")
      expect(obj.delete("foo")).toBeTruthy
      expect(obj("foo")).toBeUndefined
      expect(obj.asInstanceOf[js.Object].hasOwnProperty("foo")).toBeFalsy
      expect(obj("bar")).toEqual("foobar")
    }

    // This fails on fullOpt. We don't know why. Filed as #461
    xit("should behave as specified when deleting a non-configurable property - #461") {
      val obj = js.Dictionary.empty[js.Any]
      js.Object.defineProperty(obj.asInstanceOf[js.Object], "nonconfig",
          js.Dynamic.literal(value = 4, writable = false).asInstanceOf[js.PropertyDescriptor])
      expect(obj("nonconfig")).toEqual(4)
      expect(obj.delete("nonconfig")).toBeFalsy
      expect(obj("nonconfig")).toEqual(4)
    }

  }
}
