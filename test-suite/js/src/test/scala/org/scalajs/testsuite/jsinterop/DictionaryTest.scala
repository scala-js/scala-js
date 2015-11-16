/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.scalajs.js
import org.scalajs.jasminetest.JasmineTest

object DictionaryTest extends JasmineTest {

  describe("scala.scalajs.js.Dictionary") {

    it("should provide an equivalent of the JS delete keyword - #255") {
      val obj = js.Dictionary.empty[js.Any]
      obj("foo") = 42
      obj("bar") = "foobar"

      expect(obj("foo")).toEqual(42)
      expect(obj("bar")).toEqual("foobar")
      obj.delete("foo")
      expect(obj.contains("foo")).toBeFalsy
      expect(obj.asInstanceOf[js.Object].hasOwnProperty("foo")).toBeFalsy
      expect(obj("bar")).toEqual("foobar")
    }

    // This doesn't work on Rhino due to lack of full strict mode support - #679
    unless("rhino").
    it("should behave as specified when deleting a non-configurable property - #461 - #679") {
      val obj = js.Dictionary.empty[js.Any]
      js.Object.defineProperty(obj.asInstanceOf[js.Object], "nonconfig",
          js.Dynamic.literal(value = 4, writable = false).asInstanceOf[js.PropertyDescriptor])
      expect(obj("nonconfig")).toEqual(4)
      expect(() => obj.delete("nonconfig")).toThrow
      expect(obj("nonconfig")).toEqual(4)
    }

    it("apply should throw when not found") {
      val obj = js.Dictionary("foo" -> "bar")
      expect(() => obj("bar")).toThrow
    }

    it("should provide `get`") {
      val obj = js.Dictionary.empty[Int]
      obj("hello") = 1

      expect(obj.get("hello") == Some(1)).toBeTruthy
      expect(obj.get("world").isDefined).toBeFalsy
    }

    it("-= should ignore deleting a non-existent key") {
      val obj = js.Dictionary("a" -> "A")
      obj -= "b"
    }

    it("should treat delete as a statement - #907") {
      val obj = js.Dictionary("a" -> "A")
      obj.delete("a")
    }

    it("should provide keys") {
      val obj = js.Dictionary("a" -> "A", "b" -> "B")
      val keys = obj.keys.toList
      expect(keys.size).toEqual(2)
      expect(keys.contains("a")).toBeTruthy
      expect(keys.contains("b")).toBeTruthy
    }

    it("should survive the key 'hasOwnProperty' - #1414") {
      val obj = js.Dictionary.empty[Int]
      expect(obj.contains("hasOwnProperty")).toBeFalsy
      obj("hasOwnProperty") = 5
      expect(obj.contains("hasOwnProperty")).toBeTruthy
      obj.delete("hasOwnProperty")
      expect(obj.contains("hasOwnProperty")).toBeFalsy
    }

    it("should provide an iterator") {
      val obj = js.Dictionary("foo" -> 5, "bar" -> 42, "babar" -> 0)
      var elems: List[(String, Int)] = Nil
      for ((prop, value) <- obj) {
        elems ::= (prop, value)
      }
      expect(elems.size).toEqual(3)
      expect(elems.contains(("foo", 5))).toBeTruthy
      expect(elems.contains(("bar", 42))).toBeTruthy
      expect(elems.contains(("babar", 0))).toBeTruthy
    }

    it("should desugar arguments to delete statements - #908") {
      val kh = js.Dynamic.literal(key = "a").asInstanceOf[KeyHolder]
      val dict = js.Dictionary[String]("a" -> "A")
      def a[T](foo: String): T = dict.asInstanceOf[T]
      a[js.Dictionary[String]]("foo").delete(kh.key)
    }

  }

  @js.native
  trait KeyHolder extends js.Object {
    def key: String = js.native
  }

  describe("scala.scalajs.js.JSConverters.JSRichGenMap") {

    import js.JSConverters._

    it("should provide toJSDictionary") {
      expect(Map("a" -> 1, "b" -> 2).toJSDictionary).toEqual(
          js.Dynamic.literal(a = 1, b = 2))
      expect(Map("a" -> "foo", "b" -> "bar").toJSDictionary).toEqual(
          js.Dynamic.literal(a = "foo", b = "bar"))
    }

  }
}
