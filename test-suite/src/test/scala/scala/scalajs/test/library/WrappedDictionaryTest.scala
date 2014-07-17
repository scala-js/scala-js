/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package library

import scala.scalajs.js
import scala.scalajs.test.JasmineTest

import scala.collection.mutable

import scala.reflect.ClassTag

object WrappedDictionaryTest extends JasmineTest {

  describe("scala.scalajs.js.WrappedDictionary") {

    // Methods we actually implement

    it("should implement get") {
      val map: mutable.Map[String, Any] =
        js.Dictionary("a" -> "a", "b" -> 6, "e" -> js.undefined)
      expect(map.get("a") == Some("a")).toBeTruthy
      expect(map.get("b") == Some(6)).toBeTruthy
      expect(map.get("e") == Some(())).toBeTruthy
      expect(map.get("f") == None).toBeTruthy
    }

    it("should implement += and -=") {
      val dict = js.Dictionary[String]()
      val map: mutable.Map[String, String] = dict

      expect(js.Object.properties(dict)).toEqual(js.Array())

      map += "hello" -> "world"
      expect(dict("hello")).toEqual("world")
      map += "foo" -> "bar"
      expect(dict("foo")).toEqual("bar")
      map -= "hello"
      expect(dict.get("hello").isDefined).toBeFalsy
      expect(js.Object.properties(dict)).toEqual(js.Array("foo"))
    }

    it("should implement iterator") {
      val elems = ('a' to 'e').map(_.toString).zip(1 to 5)
      val dict = js.Dictionary[Int]()
      val map: mutable.Map[String, Int] = dict

      dict ++= elems

      expect(map.iterator.toList.sorted.sameElements(elems)).toBeTruthy
    }

    // Some arbitrary methods to test the builders

    it("should implement map") {
      def ct[A : ClassTag](x: A) = implicitly[ClassTag[A]]
      val dict = js.Dictionary[Int]()
      dict ++= Seq("one" -> 1, "two" -> 2, "three" -> 3)

      val mapChr = dict.map { case (k,v) => k(0)          -> v * 2 }
      val mapStr = dict.map { case (k,v) => k(0).toString -> v * 2 }

      expect(ct(mapChr).runtimeClass == classOf[js.WrappedDictionary[_]]).toBeFalsy
      expect(ct(mapStr).runtimeClass == classOf[js.WrappedDictionary[_]]).toBeTruthy

      expect(mapChr.size).toBe(2)
      expect(mapStr.size).toBe(2)
    }

    it("should implement withFilter") {
      val dict = js.Dictionary[Int]()
      val flt = dict.withFilter { case (k,v) => v > 5 || k == "a" }
      def size = flt.map(x => x).size

      expect(size).toBe(0)
      dict += "a" -> 1
      expect(size).toBe(1)
      dict += "b" -> 2
      expect(size).toBe(1)
      dict += "c" -> 6
      expect(size).toBe(2)
      dict += "b" -> 7
      expect(size).toBe(3)
      dict -= "a"
      expect(size).toBe(2)
    }

    it("should implement toList") {
      val dict = js.Dictionary("a" -> "a", "b" -> 6, "e" -> js.undefined)
      val list = dict.toList
      expect(list.size).toBe(3)
    }

    it("should implement to[T]") {
      val dict = js.Dictionary("a" -> "a", "b" -> 6, "e" -> js.undefined)
      val list = dict.to[List]
      expect(list.size).toBe(3)
    }

  }

}
