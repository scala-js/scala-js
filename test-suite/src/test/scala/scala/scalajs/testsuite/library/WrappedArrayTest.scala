/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.testsuite.library

import scala.scalajs.js
import org.scalajs.jasminetest.JasmineTest

import scala.collection.mutable

object WrappedArrayTest extends JasmineTest {

  describe("scala.scalajs.js.WrappedArray") {

    // Methods we actually implement

    it("should implement apply") {
      val array = js.Array(3,4,5,6,3,4)
      val seq: Seq[Int] = array

      expect(seq(0)).toEqual(3)
      expect(seq(3)).toEqual(6)

      array(0) = 4
      expect(seq(0)).toEqual(4)
    }

    it("should implement update") {
      val array = js.Array(3,4,5,6,3,4)
      val seq: mutable.Seq[Int] = array

      expect(array(1)).toEqual(4)
      seq(1) = 5
      expect(array(1)).toEqual(5)

      seq(5) = 10
      expect(array(5)).toEqual(10)
    }

    it("should implement length") {
      val array = js.Array(3,4,5,6,3,4)
      val seq: Seq[Int] = array

      expect(seq.length).toEqual(6)
      array.push(1)
      expect(seq.length).toEqual(7)
    }

    it("should implement +=:") {
      val array = js.Array(5, 8, 9)
      3 +=: array
      expect(array).toEqual(js.Array(3, 5, 8, 9))
    }

    it("should implement ++=:") {
      val array = js.Array(5, 8, 9)
      js.Array(2, 0) ++=: array
      expect(array).toEqual(js.Array(2, 0, 5, 8, 9))
      Seq(-3, -45, 1) ++=: array
      expect(array).toEqual(js.Array(-3, -45, 1, 2, 0, 5, 8, 9))
    }

    it("should implement insertAll") {
      val array = js.Array(5, 8, 9)
      array.insertAll(2, js.Array(2, 0))
      expect(array).toEqual(js.Array(5, 8, 2, 0, 9))
      array.insertAll(1, Seq(-3, -45, 1))
      expect(array).toEqual(js.Array(5, -3, -45, 1, 8, 2, 0, 9))
    }

    it("should implement remove") {
      val array = js.Array(5, 8, 2, 0, 9)
      expect(array.remove(1)).toEqual(8)
      expect(array).toEqual(js.Array(5, 2, 0, 9))

      array.remove(0, 3)
      expect(array).toEqual(js.Array(9))
    }

    // Some arbitrary methods to test the builders

    it("should implement collect") {
      // Ascribe to right type here, so we'll actually produce a WrappedArray
      val seq: js.WrappedArray[Int] = js.Array(3,4,5,6,3,4)
      val res = seq.collect {
        case x if x > 4 => 2*x
      }

      expect(res.getClass == classOf[js.WrappedArray[Int]]).toBeTruthy
      expect(res.toList == List(10,12)).toBeTruthy
      expect(res.array).toEqual(js.Array(10,12))
    }

    it("should implement diff") {
      val seq: Seq[Int] = js.Array(1,2,1,3,1,10,9)
      val diff = seq.diff(Seq(1,3,9))
      expect(diff.toList == List(2,1,1,10)).toBeTruthy
    }

    it("should implement toList") {
      val seq: Seq[Int] = js.Array(1,2,1,3,1,10,9)
      val list = seq.toList
      expect(list == List(1,2,1,3,1,10,9)).toBeTruthy
    }

    it("should implement to[T]") {
      val seq: Seq[Int] = js.Array(1,2,1,3,1,10,9)
      val list = seq.to[List]
      expect(list == List(1,2,1,3,1,10,9)).toBeTruthy
    }

  }

}
