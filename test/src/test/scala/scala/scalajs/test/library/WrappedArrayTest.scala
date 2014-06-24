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
      expect(diff.toList == List(2,1,1,10))
    }

  }

}
