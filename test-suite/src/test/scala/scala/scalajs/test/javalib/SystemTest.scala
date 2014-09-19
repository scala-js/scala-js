/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package javalib

import language.implicitConversions

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.test.JasmineTest

object SystemTest extends JasmineTest {

  // Just in here, we allow ourselves to do this
  implicit def array2jsArray[T](arr: Array[T]): js.Array[T] = arr.toJSArray

  describe("java.lang.System") {

    it("should respond to `arraycopy`") {
      val object0 = Array[Any]("[", "b", "c", "d", "e", "f", "]")
      val object1 = Array[Any](() => true, 1, "2", '3', 4.0, true, object0)

      System.arraycopy(object1, 1, object0, 1, 5)
      expect(object0.mkString).toEqual("[1234true]")

      val string0 = Array("a", "b", "c", "d", "e", "f")
      val string1 = Array("1", "2", "3", "4")

      System.arraycopy(string1, 0, string0, 3, 3)
      expect(string0.mkString).toEqual("abc123")

      val ab01Chars = Array("ab".toCharArray, "01".toCharArray)
      val chars = new Array[Array[Char]](32)
      System.arraycopy(ab01Chars, 0, chars, 0, 2)
      for (i <- Seq(0, 2, 4, 8, 16)) {
        System.arraycopy(chars, i / 4, chars, i, i)
      }

      expect(chars.filter(_ == null).length).toEqual(12)
      expect(chars.filter(_ != null).map(_.mkString).mkString).
        toEqual("ab01ab0101ab01ab0101ab0101ab01ab0101ab01")
    }

    it("should respond to `arraycopy` with range overlaps for the same array") {
      val array = new Array[Int](10)

      for (i <- 1 to 6) {
        array(i) = i
      }

      expect(array).toEqual(Array(0, 1, 2, 3, 4, 5, 6, 0, 0, 0))
      System.arraycopy(array, 0, array, 3, 7)
      expect(array).toEqual(Array(0, 1, 2, 0, 1, 2, 3, 4, 5, 6))

      System.arraycopy(array, 0, array, 1, 0)
      expect(array).toEqual(Array(0, 1, 2, 0, 1, 2, 3, 4, 5, 6))

      System.arraycopy(array, 0, array, 1, 9)
      expect(array).toEqual(Array(0, 0, 1, 2, 0, 1, 2, 3, 4, 5))

      System.arraycopy(array, 1, array, 0, 9)
      expect(array).toEqual(Array(0, 1, 2, 0, 1, 2, 3, 4, 5, 5))

      System.arraycopy(array, 0, array, 0, 10)
      expect(array).toEqual(Array(0, 1, 2, 0, 1, 2, 3, 4, 5, 5))

      val reversed = array.reverse
      System.arraycopy(reversed, 5, array, 5, 5)
      expect(array).toEqual(Array(0, 1, 2, 0, 1, 1, 0, 2, 1, 0))
    }

  }
}
