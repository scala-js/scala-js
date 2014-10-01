/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.testsuite.javalib

import language.implicitConversions

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

import org.scalajs.jasminetest.JasmineTest

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

    it("should provide identityHashCode") {
      /* This test is more restrictive than the spec, but we know our
       * implementation will always pass the test.
       */
      class HasIDHashCode

      val x1 = new HasIDHashCode
      val x2 = new HasIDHashCode
      val x1FirstHash = x1.hashCode()
      expect(x1.hashCode()).toEqual(x1FirstHash)
      expect(x1.hashCode()).not.toEqual(x2.hashCode())
      expect(x1.hashCode()).toEqual(x1FirstHash)

      expect(System.identityHashCode(x1)).toEqual(x1FirstHash)
      expect(System.identityHashCode(x2)).toEqual(x2.hashCode())
    }

    it("identityHashCode should by-pass .hashCode()") {
      val list1 = List(1, 3, 5)
      val list2 = List(1, 3, 5)
      expect(list1 == list2).toBeTruthy
      expect(list1.hashCode()).toEqual(list2.hashCode())
      expect(System.identityHashCode(list1)).not.toEqual(System.identityHashCode(list2))
    }

    it("identityHashCode(null)") {
      expect(System.identityHashCode(null)).toEqual(0)
    }

    it("identityHashCode of values implemented as JS primitives") {
      expect(System.identityHashCode("foo")).toEqual("foo".hashCode())
      expect(System.identityHashCode("")).toEqual("".hashCode())

      expect(System.identityHashCode(false)).toEqual(false.hashCode())
      expect(System.identityHashCode(true)).toEqual(true.hashCode())

      expect(System.identityHashCode(5)).toEqual(5.hashCode())
      expect(System.identityHashCode(789456)).toEqual(789456.hashCode())
    }

  }
}
