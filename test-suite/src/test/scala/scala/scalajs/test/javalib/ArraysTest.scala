/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package javalib

import scala.scalajs.js
import scala.scalajs.test.JasmineTest
import java.util.{ Arrays, Comparator }

import scala.reflect.ClassTag

object ArraysTest extends ArraysTest

/** This is also used in the typedarray package to test scala.Arrays backed
 *  by TypedArrays
 */
trait ArraysTest extends JasmineTest {

  /** Overridden by typedarray tests */
  def Array[T : ClassTag](v: T*): scala.Array[T] = scala.Array(v: _*)

  /** Overridden by typedarray tests */
  def testBody(suite: => Unit) = describe("java.util.Arrays")(suite)

  val stringComparator = new Comparator[String]() {
    def compare(s1: String, s2: String) = s1.compareTo(s2)
  }

  val intComparator = new Comparator[Int]() {
    def compare(i1: Int, i2: Int) = i1 - i2
  }

  testBody {

    it("should respond to `sort` for Int") {
      val scalaInts = Array(5, 3, 6, 1, 2, 4)
      val ints = new Array[Object](scalaInts.length)
      for (i <- 0 until scalaInts.length)
        ints(i) = scalaInts(i).asInstanceOf[Object]
      val sorted = Array(1, 2, 3, 4, 5, 6)

      Arrays.sort(ints, intComparator.asInstanceOf[Comparator[Object]])
      expect(ints).toEqual(Array(1, 2, 3, 4, 5, 6))
    }

    it("should respond to `sort` for String") {
      val scalajs: Array[Object] = Array("S", "c", "a", "l", "a", ".", "j", "s")
      val sorted = Array(".", "S", "a", "a", "c", "j", "l", "s")

      Arrays.sort(scalajs, stringComparator.asInstanceOf[Comparator[Object]])
      expect(scalajs).toEqual(sorted)
    }

    it("should respond to `fill` for Boolean") {
      val booleans = new Array[Boolean](6)
      Arrays.fill(booleans, false)
      expect(booleans).toEqual(Array(false, false, false, false, false, false))

      Arrays.fill(booleans, true)
      expect(booleans).toEqual(Array(true, true, true, true, true, true))
    }

    it("should respond to `fill` with start and end index for Boolean") {
      val booleans = new Array[Boolean](6)
      Arrays.fill(booleans, 1, 4, true)
      expect(booleans).toEqual(Array(false, true, true, true, false, false))
    }

    it("should respond to `fill` for Byte") {
      val bytes = new Array[Byte](6)
      Arrays.fill(bytes, 42.toByte)
      expect(bytes).toEqual(Array[Byte](42, 42, 42, 42, 42, 42))

      Arrays.fill(bytes, -1.toByte)
      expect(bytes).toEqual(Array[Byte](-1, -1, -1, -1, -1, -1))
    }

    it("should respond to `fill` with start and end index for Byte") {
      val bytes = new Array[Byte](6)
      Arrays.fill(bytes, 1, 4, 42.toByte)
      expect(bytes).toEqual(Array[Byte](0, 42, 42, 42, 0, 0))

      Arrays.fill(bytes, 2, 5, -1.toByte)
      expect(bytes).toEqual(Array[Byte](0, 42, -1, -1, -1, 0))
    }

    it("should respond to `fill` for Short") {
      val shorts = new Array[Short](6)
      Arrays.fill(shorts, 42.toShort)
      expect(shorts).toEqual(Array[Short](42, 42, 42, 42, 42, 42))

      Arrays.fill(shorts, -1.toShort)
      expect(shorts).toEqual(Array[Short](-1, -1, -1, -1, -1, -1))
    }

    it("should respond to `fill` with start and end index for Short") {
      val shorts = new Array[Short](6)
      Arrays.fill(shorts, 1, 4, 42.toShort)
      expect(shorts).toEqual(Array[Short](0, 42, 42, 42, 0, 0))

      Arrays.fill(shorts, 2, 5, -1.toShort)
      expect(shorts).toEqual(Array[Short](0, 42, -1, -1, -1, 0))
    }

    it("should respond to `fill` for Int") {
      val ints = new Array[Int](6)
      Arrays.fill(ints, 42)
      expect(ints).toEqual(Array(42, 42, 42, 42, 42, 42))

      Arrays.fill(ints, -1)
      expect(ints).toEqual(Array(-1, -1, -1, -1, -1, -1))
    }

    it("should respond to `fill` with start and end index for Int") {
      val ints = new Array[Int](6)
      Arrays.fill(ints, 1, 4, 42)
      expect(ints).toEqual(Array(0, 42, 42, 42, 0, 0))

      Arrays.fill(ints, 2, 5, -1)
      expect(ints).toEqual(Array(0, 42, -1, -1, -1, 0))
    }

    it("should respond to `fill` for Long") {
      val longs = new Array[Long](6)
      Arrays.fill(longs, 42L)
      expect(longs).toEqual(Array(42L, 42L, 42L, 42L, 42L, 42L))

      Arrays.fill(longs, -1L)
      expect(longs).toEqual(Array(-1L, -1L, -1L, -1L, -1L, -1L))
    }

    it("should respond to `fill` with start and end index for Long") {
      val longs = new Array[Long](6)
      Arrays.fill(longs, 1, 4, 42L)
      expect(longs).toEqual(Array(0L, 42L, 42L, 42L, 0L, 0L))

      Arrays.fill(longs, 2, 5, -1L)
      expect(longs).toEqual(Array(0L, 42L, -1L, -1L, -1L, 0L))
    }

    it("should respond to `fill` for Float") {
      val floats = new Array[Float](6)
      Arrays.fill(floats, 42.0f)
      expect(floats).toEqual(Array(42.0f, 42.0f, 42.0f, 42.0f, 42.0f, 42.0f))

      Arrays.fill(floats, -1.0f)
      expect(floats).toEqual(Array(-1.0f, -1.0f, -1.0f, -1.0f, -1.0f, -1.0f))
    }

    it("should respond to `fill` with start and end index for Float") {
      val floats = new Array[Float](6)
      Arrays.fill(floats, 1, 4, 42.0f)
      expect(floats).toEqual(Array(0.0f, 42.0f, 42.0f, 42.0f, 0.0f, 0.0f))

      Arrays.fill(floats, 2, 5, -1.0f)
      expect(floats).toEqual(Array(0.0f, 42.0f, -1.0f, -1.0f, -1.0f, 0.0f))
    }

    it("should respond to `fill` for Double") {
      val doubles = new Array[Double](6)
      Arrays.fill(doubles, 42.0)
      expect(doubles).toEqual(Array(42.0, 42.0, 42.0, 42.0, 42.0, 42.0))

      Arrays.fill(doubles, -1.0f)
      expect(doubles).toEqual(Array(-1.0, -1.0, -1.0, -1.0, -1.0, -1.0))
    }

    it("should respond to `fill` with start and end index for Double") {
      val doubles = new Array[Double](6)
      Arrays.fill(doubles, 1, 4, 42.0)
      expect(doubles).toEqual(Array(0.0, 42.0, 42.0, 42.0, 0.0, 0.0))

      Arrays.fill(doubles, 2, 5, -1.0)
      expect(doubles).toEqual(Array(0.0, 42.0, -1.0, -1.0, -1.0, 0.0))
    }

    it("should respond to `fill` for AnyRef") {
      val array = new Array[AnyRef](6)
      Arrays.fill(array, "a")
      expect(array).toEqual(Array[AnyRef]("a", "a", "a", "a", "a", "a"))

      Arrays.fill(array, "b")
      expect(array).toEqual(Array[AnyRef]("b", "b", "b", "b", "b", "b"))
    }

    it("should respond to `fill` with start and end index for AnyRef") {
      val bytes = new Array[AnyRef](6)
      Arrays.fill(bytes, 1, 4, "a")
      expect(bytes).toEqual(Array[AnyRef](null, "a", "a", "a", null, null))

      Arrays.fill(bytes, 2, 5, "b")
      expect(bytes).toEqual(Array[AnyRef](null, "a", "b", "b", "b", null))
    }

    it("should respond to `binarySearch` with start index, end index and key for Long") {
      val longs: Array[Long] = Array(1, 2, 3, 5, 6, 7)
      var ret = Arrays.binarySearch(longs, 0, 6, 5)
      expect(ret).toEqual(3)

      ret = Arrays.binarySearch(longs, 0, 6, 0)
      expect(ret).toEqual(-1)

      ret = Arrays.binarySearch(longs, 0, 6, 4)
      expect(ret).toEqual(-4)

      ret = Arrays.binarySearch(longs, 0, 6, 8)
      expect(ret).toEqual(-7)
    }

    it("should respond to `binarySearch` with key for Long") {
      val longs: Array[Long] = Array(1, 2, 3, 5, 6, 7)
      var ret = Arrays.binarySearch(longs, 5)
      expect(ret).toEqual(3)

      ret = Arrays.binarySearch(longs, 0)
      expect(ret).toEqual(-1)

      ret = Arrays.binarySearch(longs, 4)
      expect(ret).toEqual(-4)

      ret = Arrays.binarySearch(longs, 8)
      expect(ret).toEqual(-7)
    }

    it("should respond to `binarySearch` with start index, end index and key for Int") {
      val ints: Array[Int] = Array(1, 2, 3, 5, 6, 7)
      var ret = Arrays.binarySearch(ints, 0, 6, 5)
      expect(ret).toEqual(3)

      ret = Arrays.binarySearch(ints, 0, 6, 0)
      expect(ret).toEqual(-1)

      ret = Arrays.binarySearch(ints, 0, 6, 4)
      expect(ret).toEqual(-4)

      ret = Arrays.binarySearch(ints, 0, 6, 8)
      expect(ret).toEqual(-7)
    }

    it("should respond to `binarySearch` with key for Int") {
      val ints: Array[Int] = Array(1, 2, 3, 5, 6, 7)
      var ret = Arrays.binarySearch(ints, 5)
      expect(ret).toEqual(3)

      ret = Arrays.binarySearch(ints, 0)
      expect(ret).toEqual(-1)

      ret = Arrays.binarySearch(ints, 4)
      expect(ret).toEqual(-4)

      ret = Arrays.binarySearch(ints, 8)
      expect(ret).toEqual(-7)
    }

    it("should respond to `binarySearch` with start index, end index and key for Short") {
      val shorts: Array[Short] = Array(1, 2, 3, 5, 6, 7)
      var ret = Arrays.binarySearch(shorts, 0, 6, 5.toShort)
      expect(ret).toEqual(3)

      ret = Arrays.binarySearch(shorts, 0, 6, 0.toShort)
      expect(ret).toEqual(-1)

      ret = Arrays.binarySearch(shorts, 0, 6, 4.toShort)
      expect(ret).toEqual(-4)

      ret = Arrays.binarySearch(shorts, 0, 6, 8.toShort)
      expect(ret).toEqual(-7)
    }

    it("should respond to `binarySearch` with key for Short") {
      val shorts: Array[Short] = Array(1, 2, 3, 5, 6, 7)
      var ret = Arrays.binarySearch(shorts, 5.toShort)
      expect(ret).toEqual(3)

      ret = Arrays.binarySearch(shorts, 0.toShort)
      expect(ret).toEqual(-1)

      ret = Arrays.binarySearch(shorts, 4.toShort)
      expect(ret).toEqual(-4)

      ret = Arrays.binarySearch(shorts, 8.toShort)
      expect(ret).toEqual(-7)
    }

    it("should respond to `binarySearch` with start index, end index and key for Char") {
      val chars: Array[Char] = Array('b', 'c', 'd', 'f', 'g', 'h')
      var ret = Arrays.binarySearch(chars, 0, 6, 'f')
      expect(ret).toEqual(3)

      ret = Arrays.binarySearch(chars, 0, 6, 'a')
      expect(ret).toEqual(-1)

      ret = Arrays.binarySearch(chars, 0, 6, 'e')
      expect(ret).toEqual(-4)

      ret = Arrays.binarySearch(chars, 0, 6, 'i')
      expect(ret).toEqual(-7)
    }

    it("should respond to `binarySearch` with key for Char") {
      val chars: Array[Char] = Array('b', 'c', 'd', 'f', 'g', 'h')
      var ret = Arrays.binarySearch(chars, 'f')
      expect(ret).toEqual(3)

      ret = Arrays.binarySearch(chars, 'a')
      expect(ret).toEqual(-1)

      ret = Arrays.binarySearch(chars, 'e')
      expect(ret).toEqual(-4)

      ret = Arrays.binarySearch(chars, 'i')
      expect(ret).toEqual(-7)
    }

    it("should respond to `binarySearch` with start index, end index and key for Double") {
      val doubles: Array[Double] = Array(0.1, 0.2, 0.3, 0.5, 0.6, 0.7)
      var ret = Arrays.binarySearch(doubles, 0, 6, 0.5)
      expect(ret).toEqual(3)

      ret = Arrays.binarySearch(doubles, 0, 6, 0.0)
      expect(ret).toEqual(-1)

      ret = Arrays.binarySearch(doubles, 0, 6, 0.4)
      expect(ret).toEqual(-4)

      ret = Arrays.binarySearch(doubles, 0, 6, 0.8)
      expect(ret).toEqual(-7)
    }

    it("should respond to `binarySearch` with key for Double") {
      val doubles: Array[Double] = Array(0.1, 0.2, 0.3, 0.5, 0.6, 0.7)
      var ret = Arrays.binarySearch(doubles, 0.5)
      expect(ret).toEqual(3)

      ret = Arrays.binarySearch(doubles, 0.0)
      expect(ret).toEqual(-1)

      ret = Arrays.binarySearch(doubles, 0.4)
      expect(ret).toEqual(-4)

      ret = Arrays.binarySearch(doubles, 0.8)
      expect(ret).toEqual(-7)
    }

    it("should respond to `binarySearch` with start index, end index and key for Float") {
      val floats: Array[Float] = Array(0.1f, 0.2f, 0.3f, 0.5f, 0.6f, 0.7f)
      var ret = Arrays.binarySearch(floats, 0, 6, 0.5f)
      expect(ret).toEqual(3)

      ret = Arrays.binarySearch(floats, 0, 6, 0.0f)
      expect(ret).toEqual(-1)

      ret = Arrays.binarySearch(floats, 0, 6, 0.4f)
      expect(ret).toEqual(-4)

      ret = Arrays.binarySearch(floats, 0, 6, 0.8f)
      expect(ret).toEqual(-7)
    }

    it("should respond to `binarySearch` with key for Float") {
      val floats: Array[Float] = Array(0.1f, 0.2f, 0.3f, 0.5f, 0.6f, 0.7f)
      var ret = Arrays.binarySearch(floats, 0.5f)
      expect(ret).toEqual(3)

      ret = Arrays.binarySearch(floats, 0.0f)
      expect(ret).toEqual(-1)

      ret = Arrays.binarySearch(floats, 0.4f)
      expect(ret).toEqual(-4)

      ret = Arrays.binarySearch(floats, 0.8f)
      expect(ret).toEqual(-7)
    }

    it("should respond to `binarySearch` with start index, end index and key for AnyRef") {
      val strings: Array[AnyRef] = Array("aa", "abc", "cc", "zz", "zzzs", "zzzt")
      var ret = Arrays.binarySearch(strings, 0, 6, "zz")
      expect(ret).toEqual(3)

      ret = Arrays.binarySearch(strings, 0, 6, "a")
      expect(ret).toEqual(-1)

      ret = Arrays.binarySearch(strings, 0, 6, "cd")
      expect(ret).toEqual(-4)

      ret = Arrays.binarySearch(strings, 0, 6, "zzzz")
      expect(ret).toEqual(-7)
    }

    it("should respond to `binarySearch` with key for AnyRef") {
      val strings: Array[AnyRef] = Array("aa", "abc", "cc", "zz", "zzzs", "zzzt")
      var ret = Arrays.binarySearch(strings, "zz")
      expect(ret).toEqual(3)

      ret = Arrays.binarySearch(strings, "a")
      expect(ret).toEqual(-1)

      ret = Arrays.binarySearch(strings, "cd")
      expect(ret).toEqual(-4)

      ret = Arrays.binarySearch(strings, "zzzz")
      expect(ret).toEqual(-7)
    }

    it("should check ranges of input to `binarySearch`") {
      def expectException(block: => Unit)(expected: PartialFunction[Throwable, Unit]): Unit = {
        val catchAll: PartialFunction[Throwable, Unit] = {
          case e: Throwable => expect(e.getClass.getName).toBe("not thrown")
        }

        try {
          block
          expect("exception").toBe("thrown")
        } catch expected orElse catchAll
      }

      val array = Array(0, 1, 3, 4)

      expectException({ Arrays.binarySearch(array, 3, 2, 2) }) {
        case exception: IllegalArgumentException =>
          expect(exception.getMessage).toBe("fromIndex(3) > toIndex(2)")
      }

      // start/end comparison is made before index ranges checks
      expectException({ Arrays.binarySearch(array, 7, 5, 2) }) {
        case exception: IllegalArgumentException =>
          expect(exception.getMessage).toBe("fromIndex(7) > toIndex(5)")
      }

      expectException({ Arrays.binarySearch(array, -1, 4, 2) }) {
        case exception: ArrayIndexOutOfBoundsException =>
          expect(exception.getMessage).toBe("Array index out of range: -1")
      }

      expectException({ Arrays.binarySearch(array, 0, 5, 2) }) {
        case exception: ArrayIndexOutOfBoundsException =>
          expect(exception.getMessage).toBe("Array index out of range: 5")
      }
    }

    it("should respond to `copyOf` with key for Int") {
      val ints: Array[Int] = Array(1, 2, 3)
      val intscopy = Arrays.copyOf(ints, 5)
      expect(intscopy).toEqual(Array(1, 2, 3, 0, 0))
    }

    it("should respond to `copyOf` with key for Long") {
      val longs: Array[Long] = Array(1, 2, 3)
      val longscopy = Arrays.copyOf(longs, 5)
      expect(longscopy).toEqual(Array[Long](1, 2, 3, 0, 0))
    }

    it("should respond to `copyOf` with key for Short") {
      val shorts: Array[Short] = Array(1, 2, 3)
      val shortscopy = Arrays.copyOf(shorts, 5)
      expect(shortscopy).toEqual(Array[Short](1, 2, 3, 0, 0))
    }

    it("should respond to `copyOf` with key for Byte") {
      val bytes: Array[Byte] = Array(42, 43, 44)
      val floatscopy = Arrays.copyOf(bytes, 5)
      expect(floatscopy).toEqual(Array[Byte](42, 43, 44, 0, 0))
    }

    it("should respond to `copyOf` with key for Char") {
      val chars: Array[Char] = Array('a', 'b', '0')
      val charscopy = Arrays.copyOf(chars, 5)
      expect(charscopy(4)).toEqual(0.toChar)
    }

    it("should respond to `copyOf` with key for Double") {
      val doubles: Array[Double] = Array(0.1, 0.2, 0.3)
      val doublescopy = Arrays.copyOf(doubles, 5)
      expect(doublescopy).toEqual(Array[Double](0.1, 0.2, 0.3, 0, 0))
    }

    it("should respond to `copyOf` with key for Float") {
      val floats: Array[Float] = Array(0.1f, 0.2f, 0.3f)
      val floatscopy = Arrays.copyOf(floats, 5)
      expect(floatscopy).toEqual(Array[Float](0.1f, 0.2f, 0.3f, 0f, 0f))
    }

    it("should respond to `copyOf` with key for Boolean") {
      val bools: Array[Boolean] = Array(false, true, false)
      val boolscopy = Arrays.copyOf(bools, 5)
      expect(boolscopy).toEqual(Array[Boolean](false, true, false, false, false))
    }

    it("should respond to `copyOf` with key for AnyRef") {
      val anyrefs: Array[AnyRef] = Array("a", "b", "c")
      val anyrefscopy = Arrays.copyOf(anyrefs, 5)
      expect(anyrefscopy).toEqual(Array[AnyRef]("a", "b", "c", null, null))
    }

  }

}
