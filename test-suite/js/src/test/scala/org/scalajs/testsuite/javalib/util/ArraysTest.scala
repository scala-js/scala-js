/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import language.implicitConversions

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

import org.scalajs.jasminetest.JasmineTest

import java.util.{ Arrays, Comparator }

import scala.reflect.ClassTag

object ArraysTest extends ArraysTest

/** This is also used in the typedarray package to test scala.Arrays backed
 *  by TypedArrays
 */
trait ArraysTest extends JasmineTest {

  // Just in here, we allow ourselves to do this
  implicit def array2jsArray[T](arr: Array[T]): js.Array[T] = arr.toJSArray

  /** Overridden by typedarray tests */
  def Array[T: ClassTag](v: T*): scala.Array[T] = scala.Array(v: _*)

  /** Overridden by typedarray tests */
  def testBody(suite: => Unit): Unit = describe("java.util.Arrays")(suite)

  val stringComparator = new Comparator[String]() {
    def compare(s1: String, s2: String): Int = s1.compareTo(s2)
  }

  testBody {

    def testSort[T](typeName: String,  elem: Int => T, newArray: Int => Array[T],
          sort: Array[T] => Unit, sort2: (Array[T], Int, Int) => Unit): Unit = {
      it(s"should respond to `sort` for $typeName") {
        val values = Array(5, 3, 6, 1, 2, 4).map(elem)
        val arr = newArray(values.length)

        for (i <- 0 until values.length)
          arr(i) = values(i)
        sort(arr)
        for ((e, i) <- Array(1, 2, 3, 4, 5, 6).map(elem).zipWithIndex)
          expect(arr(i) == e).toBeTruthy

        for (i <- 0 until values.length)
          arr(i) = values(i)
        sort2(arr, 0, 3)
        for ((e, i) <- Array(3, 5, 6, 1, 2, 4).map(elem).zipWithIndex)
          expect(arr(i) == e).toBeTruthy

        sort2(arr, 2, 5)
        for ((e, i) <- Array(3, 5, 1, 2, 6, 4).map(elem).zipWithIndex)
          expect(arr(i) == e).toBeTruthy

        sort2(arr, 0, 6)
        for ((e, i) <- Array(1, 2, 3, 4, 5, 6).map(elem).zipWithIndex)
          expect(arr(i) == e).toBeTruthy
      }
    }
    testSort[Int]("Int", _.toInt, new Array(_), Arrays.sort(_), Arrays.sort(_, _, _))
    testSort[Long]("Long", _.toLong, new Array(_), Arrays.sort(_), Arrays.sort(_, _, _))
    testSort[Short]("Short", _.toShort, new Array(_), Arrays.sort(_), Arrays.sort(_, _, _))
    testSort[Byte]("Byte", _.toByte, new Array(_), Arrays.sort(_), Arrays.sort(_, _, _))
    testSort[Char]("Char", _.toChar, new Array(_), Arrays.sort(_), Arrays.sort(_, _, _))
    testSort[Float]("Float", _.toFloat, new Array(_), Arrays.sort(_), Arrays.sort(_, _, _))
    testSort[Double]("Double", _.toDouble, new Array(_), Arrays.sort(_), Arrays.sort(_, _, _))
    testSort[AnyRef]("String", _.toString, new Array(_), Arrays.sort(_), Arrays.sort(_, _, _))

    it("should respond to `sort` with comparator") {
      val scalajs: Array[String] = Array("S", "c", "a", "l", "a", ".", "j", "s")
      val sorted = Array[String](".", "S", "a", "a", "c", "j", "l", "s")

      Arrays.sort(scalajs, stringComparator)
      expect(scalajs).toEqual(sorted)
    }

    it("should have a `sort` that is stable") {
      case class A(n: Int)
      val cmp = new Comparator[A]() {
        def compare(a1: A, a2: A): Int = a1.n.compareTo(a2.n)
      }
      val scalajs: Array[A] = Array(A(1), A(2), A(2), A(3), A(1), A(2), A(3))
      val sorted = Array[A](scalajs(0), scalajs(4), scalajs(1), scalajs(2),
          scalajs(5), scalajs(3), scalajs(6))

      Arrays.sort(scalajs, cmp)
      expect(scalajs).toEqual(sorted)
      scalajs.zip(sorted).forall(pair => pair ._1 eq pair._2)
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
      expect(anyrefscopy.getClass() == classOf[Array[AnyRef]]).toBeTruthy
      expect(anyrefscopy).toEqual(Array[AnyRef]("a", "b", "c", null, null))

      val sequences: Array[CharSequence] = Array("a", "b", "c")
      val sequencescopy = Arrays.copyOf(sequences, 2)
      expect(sequencescopy.getClass() == classOf[Array[CharSequence]])
      expect(sequencescopy).toEqual(Array[CharSequence]("a", "b"))
    }

    it("should respond to `copyOf` with key for AnyRef with change of type") {
      class A
      case class B(x: Int) extends A

      val bs: Array[AnyRef] = Array(B(1), B(2), B(3))
      val bscopyAsA = Arrays.copyOf(bs, 5, classOf[Array[A]])
      expect(bscopyAsA.getClass() == classOf[Array[A]]).toBeTruthy
      expect(bscopyAsA).toEqual(Array[A](B(1), B(2), B(3), null, null))
    }

    it("should respond to `copyOfRange` for AnyRef") {
      val anyrefs: Array[AnyRef] = Array("a", "b", "c", "d", "e")
      val anyrefscopy = Arrays.copyOfRange(anyrefs, 2, 4)
      expect(anyrefscopy.getClass() == classOf[Array[AnyRef]]).toBeTruthy
      expect(anyrefscopy).toEqual(Array[AnyRef]("c", "d"))

      val sequences: Array[CharSequence] = Array("a", "b", "c", "d", "e")
      val sequencescopy = Arrays.copyOfRange(sequences, 1, 5)
      expect(sequencescopy.getClass() == classOf[Array[CharSequence]])
      expect(sequencescopy).toEqual(Array[CharSequence]("b", "c", "d", "e"))
    }

    it("should respond to `copyOfRange` for AnyRef with change of type") {
      class A
      case class B(x: Int) extends A
      val bs: Array[B] = Array(B(1), B(2), B(3), B(4), B(5))
      val bscopyAsA = Arrays.copyOfRange(bs, 2, 4, classOf[Array[A]])
      expect(bscopyAsA.getClass() == classOf[Array[A]]).toBeTruthy
      expect(bscopyAsA).toEqual(Array[A](B(3), B(4)))
    }

    it("should respond to `hashCode` for Boolean") {
      expect(Arrays.hashCode(null: Array[Boolean])).toEqual(0)
      expect(Arrays.hashCode(Array[Boolean]())).toEqual(1)
      expect(Arrays.hashCode(Array[Boolean](false))).toEqual(1268)
      expect(Arrays.hashCode(Array[Boolean](true, false))).toEqual(40359)
    }

    it("should respond to `hashCode` for Chars") {
      expect(Arrays.hashCode(null: Array[Char])).toEqual(0)
      expect(Arrays.hashCode(Array[Char]())).toEqual(1)
      expect(Arrays.hashCode(Array[Char]('a'))).toEqual(128)
      expect(Arrays.hashCode(Array[Char]('c', '&'))).toEqual(4068)
      expect(Arrays.hashCode(Array[Char]('-', '5', 'q'))).toEqual(74792)
      expect(Arrays.hashCode(Array[Char]('.', ' ', '\u4323', 'v', '~'))).toEqual(88584920)
    }

    it("should respond to `hashCode` for Bytes") {
      expect(Arrays.hashCode(null: Array[Byte])).toEqual(0)
      expect(Arrays.hashCode(Array[Byte]())).toEqual(1)
      expect(Arrays.hashCode(Array[Byte](1))).toEqual(32)
      expect(Arrays.hashCode(Array[Byte](7, -125))).toEqual(1053)
      expect(Arrays.hashCode(Array[Byte](3, 0, 45))).toEqual(32719)
      expect(Arrays.hashCode(Array[Byte](0, 45, 100, 1, 1))).toEqual(30065878)
    }

    it("should respond to `hashCode` for Shorts") {
      expect(Arrays.hashCode(null: Array[Short])).toEqual(0)
      expect(Arrays.hashCode(Array[Short]())).toEqual(1)
      expect(Arrays.hashCode(Array[Short](1))).toEqual(32)
      expect(Arrays.hashCode(Array[Short](7, -125))).toEqual(1053)
      expect(Arrays.hashCode(Array[Short](3, 0, 4534))).toEqual(37208)
      expect(Arrays.hashCode(Array[Short](0, 45, 100, 1, 1))).toEqual(30065878)
    }

    it("should respond to `hashCode` for Ints") {
      expect(Arrays.hashCode(null: Array[Int])).toEqual(0)
      expect(Arrays.hashCode(Array[Int]())).toEqual(1)
      expect(Arrays.hashCode(Array[Int](1))).toEqual(32)
      expect(Arrays.hashCode(Array[Int](7, -125))).toEqual(1053)
      expect(Arrays.hashCode(Array[Int](3, 0, 4534))).toEqual(37208)
      expect(Arrays.hashCode(Array[Int](0, 45, 100, 1, 1, Int.MaxValue))).toEqual(-1215441431)
    }

    it("should respond to `hashCode` for Longs") {
      expect(Arrays.hashCode(null: Array[Long])).toEqual(0)
      expect(Arrays.hashCode(Array[Long]())).toEqual(1)
      expect(Arrays.hashCode(Array[Long](1L))).toEqual(32)
      expect(Arrays.hashCode(Array[Long](7L, -125L))).toEqual(1302)
      expect(Arrays.hashCode(Array[Long](3L, 0L, 4534L))).toEqual(37208)
      expect(Arrays.hashCode(Array[Long](0L, 45L, 100L, 1L, 1L, Int.MaxValue))).toEqual(-1215441431)
      expect(Arrays.hashCode(Array[Long](0L, 34573566354545L, 100L, 1L, 1L, Int.MaxValue))).toEqual(-1952288964)
    }

    it("should respond to `hashCode` for Floats") {
      expect(Arrays.hashCode(null: Array[Float])).toEqual(0)
      expect(Arrays.hashCode(Array[Float]())).toEqual(1)
      expect(Arrays.hashCode(Array[Float](1f))).toEqual(32)
      expect(Arrays.hashCode(Array[Float](7.2f, -125.2f))).toEqual(-2082726591)
      expect(Arrays.hashCode(Array[Float](302.1f, 0.0f, 4534f))).toEqual(-1891539602)
      expect(Arrays.hashCode(Array[Float](0.0f, 45f, -100f, 1.1f, -1f, 3567f))).toEqual(-1591440133)
    }

    it("should respond to `hashCode` for Doubles") {
      expect(Arrays.hashCode(null: Array[Double])).toEqual(0)
      expect(Arrays.hashCode(Array[Double]())).toEqual(1)
      expect(Arrays.hashCode(Array[Double](1.1))).toEqual(-1503133662)
      expect(Arrays.hashCode(Array[Double](7.3, -125.23))).toEqual(-2075734168)
      expect(Arrays.hashCode(Array[Double](3.9, 0.2, 4534.9))).toEqual(-557562564)
      expect(Arrays.hashCode(Array[Double](0.1, 45.1, -100.0, 1.1, 1.7))).toEqual(-1750344582)
      expect(Arrays.hashCode(Array[Double](0.0, 34573566354545.9, 100.2, 1.1, 1.2, Int.MaxValue))).toEqual(-1764602991)
    }

    it("should respond to `hashCode` for AnyRef") {
      expect(Arrays.hashCode(null: Array[AnyRef])).toEqual(0)
      expect(Arrays.hashCode(Array[AnyRef]())).toEqual(1)
      expect(Arrays.hashCode(Array[AnyRef](null, null))).toEqual(961)
      expect(Arrays.hashCode(Array[AnyRef]("a", "b", null))).toEqual(126046)
      expect(Arrays.hashCode(Array[AnyRef](null, "a", "b", null, "fooooo"))).toEqual(-1237252983)
    }

    it("should respond to `deepHashCode`") {
      expect(Arrays.deepHashCode(null: Array[AnyRef])).toEqual(0)
      expect(Arrays.deepHashCode(Array[AnyRef]())).toEqual(1)
      expect(Arrays.deepHashCode(Array[AnyRef](null, null))).toEqual(961)
      expect(Arrays.deepHashCode(Array[AnyRef]("a", "b", null))).toEqual(126046)
      expect(Arrays.deepHashCode(Array[AnyRef](null, "a", "b", null, "fooooo"))).toEqual(-1237252983)
      expect(Arrays.deepHashCode(Array[AnyRef](null, Array[AnyRef]()))).toEqual(962)
      expect(Arrays.deepHashCode(Array[AnyRef](Array[AnyRef](), Array[AnyRef]()))).toEqual(993)
      expect(Arrays.deepHashCode(Array[AnyRef](Array[AnyRef](Array[AnyRef]())))).toEqual(63)
      expect(Arrays.deepHashCode(Array[AnyRef](Array[AnyRef](Array[Int]())))).toEqual(63)
      expect(Arrays.deepHashCode(Array[AnyRef](Array[AnyRef](Array[Double]())))).toEqual(63)
      expect(Arrays.deepHashCode(Array[AnyRef](Array[AnyRef](Array[Int](1))))).toEqual(94)
      expect(Arrays.deepHashCode(Array[AnyRef](Array[AnyRef](Array[AnyRef](1.asInstanceOf[AnyRef]))))).toEqual(94)
    }

    it("should respond to `equals` for Booleans") {
      val a1 = Array(true, false)

      expect(Arrays.equals(a1, a1)).toBeTruthy
      expect(Arrays.equals(a1, Array(true, false))).toBeTruthy

      expect(Arrays.equals(a1, Array(true))).toBeFalsy
      expect(Arrays.equals(a1, Array(false))).toBeFalsy
      expect(Arrays.equals(a1, Array[Boolean]())).toBeFalsy
      expect(Arrays.equals(a1, Array(false, true))).toBeFalsy
      expect(Arrays.equals(a1, Array(false, true, false))).toBeFalsy
    }

    it("should respond to `equals` for Bytes") {
      val a1 = Array[Byte](1, -7, 10)

      expect(Arrays.equals(null: Array[Byte], null: Array[Byte])).toBeTruthy
      expect(Arrays.equals(a1, a1)).toBeTruthy
      expect(Arrays.equals(a1, Array[Byte](1, -7, 10))).toBeTruthy

      expect(Arrays.equals(a1, null)).toBeFalsy
      expect(Arrays.equals(a1, Array[Byte](3))).toBeFalsy
      expect(Arrays.equals(a1, Array[Byte](1))).toBeFalsy
      expect(Arrays.equals(a1, Array[Byte]())).toBeFalsy
      expect(Arrays.equals(a1, Array[Byte](1, -7, 11))).toBeFalsy
      expect(Arrays.equals(a1, Array[Byte](1, -7, 11, 20))).toBeFalsy
    }

    it("should respond to `equals` for Chars") {
      val a1 = Array[Char]('a', '0', '-')

      expect(Arrays.equals(null: Array[Char], null: Array[Char])).toBeTruthy
      expect(Arrays.equals(a1, a1)).toBeTruthy
      expect(Arrays.equals(a1, Array[Char]('a', '0', '-'))).toBeTruthy

      expect(Arrays.equals(a1, null)).toBeFalsy
      expect(Arrays.equals(a1, Array[Char]('z'))).toBeFalsy
      expect(Arrays.equals(a1, Array[Char]('a'))).toBeFalsy
      expect(Arrays.equals(a1, Array[Char]())).toBeFalsy
      expect(Arrays.equals(a1, Array[Char]('a', '0', '+'))).toBeFalsy
      expect(Arrays.equals(a1, Array[Char]('a', '0', '-', 'z'))).toBeFalsy
    }

    it("should respond to `equals` for Shorts") {
      val a1 = Array[Short](1, -7, 10)

      expect(Arrays.equals(null: Array[Short], null: Array[Short])).toBeTruthy
      expect(Arrays.equals(a1, a1)).toBeTruthy
      expect(Arrays.equals(a1, Array[Short](1, -7, 10))).toBeTruthy

      expect(Arrays.equals(a1, null)).toBeFalsy
      expect(Arrays.equals(a1, Array[Short](3))).toBeFalsy
      expect(Arrays.equals(a1, Array[Short](1))).toBeFalsy
      expect(Arrays.equals(a1, Array[Short]())).toBeFalsy
      expect(Arrays.equals(a1, Array[Short](1, -7, 11))).toBeFalsy
      expect(Arrays.equals(a1, Array[Short](1, -7, 11, 20))).toBeFalsy
    }

    it("should respond to `equals` for Ints") {
      val a1 = Array[Int](1, -7, 10)

      expect(Arrays.equals(null: Array[Int], null: Array[Int])).toBeTruthy
      expect(Arrays.equals(a1, a1)).toBeTruthy
      expect(Arrays.equals(a1, Array[Int](1, -7, 10))).toBeTruthy

      expect(Arrays.equals(a1, null)).toBeFalsy
      expect(Arrays.equals(a1, Array[Int](3))).toBeFalsy
      expect(Arrays.equals(a1, Array[Int](1))).toBeFalsy
      expect(Arrays.equals(a1, Array[Int]())).toBeFalsy
      expect(Arrays.equals(a1, Array[Int](1, -7, 11))).toBeFalsy
      expect(Arrays.equals(a1, Array[Int](1, -7, 11, 20))).toBeFalsy
    }

    it("should respond to `equals` for Longs") {
      val a1 = Array[Long](1L, -7L, 10L)

      expect(Arrays.equals(null: Array[Long], null: Array[Long])).toBeTruthy
      expect(Arrays.equals(a1, a1)).toBeTruthy
      expect(Arrays.equals(a1, Array[Long](1L, -7L, 10L))).toBeTruthy

      expect(Arrays.equals(a1, null)).toBeFalsy
      expect(Arrays.equals(a1, Array[Long](3L))).toBeFalsy
      expect(Arrays.equals(a1, Array[Long](1L))).toBeFalsy
      expect(Arrays.equals(a1, Array[Long]())).toBeFalsy
      expect(Arrays.equals(a1, Array[Long](1L, -7L, 11L))).toBeFalsy
      expect(Arrays.equals(a1, Array[Long](1L, -7L, 11L, 20L))).toBeFalsy
    }

    it("should respond to `equals` for Floats") {
      val a1 = Array[Float](1.1f, -7.4f, 10.0f)

      expect(Arrays.equals(null: Array[Float], null: Array[Float])).toBeTruthy
      expect(Arrays.equals(a1, a1)).toBeTruthy
      expect(Arrays.equals(a1, Array[Float](1.1f, -7.4f, 10.0f))).toBeTruthy

      expect(Arrays.equals(a1, null)).toBeFalsy
      expect(Arrays.equals(a1, Array[Float](3.0f))).toBeFalsy
      expect(Arrays.equals(a1, Array[Float](1.1f))).toBeFalsy
      expect(Arrays.equals(a1, Array[Float]())).toBeFalsy
      expect(Arrays.equals(a1, Array[Float](1.1f, -7.4f, 11.0f))).toBeFalsy
      expect(Arrays.equals(a1, Array[Float](1.1f, -7.4f, 10.0f, 20.0f))).toBeFalsy
    }

    it("should respond to `equals` for Doubles") {
      val a1 = Array[Double](1.1, -7.4, 10.0)

      expect(Arrays.equals(null: Array[Double], null: Array[Double])).toBeTruthy
      expect(Arrays.equals(a1, a1)).toBeTruthy
      expect(Arrays.equals(a1, Array[Double](1.1, -7.4, 10.0))).toBeTruthy

      expect(Arrays.equals(a1, null)).toBeFalsy
      expect(Arrays.equals(a1, Array[Double](3.0))).toBeFalsy
      expect(Arrays.equals(a1, Array[Double](1.1))).toBeFalsy
      expect(Arrays.equals(a1, Array[Double]())).toBeFalsy
      expect(Arrays.equals(a1, Array[Double](1.1, -7.4, 11.0))).toBeFalsy
      expect(Arrays.equals(a1, Array[Double](1.1, -7.4, 10.0, 20.0))).toBeFalsy
    }

    it("should respond to `equals` for AnyRefs") {
      // scalastyle:off equals.hash.code
      class A(private val x: Int) {
        override def equals(that: Any): Boolean = that match {
          case that: A => this.x == that.x
          case _ => false
        }
      }
      // scalastyle:on equals.hash.code

      def A(x: Int): A = new A(x)

      val a1 = Array[AnyRef](A(1), A(-7), A(10))

      expect(Arrays.equals(null: Array[AnyRef], null: Array[AnyRef])).toBeTruthy
      expect(Arrays.equals(a1, a1)).toBeTruthy
      expect(Arrays.equals(a1, Array[AnyRef](A(1), A(-7), A(10)))).toBeTruthy

      expect(Arrays.equals(a1, null)).toBeFalsy
      expect(Arrays.equals(a1, Array[AnyRef](A(3)))).toBeFalsy
      expect(Arrays.equals(a1, Array[AnyRef](A(1)))).toBeFalsy
      expect(Arrays.equals(a1, Array[AnyRef]())).toBeFalsy
      expect(Arrays.equals(a1, Array[AnyRef](A(1), null, A(11)))).toBeFalsy
      expect(Arrays.equals(a1, Array[AnyRef](A(1), A(-7), A(11), A(20)))).toBeFalsy
    }

    it("should respond to `deepEquals`") {
      expect(Arrays.deepEquals(
          null: Array[AnyRef],
          null: Array[AnyRef])).toBeTruthy
      expect(Arrays.deepEquals(
          Array[AnyRef](),
          Array[AnyRef]())).toBeTruthy
      expect(Arrays.deepEquals(
          Array[AnyRef](null, null),
          Array[AnyRef](null, null))).toBeTruthy
      expect(Arrays.deepEquals(
          Array[AnyRef]("a", "b", null),
          Array[AnyRef]("a", "b", null))).toBeTruthy
      expect(Arrays.deepEquals(
          Array[AnyRef](null, "a", "b", null, "fooooo"),
          Array[AnyRef](null, "a", "b", null, "fooooo"))).toBeTruthy
      expect(Arrays.deepEquals(
          Array[AnyRef](null, Array[AnyRef]()),
          Array[AnyRef](null, Array[AnyRef]()))).toBeTruthy
      expect(Arrays.deepEquals(
          Array[AnyRef](Array[AnyRef](), Array[AnyRef]()),
          Array[AnyRef](Array[AnyRef](), Array[AnyRef]()))).toBeTruthy
      expect(Arrays.deepEquals(
          Array[AnyRef](Array[AnyRef](Array[AnyRef]())),
          Array[AnyRef](Array[AnyRef](Array[AnyRef]())))).toBeTruthy
      expect(Arrays.deepEquals(
          Array[AnyRef](Array[AnyRef](Array[Int]())),
          Array[AnyRef](Array[AnyRef](Array[Int]())))).toBeTruthy
      expect(Arrays.deepEquals(
          Array[AnyRef](Array[AnyRef](Array[Double]())),
          Array[AnyRef](Array[AnyRef](Array[Double]())))).toBeTruthy
      expect(Arrays.deepEquals(
          Array[AnyRef](Array[AnyRef](Array[Int](1))),
          Array[AnyRef](Array[AnyRef](Array[Int](1))))).toBeTruthy
      expect(Arrays.deepEquals(
          Array[AnyRef](Array[AnyRef](Array[AnyRef](1.asInstanceOf[AnyRef]))),
          Array[AnyRef](Array[AnyRef](Array[AnyRef](1.asInstanceOf[AnyRef]))))).toBeTruthy

      expect(Arrays.deepEquals(
          null: Array[AnyRef],
          Array[AnyRef]())).toBeFalsy
      expect(Arrays.deepEquals(
          Array[AnyRef](),
          null: Array[AnyRef])).toBeFalsy
      expect(Arrays.deepEquals(
          Array[AnyRef](Array[AnyRef](), null),
          Array[AnyRef](null, null))).toBeFalsy
      expect(Arrays.deepEquals(
          Array[AnyRef](null, Array[AnyRef]()),
          Array[AnyRef](null, null))).toBeFalsy
      expect(Arrays.deepEquals(
          Array[AnyRef]("a", "b", null),
          Array[AnyRef]("a", "c", null))).toBeFalsy
      expect(Arrays.deepEquals(
          Array[AnyRef](null, "a", "b", null, "fooooo"),
          Array[AnyRef](null, "a", "b", "c", "fooooo"))).toBeFalsy
      expect(Arrays.deepEquals(
          Array[AnyRef](null, Array[AnyRef]()),
          Array[AnyRef](null, Array[AnyRef](null)))).toBeFalsy
      expect(Arrays.deepEquals(
          Array[AnyRef](Array[AnyRef](), Array[AnyRef]()),
          Array[AnyRef](Array[AnyRef](), Array[AnyRef](null)))).toBeFalsy
      expect(Arrays.deepEquals(
          Array[AnyRef](Array[AnyRef](Array[AnyRef]())),
          Array[AnyRef](Array[AnyRef](Array[AnyRef](null))))).toBeFalsy
      expect(Arrays.deepEquals(
          Array[AnyRef](Array[AnyRef](Array[Int]())),
          Array[AnyRef](Array[AnyRef](Array[Int](1))))).toBeFalsy
      expect(Arrays.deepEquals(
          Array[AnyRef](Array[AnyRef](Array[Double]())),
          Array[AnyRef](Array[AnyRef](Array[Double](1.0))))).toBeFalsy
      expect(Arrays.deepEquals(
          Array[AnyRef](Array[AnyRef](Array[Int](1))),
          Array[AnyRef](Array[AnyRef](Array[Int](2))))).toBeFalsy
      expect(Arrays.deepEquals(
          Array[AnyRef](Array[AnyRef](Array[AnyRef](1.asInstanceOf[AnyRef]))),
          Array[AnyRef](Array[AnyRef](Array[AnyRef](2.asInstanceOf[AnyRef]))))).toBeFalsy
    }

    it("should respond to `toString` for Long") {
      expect(Arrays.toString(null: Array[Long])).toEqual("null")
      expect(Arrays.toString(Array[Long]())).toEqual("[]")
      expect(Arrays.toString(Array[Long](0L))).toEqual("[0]")
      expect(Arrays.toString(Array[Long](1L))).toEqual("[1]")
      expect(Arrays.toString(Array[Long](2L, 3))).toEqual("[2, 3]")
      expect(Arrays.toString(Array[Long](1L, 2L, 3L, 4L, 5L))).toEqual("[1, 2, 3, 4, 5]")
      expect(Arrays.toString(Array[Long](1L, -2L, 3L, Long.MaxValue))).toEqual("[1, -2, 3, 9223372036854775807]")
    }

    it("should respond to `toString` for Int") {
      expect(Arrays.toString(null: Array[Int])).toEqual("null")
      expect(Arrays.toString(Array[Int]())).toEqual("[]")
      expect(Arrays.toString(Array[Int](0))).toEqual("[0]")
      expect(Arrays.toString(Array[Int](1))).toEqual("[1]")
      expect(Arrays.toString(Array[Int](2, 3))).toEqual("[2, 3]")
      expect(Arrays.toString(Array[Int](1, 2, 3, 4, 5))).toEqual("[1, 2, 3, 4, 5]")
      expect(Arrays.toString(Array[Int](1, -2, 3, Int.MaxValue))).toEqual("[1, -2, 3, 2147483647]")
    }

    it("should respond to `toString` for Short") {
      expect(Arrays.toString(null: Array[Short])).toEqual("null")
      expect(Arrays.toString(Array[Short]())).toEqual("[]")
      expect(Arrays.toString(Array[Short](0))).toEqual("[0]")
      expect(Arrays.toString(Array[Short](1))).toEqual("[1]")
      expect(Arrays.toString(Array[Short](2, 3))).toEqual("[2, 3]")
      expect(Arrays.toString(Array[Short](1, 2, 3, 4, 5))).toEqual("[1, 2, 3, 4, 5]")
      expect(Arrays.toString(Array[Short](1, -2, 3, Short.MaxValue))).toEqual("[1, -2, 3, 32767]")
    }

    it("should respond to `toString` for Byte") {
      expect(Arrays.toString(null: Array[Byte])).toEqual("null")
      expect(Arrays.toString(Array[Byte]())).toEqual("[]")
      expect(Arrays.toString(Array[Byte](0))).toEqual("[0]")
      expect(Arrays.toString(Array[Byte](1))).toEqual("[1]")
      expect(Arrays.toString(Array[Byte](2, 3))).toEqual("[2, 3]")
      expect(Arrays.toString(Array[Byte](1, 2, 3, 4, 5))).toEqual("[1, 2, 3, 4, 5]")
      expect(Arrays.toString(Array[Byte](1, -2, 3, Byte.MaxValue))).toEqual("[1, -2, 3, 127]")
    }

    it("should respond to `toString` for Boolean") {
      expect(Arrays.toString(null: Array[Boolean])).toEqual("null")
      expect(Arrays.toString(Array[Boolean]())).toEqual("[]")
      expect(Arrays.toString(Array[Boolean](true))).toEqual("[true]")
      expect(Arrays.toString(Array[Boolean](false))).toEqual("[false]")
      expect(Arrays.toString(Array[Boolean](true, false))).toEqual("[true, false]")
      expect(Arrays.toString(Array[Boolean](true, true, false, false))).toEqual("[true, true, false, false]")
    }

    it("should respond to `toString` for Float") {
      expect(Arrays.toString(null: Array[Float])).toEqual("null")
      expect(Arrays.toString(Array[Float]())).toEqual("[]")
      expect(Arrays.toString(Array[Float](0.0f))).toEqual("[0]")
      expect(Arrays.toString(Array[Float](1.1f))).toEqual("[1.100000023841858]")
      expect(Arrays.toString(Array[Float](2.2f, 3f))).toEqual("[2.200000047683716, 3]")
      expect(Arrays.toString(Array[Float](1f, 2f, 3f, 4f, 5f))).toEqual("[1, 2, 3, 4, 5]")
      expect(Arrays.toString(Array[Float](1f, -2f, 3f, Float.MaxValue))).toEqual("[1, -2, 3, 3.4028234663852886e+38]")
    }

    it("should respond to `toString` for Double") {
      expect(Arrays.toString(null: Array[Double])).toEqual("null")
      expect(Arrays.toString(Array[Double]())).toEqual("[]")
      expect(Arrays.toString(Array[Double](0.0d))).toEqual("[0]")
      expect(Arrays.toString(Array[Double](1.1d))).toEqual("[1.1]")
      expect(Arrays.toString(Array[Double](2.2d, 3d))).toEqual("[2.2, 3]")
      expect(Arrays.toString(Array[Double](1d, 2d, 3d, 4d, 5d))).toEqual("[1, 2, 3, 4, 5]")
      expect(Arrays.toString(Array[Double](1d, -2d, 3d, Double.MaxValue))).toEqual(
          "[1, -2, 3, 1.7976931348623157e+308]")
    }

    it("should respond to `toString` for AnyRef") {
      class C(num: Int) {
        override def toString: String = s"C($num)"
      }
      expect(Arrays.toString(null: Array[AnyRef])).toEqual("null")
      expect(Arrays.toString(Array[AnyRef]())).toEqual("[]")
      expect(Arrays.toString(Array[AnyRef]("abc"))).toEqual("[abc]")
      expect(Arrays.toString(Array[AnyRef]("a", "b", "c"))).toEqual("[a, b, c]")
      expect(Arrays.toString(Array[AnyRef](new C(1)))).toEqual("[C(1)]")
      expect(Arrays.toString(Array[AnyRef](new C(1), "abc", Int.box(1), null))).toEqual("[C(1), abc, 1, null]")
    }

    it("should respond to `deepToString`") {
      expect(Arrays.deepToString(null: Array[AnyRef])).toEqual("null")
      expect(Arrays.deepToString(Array[AnyRef]("abc"))).toEqual("[abc]")
      expect(Arrays.deepToString(Array[AnyRef]("a", "b", "c"))).toEqual("[a, b, c]")
      expect(Arrays.deepToString(Array[AnyRef](Array[Int](1, 2, 3)))).toEqual("[[1, 2, 3]]")
      expect(Arrays.deepToString(Array[AnyRef](Array[Int](1, 2, 3),
          Array[Int](4, 5, 6)))).toEqual("[[1, 2, 3], [4, 5, 6]]")
      expect(Arrays.deepToString(Array[AnyRef](Array[AnyRef]()))).toEqual("[[]]")
      expect(Arrays.deepToString(Array[AnyRef](Array[AnyRef](Array[AnyRef]())))).toEqual("[[[]]]")
      expect(Arrays.deepToString(Array[AnyRef](Array[AnyRef](Array[AnyRef](Array[Int](1, 2, 3))),
          Array[Int](4, 5, 6)))).toEqual("[[[[1, 2, 3]]], [4, 5, 6]]")

      val recArr = Array[AnyRef](null, null)
      recArr(0) = recArr
      expect(Arrays.deepToString(recArr)).toEqual("[[...], null]")
      expect(Arrays.deepToString(Array[AnyRef](recArr))).toEqual("[[[...], null]]")
      expect(Arrays.deepToString(Array[AnyRef](recArr))).toEqual("[[[...], null]]")
      recArr(1) = Array[AnyRef](null, Array[AnyRef](null, recArr, Array[AnyRef](recArr)))
      expect(Arrays.deepToString(recArr)).toEqual("[[...], [null, [null, [...], [[...]]]]]")
    }

  }
}
