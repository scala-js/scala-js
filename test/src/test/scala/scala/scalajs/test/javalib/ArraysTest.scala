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

object ArraysTest extends JasmineTest {

  val stringComparator = new Comparator[String]() {
    def compare(s1: String, s2: String) = s1.compareTo(s2)
  }

  val intComparator = new Comparator[Int]() {
    def compare(i1: Int, i2: Int) = i1 - i2
  }

  describe("java.util.Arrays") {

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

  }
}
