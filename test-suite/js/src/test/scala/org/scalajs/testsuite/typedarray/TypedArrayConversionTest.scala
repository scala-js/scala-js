/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.typedarray

import org.scalajs.jasminetest.JasmineTest

import scala.scalajs.js
import js.typedarray._

object TypedArrayConversionTest extends JasmineTest {

  when("typedarray").
  describe("TypedArray to scala.Array conversions") {

    def data(factor: Double): js.Array[Double] =
      js.Array(-1, 1, 2, 3, 4, 5, 6, 7, 8).map((_: Int) * factor)

    def sum(factor: Double): Double = (8 * 9 / 2 - 1) * factor

    it("should convert an Int8Array to a scala.Array[Byte]") {
      val x = new Int8Array(data(1))
      val y = x.toArray

      expect(y.getClass == classOf[scala.Array[Byte]]).toBeTruthy
      expect(y.sum).toEqual(sum(1))

      // Ensure its a copy
      x(0) = 0
      expect(y.sum).toEqual(sum(1))
    }

    it("should convert an Int16Array to a scala.Array[Short]") {
      val x = new Int16Array(data(100))
      val y = x.toArray

      expect(y.getClass == classOf[scala.Array[Short]]).toBeTruthy
      expect(y.sum).toEqual(sum(100))

      // Ensure its a copy
      x(0) = 0
      expect(y.sum).toEqual(sum(100))
    }

    it("should convert an Uint16Array to a scala.Array[Char]") {
      val data = js.Array((1 to 6).map(_ * 10000): _*)
      val sum = (6*7/2*10000).toChar

      val x = new Uint16Array(data)
      val y = x.toArray

      expect(y.getClass == classOf[scala.Array[Char]]).toBeTruthy
      expect(y.sum).toEqual(sum)

      // Ensure its a copy
      x(0) = 0
      expect(y.sum).toEqual(sum)
    }

    it("should convert an Int32Array to a scala.Array[Int]") {
      val x = new Int32Array(data(10000))
      val y = x.toArray

      expect(y.getClass == classOf[scala.Array[Int]]).toBeTruthy
      expect(y.sum).toEqual(sum(10000))

      // Ensure its a copy
      x(0) = 0
      expect(y.sum).toEqual(sum(10000))
    }

    it("should convert a Float32Array to a scala.Array[Float]") {
      val x = new Float32Array(data(0.2))
      val y = x.toArray

      expect(y.getClass == classOf[scala.Array[Float]]).toBeTruthy
      expect(y.sum).toBeCloseTo(sum(0.2), 6)

      // Ensure its a copy
      x(0) = 0
      expect(y.sum).toBeCloseTo(sum(0.2), 6)
    }

    it("should convert a Float64Array to a scala.Array[Double]") {
      val x = new Float64Array(data(0.2))
      val y = x.toArray

      expect(y.getClass == classOf[scala.Array[Double]]).toBeTruthy
      expect(y.sum).toEqual(sum(0.2))

      // Ensure its a copy
      x(0) = 0
      expect(y.sum).toEqual(sum(0.2))
    }

  }

  when("typedarray").
  describe("scala.Array to TypedArray conversions") {

    it("should convert a scala.Array[Byte] to an Int8Array") {
      val x = (Byte.MinValue to Byte.MaxValue).map(_.toByte).toArray
      val y = x.toTypedArray

      expect(y.isInstanceOf[Int8Array]).toBeTruthy
      expect(y.length).toBe(x.length)

      for (i <- 0 until y.length)
        expect(y(i)).toBe(x(i))

      // Ensure its a copy
      x(0) = 0
      expect(y(0)).toBe(Byte.MinValue)
    }

    it("should convert a scala.Array[Short] to an Int16Array") {
      val x = ((Short.MinValue to (Short.MinValue + 1000)) ++
              ((Short.MaxValue - 1000) to Short.MaxValue)).map(_.toShort).toArray
      val y = x.toTypedArray

      expect(y.isInstanceOf[Int16Array]).toBeTruthy
      expect(y.length).toBe(x.length)

      for (i <- 0 until y.length)
        expect(y(i)).toBe(x(i))

      // Ensure its a copy
      x(0) = 0
      expect(y(0)).toBe(Short.MinValue)
    }

    it("should convert a scala.Array[Char] to an Uint16Array") {
      val x = ((Char.MaxValue - 1000) to Char.MaxValue).map(_.toChar).toArray
      val y = x.toTypedArray

      expect(y.isInstanceOf[Uint16Array]).toBeTruthy
      expect(y.length).toBe(x.length)

      for (i <- 0 until y.length)
        expect(y(i)).toBe(x(i).toInt)

      // Ensure its a copy
      x(0) = 0
      expect(y(0)).toBe(Char.MaxValue - 1000)
    }

    it("should convert a scala.Array[Int] to an Int32Array") {
      val x = ((Int.MinValue to (Int.MinValue + 1000)) ++
              ((Int.MaxValue - 1000) to Int.MaxValue)).toArray
      val y = x.toTypedArray

      expect(y.isInstanceOf[Int32Array]).toBeTruthy
      expect(y.length).toBe(x.length)

      for (i <- 0 until y.length)
        expect(y(i)).toBe(x(i))

      // Ensure its a copy
      x(0) = 0
      expect(y(0)).toBe(Int.MinValue)
    }

    it("should convert a scala.Array[Float] to a Float32Array") {
      val x = Array[Float](1.0f, 2.0f, -2.3f, 5.3f)
      val y = x.toTypedArray

      expect(y.isInstanceOf[Float32Array]).toBeTruthy
      expect(y.length).toBe(x.length)

      for (i <- 0 until y.length)
        expect(y(i)).toBe(x(i))

      // Ensure its a copy
      x(0) = 0
      expect(y(0)).toBe(1.0f)
    }

    it("should convert a scala.Array[Double] to a Float64Array") {
      val x = Array[Double](1.0, 2.0, -2.3, 5.3)
      val y = x.toTypedArray

      expect(y.isInstanceOf[Float64Array]).toBeTruthy
      expect(y.length).toBe(x.length)

      for (i <- 0 until y.length)
        expect(y(i)).toBe(x(i))

      // Ensure its a copy
      x(0) = 0
      expect(y(0)).toBe(1.0)
    }

  }

}
