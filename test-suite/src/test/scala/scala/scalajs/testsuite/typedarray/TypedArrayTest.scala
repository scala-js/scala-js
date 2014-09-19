/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.testsuite.typedarray

import org.scalajs.jasminetest.JasmineTest
import org.scalajs.jasmine.JasmineExpectation

import scala.scalajs.js
import js.typedarray._

/** Shallow test for TypedArrays. Basically just tests that the method exist and
 *  return something which could be a right result. It is probably sufficient to
 *  test whether a runtime supports TypedArrays.
 */
object TypedArrayTest extends JasmineTest {

  /** Generalized tests for all TypedArrays */
  def tests[V, T <: TypedArray[V, T]](name: String,
      bytesPerElement: => Int,
      lenCtor: Int => T,
      tarrCtor: TypedArray[_, _] => T,
      arrCtor: js.Array[_] => T,
      bufCtor1: (ArrayBuffer) => T,
      bufCtor2: (ArrayBuffer, Int) => T,
      bufCtor3: (ArrayBuffer, Int, Int) => T,
      hasType: Any => Boolean,
      expectV: V => JasmineExpectation,
      intToV: Int => V
  ) = {

    when("typedarray").
    describe(name) {

      it(s"should allow constructing a new $name with length") {
        val x = lenCtor(10)
        expect(hasType(x)).toBeTruthy
        expect(x.length).toBe(10)
      }

      it(s"should allow constructing a new $name from an Int8Array") {
        val x = tarrCtor(new Float32Array(js.Array(3, 7)))
        expect(hasType(x)).toBeTruthy
        expect(x.length).toBe(2)

        expectV(x(0)).toEqual(3)
        expectV(x(1)).toEqual(7)
      }

      it(s"should allow constructing a new $name from a js.Array") {
        val x = arrCtor(js.Array(5,6,7))
        expect(hasType(x)).toBeTruthy
        expect(x.length).toBe(3)

        expectV(x(0)).toEqual(5)
        expectV(x(1)).toEqual(6)
        expectV(x(2)).toEqual(7)
      }

      it(s"should allow constructing a new $name from an ArrayBuffer (1 arg)") {
        val buf = arrCtor(js.Array(5, 6, 7, 8)).buffer
        val x = bufCtor1(buf)
        expect(hasType(x)).toBeTruthy
        expect(x.length).toBe(4)

        expectV(x(0)).toEqual(5)
        expectV(x(1)).toEqual(6)
        expectV(x(2)).toEqual(7)
        expectV(x(3)).toEqual(8)
      }

      it(s"should allow constructing a new $name from an ArrayBuffer (2 arg)") {
        val buf = arrCtor(js.Array(5, 6, 7, 8)).buffer
        val x = bufCtor2(buf, bytesPerElement)
        expect(hasType(x)).toBeTruthy
        expect(x.length).toBe(3)

        expectV(x(0)).toEqual(6)
        expectV(x(1)).toEqual(7)
        expectV(x(2)).toEqual(8)
      }

      it(s"should allow constructing a new $name from an ArrayBuffer (3 arg)") {
        val buf = arrCtor(js.Array(5, 6, 7, 8)).buffer
        val x = bufCtor3(buf, bytesPerElement, 2)
        expect(hasType(x)).toBeTruthy
        expect(x.length).toBe(2)

        expectV(x(0)).toEqual(6)
        expectV(x(1)).toEqual(7)
      }

      it("should allow retrieving the length") {
        val x = lenCtor(100)
        expect(x.length).toBe(100)
      }

      it("should allow retrieving an element") {
        val x = arrCtor(js.Array(5))
        expectV(x(0)).toBe(5)
      }

      it("should allow setting an element") {
        val x = lenCtor(2)

        x(0) = intToV(5)
        x(1) = intToV(10)

        expectV(x(0)).toBe(5)
        expectV(x(1)).toBe(10)
      }

      it("should provide `get`") {
        val x = arrCtor(js.Array(10))
        expectV(x.get(0)).toBe(10)
      }

      it("should provide `set` for a single element") {
        val x = lenCtor(10)
        x.set(0, intToV(5))
        x.set(1, intToV(6))

        expectV(x(0)).toBe(5)
        expectV(x(1)).toBe(6)
        expectV(x(2)).toBe(0)
      }

      it("should provide `set` for a js.Array with one arguments") {
        val x = lenCtor(10)
        x.set(js.Array(5,6,7))
        expectV(x(0)).toBe(5)
        expectV(x(1)).toBe(6)
        expectV(x(2)).toBe(7)
        expectV(x(3)).toBe(0)
        expectV(x(4)).toBe(0)
        expectV(x(5)).toBe(0)
      }

      it("should provide `set` for a js.Array with two arguments") {
        val x = lenCtor(10)
        x.set(js.Array(5,6,7), 2)
        expectV(x(0)).toBe(0)
        expectV(x(1)).toBe(0)
        expectV(x(2)).toBe(5)
        expectV(x(3)).toBe(6)
        expectV(x(4)).toBe(7)
        expectV(x(5)).toBe(0)
      }

      it("should provide `set` for a TypedArray with one argument") {
        val x = lenCtor(10)
        x.set(new Int8Array(js.Array(5,6,7)))
        expectV(x(0)).toBe(5)
        expectV(x(1)).toBe(6)
        expectV(x(2)).toBe(7)
        expectV(x(3)).toBe(0)
        expectV(x(4)).toBe(0)
        expectV(x(5)).toBe(0)
      }

      it("should provide `set` for a TypedArray with two arguments") {
        val x = lenCtor(10)
        x.set(new Int8Array(js.Array(5,6,7)), 2)
        expectV(x(0)).toBe(0)
        expectV(x(1)).toBe(0)
        expectV(x(2)).toBe(5)
        expectV(x(3)).toBe(6)
        expectV(x(4)).toBe(7)
        expectV(x(5)).toBe(0)
      }

      it("should provide `subarray` with one argument") {
        val x = arrCtor(js.Array(1,2,3,4,5,6,7,8,9))
        val y = x.subarray(2)

        expect(y.length).toBe(7)
        expectV(y(0)).toBe(3)

        x(2) = intToV(100)

        expectV(y(0)).toBe(100)
      }

      it("should provide `subarray` with two arguments") {
        val x = arrCtor(js.Array(1,2,3,4,5,6,7,8,9))
        val y = x.subarray(2, 4)

        expect(y.length).toBe(2)
        expectV(y(0)).toBe(3)

        x(2) = intToV(100)

        expectV(y(0)).toBe(100)
      }

      it("should provide `buffer`") {
        val x = arrCtor(js.Array(1,2,3,4,5,6,7,8,9))
        val y = bufCtor3(x.buffer, 0, 2)

        expect(x.buffer eq y.buffer).toBeTruthy
      }

      it("should provide `byteLength`") {
        val x = arrCtor(js.Array(0 until bytesPerElement * 4: _*))
        val y = bufCtor3(x.buffer, bytesPerElement, 3)

        expect(y.byteLength).toBe(3 * bytesPerElement)
      }

      it("should provide `byteOffset`") {
        val x = arrCtor(js.Array(0 until bytesPerElement * 4: _*))
        val y = bufCtor3(x.buffer, bytesPerElement, 3)

        expect(y.byteOffset).toBe(bytesPerElement)
      }

    }
  }

  tests("Int8Array",
      Int8Array.BYTES_PER_ELEMENT,
      len => new Int8Array(len),
      tarr => new Int8Array(tarr),
      arr => new Int8Array(arr),
      buf => new Int8Array(buf),
      (buf, start) => new Int8Array(buf, start),
      (buf, start, end) => new Int8Array(buf, start, end),
      _.isInstanceOf[Int8Array],
      expect (_: Byte),
      _.toByte)

  tests("Uint8Array",
      Uint8Array.BYTES_PER_ELEMENT,
      len => new Uint8Array(len),
      tarr => new Uint8Array(tarr),
      arr => new Uint8Array(arr),
      buf => new Uint8Array(buf),
      (buf, start) => new Uint8Array(buf, start),
      (buf, start, end) => new Uint8Array(buf, start, end),
      _.isInstanceOf[Uint8Array],
      expect (_: Short),
      _.toShort)

  tests("Uint8ClampedArray",
      Uint8ClampedArray.BYTES_PER_ELEMENT,
      len => new Uint8ClampedArray(len),
      tarr => new Uint8ClampedArray(tarr),
      arr => new Uint8ClampedArray(arr),
      buf => new Uint8ClampedArray(buf),
      (buf, start) => new Uint8ClampedArray(buf, start),
      (buf, start, end) => new Uint8ClampedArray(buf, start, end),
      _.isInstanceOf[Uint8ClampedArray],
      expect (_: Int),
      _.toInt)

  tests("Int16Array",
      Int16Array.BYTES_PER_ELEMENT,
      len => new Int16Array(len),
      tarr => new Int16Array(tarr),
      arr => new Int16Array(arr),
      buf => new Int16Array(buf),
      (buf, start) => new Int16Array(buf, start),
      (buf, start, end) => new Int16Array(buf, start, end),
      _.isInstanceOf[Int16Array],
      expect (_: Short),
      _.toShort)

  tests("Uint16Array",
      Uint16Array.BYTES_PER_ELEMENT,
      len => new Uint16Array(len),
      tarr => new Uint16Array(tarr),
      arr => new Uint16Array(arr),
      buf => new Uint16Array(buf),
      (buf, start) => new Uint16Array(buf, start),
      (buf, start, end) => new Uint16Array(buf, start, end),
      _.isInstanceOf[Uint16Array],
      expect (_: Int),
      _.toInt)

  tests("Int32Array",
      Int32Array.BYTES_PER_ELEMENT,
      len => new Int32Array(len),
      tarr => new Int32Array(tarr),
      arr => new Int32Array(arr),
      buf => new Int32Array(buf),
      (buf, start) => new Int32Array(buf, start),
      (buf, start, end) => new Int32Array(buf, start, end),
      _.isInstanceOf[Int32Array],
      expect (_: Int),
      _.toInt)

  tests("Uint32Array",
      Uint32Array.BYTES_PER_ELEMENT,
      len => new Uint32Array(len),
      tarr => new Uint32Array(tarr),
      arr => new Uint32Array(arr),
      buf => new Uint32Array(buf),
      (buf, start) => new Uint32Array(buf, start),
      (buf, start, end) => new Uint32Array(buf, start, end),
      _.isInstanceOf[Uint32Array],
      expect (_: Double),
      _.toDouble)

  tests("Float32Array",
      Float32Array.BYTES_PER_ELEMENT,
      len => new Float32Array(len),
      tarr => new Float32Array(tarr),
      arr => new Float32Array(arr),
      buf => new Float32Array(buf),
      (buf, start) => new Float32Array(buf, start),
      (buf, start, end) => new Float32Array(buf, start, end),
      _.isInstanceOf[Float32Array],
      expect (_: Float),
      _.toFloat)

  tests("Float64Array",
      Float64Array.BYTES_PER_ELEMENT,
      len => new Float64Array(len),
      tarr => new Float64Array(tarr),
      arr => new Float64Array(arr),
      buf => new Float64Array(buf),
      (buf, start) => new Float64Array(buf, start),
      (buf, start, end) => new Float64Array(buf, start, end),
      _.isInstanceOf[Float64Array],
      expect (_: Double),
      _.toDouble)

}
