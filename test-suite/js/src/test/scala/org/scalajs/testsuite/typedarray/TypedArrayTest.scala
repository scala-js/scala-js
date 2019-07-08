/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.testsuite.typedarray

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test
import org.scalajs.testsuite.utils.Requires

import scala.scalajs.js
import js.typedarray._

/** Shallow test for TypedArrays. Basically just tests that the method exist and
 *  return something which could be a right result. It is probably sufficient to
 *  test whether a runtime supports TypedArrays.
 */
trait TypedArrayTest[V, T <: TypedArray[V, T]] {

  def bytesPerElement: Int
  def ofFn(items: V*): T
  def fromFn(iterable: js.Iterable[V]): T
  def fromFn[E](iterable: js.Iterable[E])(mapper: js.Function1[E, V]): T
  def fromFn[D, E](iterable: js.Iterable[E], thisObj: D)(mapper: js.ThisFunction1[D, E, V]): T
  def lenCtor(len: Int): T
  def tarrCtor(tarr: TypedArray[_, _]): T
  def itCtor(arr: js.Iterable[_]): T
  def bufCtor1(buf: ArrayBuffer): T
  def bufCtor2(buf: ArrayBuffer, start: Int): T
  def bufCtor3(buf: ArrayBuffer, start: Int, end: Int): T
  def hasType(obj: Any): Boolean
  def intToV(n: Int): V

  @Test def should_provide_a_factory_method_of(): Unit = {
    val x = ofFn(intToV(0), intToV(1), intToV(2))
    assertEquals(3, x.length)
    assertEquals(2, x(2))
  }

  @Test def should_provide_a_factory_method_from(): Unit = {
    val x = fromFn(js.Array(intToV(0), intToV(1), intToV(2)))
    assertEquals(3, x.length)
    assertEquals(2, x(2))
  }

  @Test def should_provide_a_factory_method_from_with_mapping_function(): Unit = {
    val src = js.Array("", "a", "bc")
    val x = fromFn(src)((s: String) => intToV(s.length))
    assertEquals(3, x.length)
    assertEquals(2, x(2))
  }

  @Test def should_provide_a_factory_method_from_with_mapping_function_and_thisArg(): Unit = {
    val src = js.Array("", "a", "bc")
    val x = fromFn(src, 10)((thisArg: Int, s: String) => intToV(s.length * thisArg))
    assertEquals(3, x.length)
    assertEquals(20, x(2))
  }

  @Test def should_allow_constructing_a_new_name_with_length(): Unit = {
    val x = lenCtor(10)
    assertTrue(hasType(x))
    assertEquals(10, x.length)
  }

  @Test def should_allow_constructing_a_new_name_from_an_Int8Array(): Unit = {
    val x = tarrCtor(new Float32Array(js.Array(3, 7)))
    assertTrue(hasType(x))
    assertEquals(2, x.length)

    assertEquals(3, x(0))
    assertEquals(7, x(1))
  }

  @Test def should_allow_constructing_a_new_name_from_a_js_Array(): Unit = {
    val x = itCtor(js.Array(5,6,7))
    assertTrue(hasType(x))
    assertEquals(3, x.length)

    assertEquals(5, x(0))
    assertEquals(6, x(1))
    assertEquals(7, x(2))
  }

  @Test def should_allow_constructing_a_new_name_from_an_ArrayBuffer_1_arg(): Unit = {
    val buf = itCtor(js.Array(5, 6, 7, 8)).buffer
    val x = bufCtor1(buf)
    assertTrue(hasType(x))
    assertEquals(4, x.length)

    assertEquals(5, x(0))
    assertEquals(6, x(1))
    assertEquals(7, x(2))
    assertEquals(8, x(3))
  }

  @Test def should_allow_constructing_a_new_name_from_an_ArrayBuffer_2_args(): Unit = {
    val buf = itCtor(js.Array(5, 6, 7, 8)).buffer
    val x = bufCtor2(buf, bytesPerElement)
    assertTrue(hasType(x))
    assertEquals(3, x.length)

    assertEquals(6, x(0))
    assertEquals(7, x(1))
    assertEquals(8, x(2))
  }

  @Test def should_allow_constructing_a_new_name_from_an_ArrayBuffer_3_args(): Unit = {
    val buf = itCtor(js.Array(5, 6, 7, 8)).buffer
    val x = bufCtor3(buf, bytesPerElement, 2)
    assertTrue(hasType(x))
    assertEquals(2, x.length)

    assertEquals(6, x(0))
    assertEquals(7, x(1))
  }

  @Test def should_allow_retrieving_the_should_allow_retrieving_the(): Unit = {
    val x = lenCtor(100)
    assertEquals(100, x.length)
  }

  @Test def should_allow_retrieving_an_should_allow_retrieving_an(): Unit = {
    val x = itCtor(js.Array(5))
    assertEquals(5, x(0))
  }

  @Test def should_allow_setting_an_should_allow_setting_an(): Unit = {
    val x = lenCtor(2)

    x(0) = intToV(5)
    x(1) = intToV(10)

    assertEquals(5, x(0))
    assertEquals(10, x(1))
  }

  @Test def should_provide_should_provide(): Unit = {
    val x = itCtor(js.Array(10))
    assertEquals(10, x.get(0))
  }

  @Test def set_for_a_single_element(): Unit = {
    val x = lenCtor(10)
    x.set(0, intToV(5))
    x.set(1, intToV(6))

    assertEquals(5, x(0))
    assertEquals(6, x(1))
    assertEquals(0, x(2))
  }

  @Test def set_for_a_js_Array_with_one_arguments(): Unit = {
    val x = lenCtor(10)
    x.set(js.Array(5,6,7))
    assertEquals(5, x(0))
    assertEquals(6, x(1))
    assertEquals(7, x(2))
    assertEquals(0, x(3))
    assertEquals(0, x(4))
    assertEquals(0, x(5))
  }

  @Test def should_provide_set_for_a_js_Array_with_two_arguments(): Unit = {
    val x = lenCtor(10)
    x.set(js.Array(5,6,7), 2)
    assertEquals(0, x(0))
    assertEquals(0, x(1))
    assertEquals(5, x(2))
    assertEquals(6, x(3))
    assertEquals(7, x(4))
    assertEquals(0, x(5))
  }

  @Test def should_provide_set_for_a_TypedArray_with_one_argument(): Unit = {
    val x = lenCtor(10)
    x.set(new Int8Array(js.Array(5,6,7)))
    assertEquals(5, x(0))
    assertEquals(6, x(1))
    assertEquals(7, x(2))
    assertEquals(0, x(3))
    assertEquals(0, x(4))
    assertEquals(0, x(5))
  }

  @Test def should_provide_set_for_a_TypedArray_with_two_arguments(): Unit = {
    val x = lenCtor(10)
    x.set(new Int8Array(js.Array(5,6,7)), 2)
    assertEquals(0, x(0))
    assertEquals(0, x(1))
    assertEquals(5, x(2))
    assertEquals(6, x(3))
    assertEquals(7, x(4))
    assertEquals(0, x(5))
  }

  @Test def subarray_with_one_argument(): Unit = {
    val x = itCtor(js.Array(1,2,3,4,5,6,7,8,9))
    val y = x.subarray(2)

    assertEquals(7, y.length)
    assertEquals(3, y(0))

    x(2) = intToV(100)

    assertEquals(100, y(0))
  }

  @Test def subarray_with_two_arguments(): Unit = {
    val x = itCtor(js.Array(1,2,3,4,5,6,7,8,9))
    val y = x.subarray(2, 4)

    assertEquals(2, y.length)
    assertEquals(3, y(0))

    x(2) = intToV(100)

    assertEquals(100, y(0))
  }

  @Test def buffer(): Unit = {
    val x = itCtor(js.Array(1,2,3,4,5,6,7,8,9))
    val y = bufCtor3(x.buffer, 0, 2)

    assertSame(x.buffer, y.buffer)
  }

  @Test def byteLength(): Unit = {
    val x = itCtor(js.Array(0 until bytesPerElement * 4: _*))
    val y = bufCtor3(x.buffer, bytesPerElement, 3)

    assertEquals(3 * bytesPerElement, y.byteLength)
  }

  @Test def byteOffset(): Unit = {
    val x = itCtor(js.Array(0 until bytesPerElement * 4: _*))
    val y = bufCtor3(x.buffer, bytesPerElement, 3)

    assertEquals(bytesPerElement, y.byteOffset)
  }

  @Test def is_iterable(): Unit = {
    assumeTrue("Assuming JavaScript symbols are supported",
        org.scalajs.testsuite.utils.Platform.jsSymbols)

    import js.JSConverters._

    val testData: List[Int] = (1 to 10).toList
    val typedArray: T = itCtor(testData.toJSArray)
    val jsIterable: js.Iterable[V] = typedArray
    val iterable: Iterable[V] = jsIterable

    assertEquals(iterable.toList, testData.map(intToV))
  }

  @Test def from_iterable(): Unit = {
    assumeTrue("Assuming JavaScript symbols are supported",
        org.scalajs.testsuite.utils.Platform.jsSymbols)

    import js.JSConverters._

    val x = itCtor(Iterable(1, 2, 3, 4).toJSIterable)

    assertEquals(4, x.length)
    assertEquals(1, x(0))
    assertEquals(2, x(1))
    assertEquals(3, x(2))
    assertEquals(4, x(3))
  }
}

object Int8ArrayTest extends Requires.TypedArray

class Int8ArrayTest extends TypedArrayTest[Byte, Int8Array] {
  def ofFn(items: Byte*): Int8Array = Int8Array.of(items: _*)
  def fromFn(iterable: js.Iterable[Byte]): Int8Array = Int8Array.from(iterable)
  def fromFn[E](iterable: js.Iterable[E])(fn: js.Function1[E, Byte]): Int8Array = Int8Array.from(iterable, fn)
  def fromFn[D, E](iterable: js.Iterable[E], thisObj: D)(fn: js.ThisFunction1[D, E, Byte]): Int8Array =
    Int8Array.from(iterable, fn, thisObj)
  def bytesPerElement: Int = Int8Array.BYTES_PER_ELEMENT
  def lenCtor(len: Int): Int8Array = new Int8Array(len)
  def tarrCtor(tarr: TypedArray[_, _]): Int8Array = new Int8Array(tarr)
  def itCtor(arr: js.Iterable[_]): Int8Array = new Int8Array(arr)
  def bufCtor1(buf: ArrayBuffer): Int8Array = new Int8Array(buf)
  def bufCtor2(buf: ArrayBuffer, start: Int): Int8Array = new Int8Array(buf, start)
  def bufCtor3(buf: ArrayBuffer, start: Int, end: Int): Int8Array = new Int8Array(buf, start, end)
  def hasType(obj: Any): Boolean = obj.isInstanceOf[Int8Array]
  def intToV(n: Int): Byte = n.toByte
}

object Uint8ArrayTest extends Requires.TypedArray

class Uint8ArrayTest extends TypedArrayTest[Short, Uint8Array] {
  def ofFn(items: Short*): Uint8Array = Uint8Array.of(items: _*)
  def fromFn(iterable: js.Iterable[Short]): Uint8Array = Uint8Array.from(iterable)
  def fromFn[E](iterable: js.Iterable[E])(fn: js.Function1[E, Short]): Uint8Array = Uint8Array.from(iterable, fn)
  def fromFn[D, E](iterable: js.Iterable[E], thisObj: D)(fn: js.ThisFunction1[D, E, Short]): Uint8Array =
    Uint8Array.from(iterable, fn, thisObj)
  def bytesPerElement: Int = Uint8Array.BYTES_PER_ELEMENT
  def lenCtor(len: Int): Uint8Array = new Uint8Array(len)
  def tarrCtor(tarr: TypedArray[_, _]): Uint8Array = new Uint8Array(tarr)
  def itCtor(arr: js.Iterable[_]): Uint8Array = new Uint8Array(arr)
  def bufCtor1(buf: ArrayBuffer): Uint8Array = new Uint8Array(buf)
  def bufCtor2(buf: ArrayBuffer, start: Int): Uint8Array = new Uint8Array(buf, start)
  def bufCtor3(buf: ArrayBuffer, start: Int, end: Int): Uint8Array = new Uint8Array(buf, start, end)
  def hasType(obj: Any): Boolean = obj.isInstanceOf[Uint8Array]
  def intToV(n: Int): Short = n.toShort
}

object Uint8ClampedArrayTest extends Requires.TypedArray

class Uint8ClampedArrayTest extends TypedArrayTest[Int, Uint8ClampedArray] {
  def ofFn(items: Int*): Uint8ClampedArray = Uint8ClampedArray.of(items: _*)
  def fromFn(iterable: js.Iterable[Int]): Uint8ClampedArray = Uint8ClampedArray.from(iterable)
  def fromFn[E](iterable: js.Iterable[E])(fn: js.Function1[E, Int]): Uint8ClampedArray =
    Uint8ClampedArray.from(iterable, fn)
  def fromFn[D, E](iterable: js.Iterable[E], thisObj: D)(fn: js.ThisFunction1[D, E, Int]): Uint8ClampedArray =
    Uint8ClampedArray.from(iterable, fn, thisObj)
  def bytesPerElement: Int = Uint8ClampedArray.BYTES_PER_ELEMENT
  def lenCtor(len: Int): Uint8ClampedArray = new Uint8ClampedArray(len)
  def tarrCtor(tarr: TypedArray[_, _]): Uint8ClampedArray = new Uint8ClampedArray(tarr)
  def itCtor(arr: js.Iterable[_]): Uint8ClampedArray = new Uint8ClampedArray(arr)
  def bufCtor1(buf: ArrayBuffer): Uint8ClampedArray = new Uint8ClampedArray(buf)
  def bufCtor2(buf: ArrayBuffer, start: Int): Uint8ClampedArray = new Uint8ClampedArray(buf, start)
  def bufCtor3(buf: ArrayBuffer, start: Int, end: Int): Uint8ClampedArray = new Uint8ClampedArray(buf, start, end)
  def hasType(obj: Any): Boolean = obj.isInstanceOf[Uint8ClampedArray]
  def intToV(n: Int): Int = n
}

object Int16ArrayTest extends Requires.TypedArray

class Int16ArrayTest extends TypedArrayTest[Short, Int16Array] {
  def ofFn(items: Short*): Int16Array = Int16Array.of(items: _*)
  def fromFn(iterable: js.Iterable[Short]): Int16Array = Int16Array.from(iterable)
  def fromFn[E](iterable: js.Iterable[E])(fn: js.Function1[E, Short]): Int16Array = Int16Array.from(iterable, fn)
  def fromFn[D, E](iterable: js.Iterable[E], thisObj: D)(fn: js.ThisFunction1[D, E, Short]): Int16Array =
    Int16Array.from(iterable, fn, thisObj)
  def bytesPerElement: Int = Int16Array.BYTES_PER_ELEMENT
  def lenCtor(len: Int): Int16Array = new Int16Array(len)
  def tarrCtor(tarr: TypedArray[_, _]): Int16Array = new Int16Array(tarr)
  def itCtor(arr: js.Iterable[_]): Int16Array = new Int16Array(arr)
  def bufCtor1(buf: ArrayBuffer): Int16Array = new Int16Array(buf)
  def bufCtor2(buf: ArrayBuffer, start: Int): Int16Array = new Int16Array(buf, start)
  def bufCtor3(buf: ArrayBuffer, start: Int, end: Int): Int16Array = new Int16Array(buf, start, end)
  def hasType(obj: Any): Boolean = obj.isInstanceOf[Int16Array]
  def intToV(n: Int): Short = n.toShort
}

object Uint16ArrayTest extends Requires.TypedArray

class Uint16ArrayTest extends TypedArrayTest[Int, Uint16Array] {
  def ofFn(items: Int*): Uint16Array = Uint16Array.of(items: _*)
  def fromFn(iterable: js.Iterable[Int]): Uint16Array = Uint16Array.from(iterable)
  def fromFn[E](iterable: js.Iterable[E])(fn: js.Function1[E, Int]): Uint16Array = Uint16Array.from(iterable, fn)
  def fromFn[D, E](iterable: js.Iterable[E], thisObj: D)(fn: js.ThisFunction1[D, E, Int]): Uint16Array =
    Uint16Array.from(iterable, fn, thisObj)
  def bytesPerElement: Int = Uint16Array.BYTES_PER_ELEMENT
  def lenCtor(len: Int): Uint16Array = new Uint16Array(len)
  def tarrCtor(tarr: TypedArray[_, _]): Uint16Array = new Uint16Array(tarr)
  def itCtor(arr: js.Iterable[_]): Uint16Array =  new Uint16Array(arr)
  def bufCtor1(buf: ArrayBuffer): Uint16Array = new Uint16Array(buf)
  def bufCtor2(buf: ArrayBuffer, start: Int): Uint16Array = new Uint16Array(buf, start)
  def bufCtor3(buf: ArrayBuffer, start: Int, end: Int): Uint16Array = new Uint16Array(buf, start, end)
  def hasType(obj: Any): Boolean = obj.isInstanceOf[Uint16Array]
  def intToV(n: Int): Int = n
}

object Int32ArrayTest extends Requires.TypedArray

class Int32ArrayTest extends TypedArrayTest[Int, Int32Array] {
  def ofFn(items: Int*): Int32Array = Int32Array.of(items: _*)
  def fromFn(iterable: js.Iterable[Int]): Int32Array = Int32Array.from(iterable)
  def fromFn[E](iterable: js.Iterable[E])(fn: js.Function1[E, Int]): Int32Array = Int32Array.from(iterable, fn)
  def fromFn[D, E](iterable: js.Iterable[E], thisObj: D)(fn: js.ThisFunction1[D, E, Int]): Int32Array =
    Int32Array.from(iterable, fn, thisObj)
  def bytesPerElement: Int = Int32Array.BYTES_PER_ELEMENT
  def lenCtor(len: Int): Int32Array = new Int32Array(len)
  def tarrCtor(tarr: TypedArray[_, _]): Int32Array = new Int32Array(tarr)
  def itCtor(arr: js.Iterable[_]): Int32Array = new Int32Array(arr)
  def bufCtor1(buf: ArrayBuffer): Int32Array = new Int32Array(buf)
  def bufCtor2(buf: ArrayBuffer, start: Int): Int32Array = new Int32Array(buf, start)
  def bufCtor3(buf: ArrayBuffer, start: Int, end: Int): Int32Array = new Int32Array(buf, start, end)
  def hasType(obj: Any): Boolean = obj.isInstanceOf[Int32Array]
  def intToV(n: Int): Int = n
}

object Uint32ArrayTest extends Requires.TypedArray

class Uint32ArrayTest extends TypedArrayTest[Double, Uint32Array] {
  def ofFn(items: Double*): Uint32Array = Uint32Array.of(items: _*)
  def fromFn(iterable: js.Iterable[Double]): Uint32Array = Uint32Array.from(iterable)
  def fromFn[E](iterable: js.Iterable[E])(fn: js.Function1[E, Double]): Uint32Array = Uint32Array.from(iterable, fn)
  def fromFn[D, E](iterable: js.Iterable[E], thisObj: D)(fn: js.ThisFunction1[D, E, Double]): Uint32Array =
    Uint32Array.from(iterable, fn, thisObj)
  def bytesPerElement: Int = Uint32Array.BYTES_PER_ELEMENT
  def lenCtor(len: Int): Uint32Array = new Uint32Array(len)
  def tarrCtor(tarr: TypedArray[_, _]): Uint32Array = new Uint32Array(tarr)
  def itCtor(arr: js.Iterable[_]): Uint32Array = new Uint32Array(arr)
  def bufCtor1(buf: ArrayBuffer): Uint32Array = new Uint32Array(buf)
  def bufCtor2(buf: ArrayBuffer, start: Int): Uint32Array = new Uint32Array(buf, start)
  def bufCtor3(buf: ArrayBuffer, start: Int, end: Int): Uint32Array = new Uint32Array(buf, start, end)
  def hasType(obj: Any): Boolean = obj.isInstanceOf[Uint32Array]
  def intToV(n: Int): Double = n.toDouble
}

object Float32ArrayTest extends Requires.TypedArray

class Float32ArrayTest extends TypedArrayTest[Float, Float32Array] {
  def ofFn(items: Float*): Float32Array = Float32Array.of(items: _*)
  def fromFn(iterable: js.Iterable[Float]): Float32Array = Float32Array.from(iterable)
  def fromFn[E](iterable: js.Iterable[E])(fn: js.Function1[E, Float]): Float32Array = Float32Array.from(iterable, fn)
  def fromFn[D, E](iterable: js.Iterable[E], thisObj: D)(fn: js.ThisFunction1[D, E, Float]): Float32Array =
    Float32Array.from(iterable, fn, thisObj)
  def bytesPerElement: Int = Float32Array.BYTES_PER_ELEMENT
  def lenCtor(len: Int): Float32Array = new Float32Array(len)
  def tarrCtor(tarr: TypedArray[_, _]): Float32Array = new Float32Array(tarr)
  def itCtor(arr: js.Iterable[_]): Float32Array =  new Float32Array(arr)
  def bufCtor1(buf: ArrayBuffer): Float32Array = new Float32Array(buf)
  def bufCtor2(buf: ArrayBuffer, start: Int): Float32Array = new Float32Array(buf, start)
  def bufCtor3(buf: ArrayBuffer, start: Int, end: Int): Float32Array = new Float32Array(buf, start, end)
  def hasType(obj: Any): Boolean = obj.isInstanceOf[Float32Array]
  def intToV(n: Int): Float = n.toFloat
}

object Float64ArrayTest extends Requires.TypedArray

class Float64ArrayTest extends TypedArrayTest[Double, Float64Array] {
  def ofFn(items: Double*): Float64Array = Float64Array.of(items: _*)
  def fromFn(iterable: js.Iterable[Double]): Float64Array = Float64Array.from(iterable)
  def fromFn[E](iterable: js.Iterable[E])(fn: js.Function1[E, Double]): Float64Array = Float64Array.from(iterable, fn)
  def fromFn[D, E](iterable: js.Iterable[E], thisObj: D)(fn: js.ThisFunction1[D, E, Double]): Float64Array =
    Float64Array.from(iterable, fn, thisObj)
  def bytesPerElement: Int = Float64Array.BYTES_PER_ELEMENT
  def lenCtor(len: Int): Float64Array = new Float64Array(len)
  def tarrCtor(tarr: TypedArray[_, _]): Float64Array = new Float64Array(tarr)
  def itCtor(arr: js.Iterable[_]): Float64Array = new Float64Array(arr)
  def bufCtor1(buf: ArrayBuffer): Float64Array = new Float64Array(buf)
  def bufCtor2(buf: ArrayBuffer, start: Int): Float64Array = new Float64Array(buf, start)
  def bufCtor3(buf: ArrayBuffer, start: Int, end: Int): Float64Array = new Float64Array(buf, start, end)
  def hasType(obj: Any): Boolean = obj.isInstanceOf[Float64Array]
  def intToV(n: Int): Double = n.toDouble
}
