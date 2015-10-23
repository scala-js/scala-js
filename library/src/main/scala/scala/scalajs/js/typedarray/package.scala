package scala.scalajs.js

import JSConverters._

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  The typdearray package provides facade types for JavaScript
 *  ArrayBuffer, TypeArrays and DataView. Further, it provides
 *  conversions between primitive Scala arrays and TypedArrays
 */
package object typedarray {

  // Implicit classes scala.Array -> TypedArray

  /** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
   *  Adds `toTypedArray` conversion to an `Array[Byte]`
   */
  implicit class AB2TA(val array: scala.Array[Byte]) extends AnyVal {
    def toTypedArray: Int8Array = byteArray2Int8Array(array)
  }

  /** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
   *  Adds `toTypedArray` conversion to an `Array[Short]`
   */
  implicit class AS2TA(val array: scala.Array[Short]) extends AnyVal {
    def toTypedArray: Int16Array = shortArray2Int16Array(array)
  }

  /** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
   *  Adds `toTypedArray` conversion to an `Array[Char]`
   */
  implicit class AC2TA(val array: scala.Array[Char]) extends AnyVal {
    def toTypedArray: Uint16Array = charArray2Uint16Array(array)
  }

  /** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
   *  Adds `toTypedArray` conversion to an `Array[Int]`
   */
  implicit class AI2TA(val array: scala.Array[Int]) extends AnyVal {
    def toTypedArray: Int32Array = intArray2Int32Array(array)
  }

  /** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
   *  Adds `toTypedArray` conversion to an `Array[Float]`
   */
  implicit class AF2TA(val array: scala.Array[Float]) extends AnyVal {
    def toTypedArray: Float32Array = floatArray2Float32Array(array)
  }

  /** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
   *  Adds `toTypedArray` conversion to an `Array[Double]`
   */
  implicit class AD2TA(val array: scala.Array[Double]) extends AnyVal {
    def toTypedArray: Float64Array = doubleArray2Float64Array(array)
  }

  // Implicit classes TypedArray -> scala.Array

  /** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
   *  Adds `toArray` conversion to a [[Int8Array]]
   */
  implicit class TA2AB(val array: Int8Array) extends AnyVal {
    def toArray: scala.Array[Byte] = int8Array2ByteArray(array)
  }

  /** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
   *  Adds `toArray` conversion to a [[Int16Array]]
   */
  implicit class TA2AS(val array: Int16Array) extends AnyVal {
    def toArray: scala.Array[Short] = int16Array2ShortArray(array)
  }

  /** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
   *  Adds `toArray` conversion to a [[Uint16Array]]
   */
  implicit class TA2AC(val array: Uint16Array) extends AnyVal {
    def toArray: scala.Array[Char] = uint16Array2CharArray(array)
  }

  /** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
   *  Adds `toArray` conversion to a [[Int32Array]]
   */
  implicit class TA2AI(val array: Int32Array) extends AnyVal {
    def toArray: scala.Array[Int] = int32Array2IntArray(array)
  }

  /** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
   *  Adds `toArray` conversion to a [[Float32Array]]
   */
  implicit class TA2AF(val array: Float32Array) extends AnyVal {
    def toArray: scala.Array[Float] = float32Array2FloatArray(array)
  }

  /** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
   *  Adds `toArray` conversion to a [[Float64Array]]
   */
  implicit class TA2AD(val array: Float64Array) extends AnyVal {
    def toArray: scala.Array[Double] = float64Array2DoubleArray(array)
  }

  // scala.Array -> TypedArray

  def byteArray2Int8Array(array: scala.Array[Byte]): Int8Array =
    array2typedArrayImpl(array, new Int8Array(array.length))

  def shortArray2Int16Array(array: scala.Array[Short]): Int16Array =
    array2typedArrayImpl(array, new Int16Array(array.length))

  def charArray2Uint16Array(array: scala.Array[Char]): Uint16Array = {
    // Can't use array2typedArrayImpl because Uint16Array contains Ints
    val len = array.length
    val dest = new Uint16Array(len)
    var i = 0
    while (i < len) {
      dest(i) = array(i).toInt
      i += 1
    }
    dest
  }

  def intArray2Int32Array(array: scala.Array[Int]): Int32Array =
    array2typedArrayImpl(array, new Int32Array(array.length))

  def floatArray2Float32Array(array: scala.Array[Float]): Float32Array =
    array2typedArrayImpl(array, new Float32Array(array.length))

  def doubleArray2Float64Array(array: scala.Array[Double]): Float64Array =
    array2typedArrayImpl(array, new Float64Array(array.length))

  @inline private def array2typedArrayImpl[ // scalastyle:ignore
      @specialized(Byte, Short, Int, Float, Double) T,
      Repr <: TypedArray[T, Repr]](
      array: scala.Array[T], dest: Repr): Repr = {
    val len = array.length
    var i = 0
    while (i < len) {
      dest(i) = array(i)
      i += 1
    }
    dest
  }

  // TypedArray -> scala.Array

  def int8Array2ByteArray(array: Int8Array): scala.Array[Byte] =
    typedArray2arrayImpl(array, new scala.Array(array.length))

  def int16Array2ShortArray(array: Int16Array): scala.Array[Short] =
    typedArray2arrayImpl(array, new scala.Array(array.length))

  def uint16Array2CharArray(array: Uint16Array): scala.Array[Char] = {
    // Can't use typedArray2arrayImpl because Uint16Array contains Ints
    val len = array.length
    val dest = new scala.Array[Char](len)
    var i = 0
    while (i < len) {
      dest(i) = array(i).toChar
      i += 1
    }
    dest
  }

  def int32Array2IntArray(array: Int32Array): scala.Array[Int] =
    typedArray2arrayImpl(array, new scala.Array(array.length))

  def float32Array2FloatArray(array: Float32Array): scala.Array[Float] =
    typedArray2arrayImpl(array, new scala.Array(array.length))

  def float64Array2DoubleArray(array: Float64Array): scala.Array[Double] =
    typedArray2arrayImpl(array, new scala.Array(array.length))

  @inline private def typedArray2arrayImpl[ // scalastyle:ignore
      @specialized(Byte, Short, Int, Float, Double) T,
      Repr <: TypedArray[T, Repr]](
      array: Repr, dest: scala.Array[T]): scala.Array[T] = {
    val len = dest.length
    var i = 0
    while (i < len) {
      dest(i) = array(i)
      i += 1
    }
    dest
  }

}
