package scala.scalajs.js

/** The typdearray package provides facade types for JavaScript
 *  ArrayBuffer, TypeArrays and DataView. Further, it provides
 *  conversions between primitive Scala arrays and TypedArrays
 */
package object typedarray {

  // Implicit classes scala.Array -> TypedArray
  implicit class AB2TA(val array: scala.Array[Byte]) extends AnyVal {
    def toTypedArray: Int8Array = byteArray2Int8Array(array)
  }

  implicit class AS2TA(val array: scala.Array[Short]) extends AnyVal {
    def toTypedArray: Int16Array = shortArray2Int16Array(array)
  }

  implicit class AC2TA(val array: scala.Array[Char]) extends AnyVal {
    def toTypedArray: Uint16Array = charArray2Uint16Array(array)
  }

  implicit class AI2TA(val array: scala.Array[Int]) extends AnyVal {
    def toTypedArray: Int32Array = intArray2Int32Array(array)
  }

  implicit class AF2TA(val array: scala.Array[Float]) extends AnyVal {
    def toTypedArray: Float32Array = floatArray2Float32Array(array)
  }

  implicit class AD2TA(val array: scala.Array[Double]) extends AnyVal {
    def toTypedArray: Float64Array = doubleArray2Float64Array(array)
  }

  // Implicit classes TypedArray -> scala.Array
  implicit class TA2AB(val array: Int8Array) extends AnyVal {
    def toArray: scala.Array[Byte] = int8Array2ByteArray(array)
  }

  implicit class TA2AS(val array: Int16Array) extends AnyVal {
    def toArray: scala.Array[Short] = int16Array2ShortArray(array)
  }

  implicit class TA2AC(val array: Uint16Array) extends AnyVal {
    def toArray: scala.Array[Char] = uint16Array2CharArray(array)
  }

  implicit class TA2AI(val array: Int32Array) extends AnyVal {
    def toArray: scala.Array[Int] = int32Array2IntArray(array)
  }

  implicit class TA2AF(val array: Float32Array) extends AnyVal {
    def toArray: scala.Array[Float] = float32Array2FloatArray(array)
  }

  implicit class TA2AD(val array: Float64Array) extends AnyVal {
    def toArray: scala.Array[Double] = float64Array2DoubleArray(array)
  }

  // scala.Array -> TypedArray
  def byteArray2Int8Array(array: scala.Array[Byte]): Int8Array = sys.error("stub")
  def shortArray2Int16Array(array: scala.Array[Short]): Int16Array = sys.error("stub")
  def charArray2Uint16Array(array: scala.Array[Char]): Uint16Array = sys.error("stub")
  def intArray2Int32Array(array: scala.Array[Int]): Int32Array = sys.error("stub")
  def floatArray2Float32Array(array: scala.Array[Float]): Float32Array = sys.error("stub")
  def doubleArray2Float64Array(array: scala.Array[Double]): Float64Array = sys.error("stub")

  // TypedArray -> scala.Array
  def int8Array2ByteArray(array: Int8Array): scala.Array[Byte] = sys.error("stub")
  def int16Array2ShortArray(array: Int16Array): scala.Array[Short] = sys.error("stub")
  def uint16Array2CharArray(array: Uint16Array): scala.Array[Char] = sys.error("stub")
  def int32Array2IntArray(array: Int32Array): scala.Array[Int] = sys.error("stub")
  def float32Array2FloatArray(array: Float32Array): scala.Array[Float] = sys.error("stub")
  def float64Array2DoubleArray(array: Float64Array): scala.Array[Double] = sys.error("stub")

}
