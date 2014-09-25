package scala.scalajs.js.typedarray

import scala.scalajs.js

class DataView(buffer: ArrayBuffer, byteOffset: Int = 0,
    byteLength: Int = ???) extends ArrayBufferView {

  def getInt8(byteOffset: Int): Byte = js.native
  def getUint8(byteOffset: Int): Short = js.native
  def getInt16(byteOffset: Int, littleEndian: Boolean = false): Short = js.native
  def getUint16(byteOffset: Int, littleEndian: Boolean = false): Int = js.native
  def getInt32(byteOffset: Int, littleEndian: Boolean = false): Int = js.native
  def getUint32(byteOffset: Int, littleEndian: Boolean = false): Double = js.native
  def getFloat32(byteOffset: Int, littleEndian: Boolean = false): Float = js.native
  def getFloat64(byteOffset: Int, littleEndian: Boolean = false): Double = js.native

  def setInt8(byteOffset: Int, value: Byte): Unit = js.native
  def setUint8(byteOffset: Int, value: Short): Unit = js.native
  def setInt16(byteOffset: Int, value: Short, littleEndian: Boolean = false): Unit = js.native
  def setUint16(byteOffset: Int, value: Int, littleEndian: Boolean = false): Unit = js.native
  def setInt32(byteOffset: Int, value: Int, littleEndian: Boolean = false): Unit = js.native
  def setUint32(byteOffset: Int, value: Double, littleEndian: Boolean = false): Unit = js.native
  def setFloat32(byteOffset: Int, value: Float, littleEndian: Boolean = false): Unit = js.native
  def setFloat64(byteOffset: Int, value: Double, littleEndian: Boolean = false): Unit = js.native

}
