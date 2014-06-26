package scala.scalajs.js.typedarray

class DataView(buffer: ArrayBuffer, byteOffset: Int = 0,
    byteLength: Int = ???) extends ArrayBufferView {

  def getInt8(byteOffset: Int): Byte = ???
  def getUint8(byteOffset: Int): Short = ???
  def getInt16(byteOffset: Int, littleEndian: Boolean = false): Short = ???
  def getUint16(byteOffset: Int, littleEndian: Boolean = false): Int = ???
  def getInt32(byteOffset: Int, littleEndian: Boolean = false): Int = ???
  def getUint32(byteOffset: Int, littleEndian: Boolean = false): Double = ???
  def getFloat32(byteOffset: Int, littleEndian: Boolean = false): Float = ???
  def getFloat64(byteOffset: Int, littleEndian: Boolean = false): Double = ???

  def setInt8(byteOffset: Int, value: Byte): Unit = ???
  def setUint8(byteOffset: Int, value: Short): Unit = ???
  def setInt16(byteOffset: Int, value: Short, littleEndian: Boolean = false): Unit = ???
  def setUint16(byteOffset: Int, value: Int, littleEndian: Boolean = false): Unit = ???
  def setInt32(byteOffset: Int, value: Int, littleEndian: Boolean = false): Unit = ???
  def setUint32(byteOffset: Int, value: Double, littleEndian: Boolean = false): Unit = ???
  def setFloat32(byteOffset: Int, value: Float, littleEndian: Boolean = false): Unit = ???
  def setFloat64(byteOffset: Int, value: Double, littleEndian: Boolean = false): Unit = ???

}
