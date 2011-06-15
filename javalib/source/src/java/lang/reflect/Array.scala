package java.lang.reflect

import java.lang.Class

object Array {
  def newInstance(componentType: Class[_], length: Int) =
    newArray(componentType, length)

  def newInstance(componentType: Class[_], dimensions: scala.Array[Int]) =
    multiNewArray(componentType, dimensions)

  @native def getLength(array: AnyRef): Int

  @native def get(array: AnyRef, index: Int): Any

  @native def getBoolean(array: AnyRef, index: Int): Boolean
  @native def getByte(array: AnyRef, index: Int): Byte
  @native def getChar(array: AnyRef, index: Int): Char
  @native def getShort(array: AnyRef, index: Int): Short
  @native def getInt(array: AnyRef, index: Int): Int
  @native def getLong(array: AnyRef, index: Int): Long
  @native def getFloat(array: AnyRef, index: Int): Float
  @native def getDouble(array: AnyRef, index: Int): Double

  @native def set(array: AnyRef, index: Int, value: Any): Unit

  @native def setBoolean(array: AnyRef, index: Int, value: Boolean): Unit
  @native def setByte(array: AnyRef, index: Int, value: Byte): Unit
  @native def setChar(array: AnyRef, index: Int, value: Char): Unit
  @native def setShort(array: AnyRef, index: Int, value: Short): Unit
  @native def setInt(array: AnyRef, index: Int, value: Int): Unit
  @native def setLong(array: AnyRef, index: Int, value: Long): Unit
  @native def setFloat(array: AnyRef, index: Int, value: Float): Unit
  @native def setDouble(array: AnyRef, index: Int, value: Double): Unit
  
  @native private def newArray(componentType: Class[_], length: Int): AnyRef
  @native private def multiNewArray(componentType: Class[_],
      dimensions: scala.Array[Int]): AnyRef
}
