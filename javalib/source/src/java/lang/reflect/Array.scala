package java.lang.reflect

import scala.js._

import java.lang.Class

object Array {
  private[lang] def getUnderlying[A <: JSAny](array: AnyRef): JSArray[A] =
    array.asInstanceOf[JSDynamic].underlying.asInstanceOf

  def newInstance(componentType: Class[_], length: Int) =
    newArray(componentType, length)

  def newInstance(componentType: Class[_], dimensions: scala.Array[Int]) =
    multiNewArray(componentType, dimensions)

  def getLength(array: AnyRef): Int =
    getUnderlying(array).length.toInt

  def get(array: AnyRef, index: Int): Any = getUnderlying(array)(index)

  def getBoolean(array: AnyRef, index: Int): Boolean = getUnderlying[JSBoolean](array)(index)
  def getByte(array: AnyRef, index: Int): Byte = getUnderlying[JSNumber](array)(index).toByte
  def getChar(array: AnyRef, index: Int): Char = getUnderlying[JSNumber](array)(index).toInt.toChar
  def getShort(array: AnyRef, index: Int): Short = getUnderlying[JSNumber](array)(index).toShort
  def getInt(array: AnyRef, index: Int): Int = getUnderlying[JSNumber](array)(index).toInt
  def getLong(array: AnyRef, index: Int): Long = getUnderlying[JSNumber](array)(index).toLong
  def getFloat(array: AnyRef, index: Int): Float = getUnderlying[JSNumber](array)(index).toFloat
  def getDouble(array: AnyRef, index: Int): Double = getUnderlying[JSNumber](array)(index).toDouble

  def set(array: AnyRef, index: Int, value: Any): Unit = getUnderlying[JSAny](array)(index) = value.asInstanceOf[JSAny]

  def setBoolean(array: AnyRef, index: Int, value: Boolean): Unit = getUnderlying[JSAny](array)(index) = value
  def setByte(array: AnyRef, index: Int, value: Byte): Unit = getUnderlying[JSAny](array)(index) = value
  def setChar(array: AnyRef, index: Int, value: Char): Unit = getUnderlying[JSAny](array)(index) = value.toInt
  def setShort(array: AnyRef, index: Int, value: Short): Unit = getUnderlying[JSAny](array)(index) = value
  def setInt(array: AnyRef, index: Int, value: Int): Unit = getUnderlying[JSAny](array)(index) = value
  def setLong(array: AnyRef, index: Int, value: Long): Unit = getUnderlying[JSAny](array)(index) = value
  def setFloat(array: AnyRef, index: Int, value: Float): Unit = getUnderlying[JSAny](array)(index) = value
  def setDouble(array: AnyRef, index: Int, value: Double): Unit = getUnderlying[JSAny](array)(index) = value

  private def newArray(componentType: Class[_], length: Int): AnyRef = {
    componentType.env.newArrayObject(
        componentType.data.array, JSArray(length:JSNumber))
  }

  private def multiNewArray(componentType: Class[_],
      dimensions: scala.Array[Int]): AnyRef = {
    val lengths = getUnderlying[JSNumber](dimensions)
    var arrayClassData = componentType.data
    var i = 0
    while (i < lengths.length) {
      arrayClassData = arrayClassData.array
      i += 1
    }
    componentType.env.newArrayObject(arrayClassData, lengths)
  }
}
