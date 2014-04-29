package java.lang.reflect

import scala.scalajs.js

import java.lang.Class

object Array {
  private[lang] def getUnderlying[A](array: AnyRef): js.Array[A] =
    array.asInstanceOf[js.Dynamic].u.asInstanceOf[js.Array[A]]

  def newInstance(componentType: Class[_], length: Int) =
    newArray(componentType, length)

  def newInstance(componentType: Class[_], dimensions: scala.Array[Int]) =
    multiNewArray(componentType, dimensions)

  def getLength(array: AnyRef): Int =
    getUnderlying(array).length.toInt

  def get(array: AnyRef, index: Int): Any = getUnderlying(array)(index)

  def getBoolean(array: AnyRef, index: Int): Boolean = getUnderlying[Boolean](array)(index)
  def getByte(array: AnyRef, index: Int): Byte = getUnderlying[Byte](array)(index)
  def getChar(array: AnyRef, index: Int): Char = getUnderlying[Int](array)(index).toChar
  def getShort(array: AnyRef, index: Int): Short = getUnderlying[Short](array)(index)
  def getInt(array: AnyRef, index: Int): Int = getUnderlying[Int](array)(index)
  def getLong(array: AnyRef, index: Int): Long = getUnderlying[Long](array)(index)
  def getFloat(array: AnyRef, index: Int): Float = getUnderlying[Float](array)(index)
  def getDouble(array: AnyRef, index: Int): Double = getUnderlying[Double](array)(index)

  def set(array: AnyRef, index: Int, value: Any): Unit = getUnderlying[Any](array)(index) = value

  def setBoolean(array: AnyRef, index: Int, value: Boolean): Unit = getUnderlying[Boolean](array)(index) = value
  def setByte(array: AnyRef, index: Int, value: Byte): Unit = getUnderlying[Byte](array)(index) = value
  def setChar(array: AnyRef, index: Int, value: Char): Unit = getUnderlying[Int](array)(index) = value.toInt
  def setShort(array: AnyRef, index: Int, value: Short): Unit = getUnderlying[Short](array)(index) = value
  def setInt(array: AnyRef, index: Int, value: Int): Unit = getUnderlying[Int](array)(index) = value
  def setLong(array: AnyRef, index: Int, value: Long): Unit = getUnderlying[Long](array)(index) = value
  def setFloat(array: AnyRef, index: Int, value: Float): Unit = getUnderlying[Float](array)(index) = value
  def setDouble(array: AnyRef, index: Int, value: Double): Unit = getUnderlying[Double](array)(index) = value

  private def newArray(componentType: Class[_], length: Int): AnyRef = {
    js.Dynamic.global.ScalaJS.newArrayObject(
        componentType.data.getArrayOf(), js.Array(length))
  }

  private def multiNewArray(componentType: Class[_],
      dimensions: scala.Array[Int]): AnyRef = {
    val lengths = getUnderlying[Int](dimensions)
    var arrayClassData = componentType.data
    var i = 0
    while (i < lengths.length) {
      arrayClassData = arrayClassData.getArrayOf()
      i += 1
    }
    js.Dynamic.global.ScalaJS.newArrayObject(arrayClassData, lengths)
  }
}
