package java.lang.reflect

import scala.scalajs.js

import js.JSConverters._

import java.lang.Class

object Array {
  def newInstance(componentType: Class[_], length: Int): AnyRef =
    componentType.newArrayOfThisClass(js.Array(length))

  def newInstance(componentType: Class[_], dimensions: scala.Array[Int]): AnyRef =
    componentType.newArrayOfThisClass(dimensions.toJSArray)

  def getLength(array: AnyRef): Int = array match {
    // yes, this is kind of stupid, but that's how it is
    case array: Array[Object]  => array.length
    case array: Array[Boolean] => array.length
    case array: Array[Char]    => array.length
    case array: Array[Byte]    => array.length
    case array: Array[Short]   => array.length
    case array: Array[Int]     => array.length
    case array: Array[Long]    => array.length
    case array: Array[Float]   => array.length
    case array: Array[Double]  => array.length
    case _ => throw new IllegalArgumentException("argument type mismatch")
  }

  def get(array: AnyRef, index: Int): AnyRef = array match {
    case array: Array[Object]  => array(index)
    case array: Array[Boolean] => new java.lang.Boolean(array(index))
    case array: Array[Char]    => new java.lang.Character(array(index))
    case array: Array[Byte]    => new java.lang.Byte(array(index))
    case array: Array[Short]   => new java.lang.Short(array(index))
    case array: Array[Int]     => new java.lang.Integer(array(index))
    case array: Array[Long]    => new java.lang.Long(array(index))
    case array: Array[Float]   => new java.lang.Float(array(index))
    case array: Array[Double]  => new java.lang.Double(array(index))
    case _ => throw new IllegalArgumentException("argument type mismatch")
  }

  def getBoolean(array: AnyRef, index: Int): Boolean = array match {
    case array: Array[Boolean] => array(index)
    case _ => throw new IllegalArgumentException("argument type mismatch")
  }

  def getChar(array: AnyRef, index: Int): Char = array match {
    case array: Array[Char]    => array(index)
    case _ => throw new IllegalArgumentException("argument type mismatch")
  }

  def getByte(array: AnyRef, index: Int): Byte = array match {
    case array: Array[Byte] => array(index)
    case _ => throw new IllegalArgumentException("argument type mismatch")
  }

  def getShort(array: AnyRef, index: Int): Short = array match {
    case array: Array[Short] => array(index)
    case array: Array[Byte]  => array(index)
    case _ => throw new IllegalArgumentException("argument type mismatch")
  }

  def getInt(array: AnyRef, index: Int): Int = array match {
    case array: Array[Int]   => array(index)
    case array: Array[Char]  => array(index)
    case array: Array[Byte]  => array(index)
    case array: Array[Short] => array(index)
    case _ => throw new IllegalArgumentException("argument type mismatch")
  }

  def getLong(array: AnyRef, index: Int): Long = array match {
    case array: Array[Long]  => array(index)
    case array: Array[Char]  => array(index)
    case array: Array[Byte]  => array(index)
    case array: Array[Short] => array(index)
    case array: Array[Int]   => array(index)
    case _ => throw new IllegalArgumentException("argument type mismatch")
  }

  def getFloat(array: AnyRef, index: Int): Float = array match {
    case array: Array[Float] => array(index)
    case array: Array[Char]  => array(index)
    case array: Array[Byte]  => array(index)
    case array: Array[Short] => array(index)
    case array: Array[Int]   => array(index)
    case array: Array[Long]  => array(index)
    case _ => throw new IllegalArgumentException("argument type mismatch")
  }

  def getDouble(array: AnyRef, index: Int): Double = array match {
    case array: Array[Double] => array(index)
    case array: Array[Char]   => array(index)
    case array: Array[Byte]   => array(index)
    case array: Array[Short]  => array(index)
    case array: Array[Int]    => array(index)
    case array: Array[Long]   => array(index)
    case array: Array[Float]  => array(index)
    case _ => throw new IllegalArgumentException("argument type mismatch")
  }

  def set(array: AnyRef, index: Int, value: AnyRef): Unit = array match {
    case array: Array[Object] => array(index) = value
    case _ =>
      (value: Any) match {
        case value: Boolean => setBoolean(array, index, value)
        case value: Char    => setChar(array, index, value)
        case value: Byte    => setByte(array, index, value)
        case value: Short   => setShort(array, index, value)
        case value: Int     => setInt(array, index, value)
        case value: Long    => setLong(array, index, value)
        case value: Float   => setFloat(array, index, value)
        case value: Double  => setDouble(array, index, value)
        case _ => throw new IllegalArgumentException("argument type mismatch")
      }
  }

  def setBoolean(array: AnyRef, index: Int, value: Boolean): Unit = array match {
    case array: Array[Boolean] => array(index) = value
    case _ => throw new IllegalArgumentException("argument type mismatch")
  }

  def setChar(array: AnyRef, index: Int, value: Char): Unit = array match {
    case array: Array[Char]   => array(index) = value
    case array: Array[Int]    => array(index) = value
    case array: Array[Long]   => array(index) = value
    case array: Array[Float]  => array(index) = value
    case array: Array[Double] => array(index) = value
    case _ => throw new IllegalArgumentException("argument type mismatch")
  }

  def setByte(array: AnyRef, index: Int, value: Byte): Unit = array match {
    case array: Array[Byte]   => array(index) = value
    case array: Array[Short]  => array(index) = value
    case array: Array[Int]    => array(index) = value
    case array: Array[Long]   => array(index) = value
    case array: Array[Float]  => array(index) = value
    case array: Array[Double] => array(index) = value
    case _ => throw new IllegalArgumentException("argument type mismatch")
  }

  def setShort(array: AnyRef, index: Int, value: Short): Unit = array match {
    case array: Array[Short]  => array(index) = value
    case array: Array[Int]    => array(index) = value
    case array: Array[Long]   => array(index) = value
    case array: Array[Float]  => array(index) = value
    case array: Array[Double] => array(index) = value
    case _ => throw new IllegalArgumentException("argument type mismatch")
  }

  def setInt(array: AnyRef, index: Int, value: Int): Unit = array match {
    case array: Array[Int]    => array(index) = value
    case array: Array[Long]   => array(index) = value
    case array: Array[Float]  => array(index) = value
    case array: Array[Double] => array(index) = value
    case _ => throw new IllegalArgumentException("argument type mismatch")
  }

  def setLong(array: AnyRef, index: Int, value: Long): Unit = array match {
    case array: Array[Long]   => array(index) = value
    case array: Array[Float]  => array(index) = value
    case array: Array[Double] => array(index) = value
    case _ => throw new IllegalArgumentException("argument type mismatch")
  }

  def setFloat(array: AnyRef, index: Int, value: Float): Unit = array match {
    case array: Array[Float]  => array(index) = value
    case array: Array[Double] => array(index) = value
    case _ => throw new IllegalArgumentException("argument type mismatch")
  }

  def setDouble(array: AnyRef, index: Int, value: Double): Unit = array match {
    case array: Array[Double] => array(index) = value
    case _ => throw new IllegalArgumentException("argument type mismatch")
  }
}
