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

package java.lang.reflect

import scala.scalajs.js

import java.lang.Class

object Array {
  @inline
  def newInstance(componentType: Class[_], length: Int): AnyRef =
    componentType.newArrayOfThisClass(length)

  def newInstance(componentType: Class[_], dimensions: scala.Array[Int]): AnyRef = {
    def rec(componentType: Class[_], offset: Int): AnyRef = {
      val length = dimensions(offset)
      val result = newInstance(componentType, length)
      val innerOffset = offset + 1
      if (innerOffset < dimensions.length) {
        val result2 = result.asInstanceOf[Array[AnyRef]]
        val innerComponentType = componentType.getComponentType()
        var i = 0
        while (i != length) {
          result2(i) = rec(innerComponentType, innerOffset)
          i += 1
        }
      }
      result
    }

    var componentType2 = componentType
    var i = 1
    while (i != dimensions.length) {
      componentType2 = newInstance(componentType2, 0).getClass()
      i += 1
    }
    rec(componentType2, 0)
  }

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
    case _                     => mismatch(array)
  }

  def get(array: AnyRef, index: Int): AnyRef = array match {
    case array: Array[Object]  => array(index)
    case array: Array[Boolean] => java.lang.Boolean.valueOf(array(index))
    case array: Array[Char]    => java.lang.Character.valueOf(array(index))
    case array: Array[Byte]    => java.lang.Byte.valueOf(array(index))
    case array: Array[Short]   => java.lang.Short.valueOf(array(index))
    case array: Array[Int]     => java.lang.Integer.valueOf(array(index))
    case array: Array[Long]    => java.lang.Long.valueOf(array(index))
    case array: Array[Float]   => java.lang.Float.valueOf(array(index))
    case array: Array[Double]  => java.lang.Double.valueOf(array(index))
    case _                     => mismatch(array)
  }

  def getBoolean(array: AnyRef, index: Int): Boolean = array match {
    case array: Array[Boolean] => array(index)
    case _                     => mismatch(array)
  }

  def getChar(array: AnyRef, index: Int): Char = array match {
    case array: Array[Char] => array(index)
    case _                  => mismatch(array)
  }

  def getByte(array: AnyRef, index: Int): Byte = array match {
    case array: Array[Byte] => array(index)
    case _                  => mismatch(array)
  }

  def getShort(array: AnyRef, index: Int): Short = array match {
    case array: Array[Short] => array(index)
    case array: Array[Byte]  => array(index)
    case _                   => mismatch(array)
  }

  def getInt(array: AnyRef, index: Int): Int = array match {
    case array: Array[Int]   => array(index)
    case array: Array[Char]  => array(index)
    case array: Array[Byte]  => array(index)
    case array: Array[Short] => array(index)
    case _                   => mismatch(array)
  }

  def getLong(array: AnyRef, index: Int): Long = array match {
    case array: Array[Long]  => array(index)
    case array: Array[Char]  => array(index)
    case array: Array[Byte]  => array(index)
    case array: Array[Short] => array(index)
    case array: Array[Int]   => array(index)
    case _                   => mismatch(array)
  }

  def getFloat(array: AnyRef, index: Int): Float = array match {
    case array: Array[Float] => array(index)
    case array: Array[Char]  => array(index)
    case array: Array[Byte]  => array(index)
    case array: Array[Short] => array(index)
    case array: Array[Int]   => array(index).toFloat
    case array: Array[Long]  => array(index).toFloat
    case _                   => mismatch(array)
  }

  def getDouble(array: AnyRef, index: Int): Double = array match {
    case array: Array[Double] => array(index)
    case array: Array[Char]   => array(index)
    case array: Array[Byte]   => array(index)
    case array: Array[Short]  => array(index)
    case array: Array[Int]    => array(index)
    case array: Array[Long]   => array(index).toDouble
    case array: Array[Float]  => array(index)
    case _                    => mismatch(array)
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
        case _              => mismatch(array)
      }
  }

  def setBoolean(array: AnyRef, index: Int, value: Boolean): Unit = array match {
    case array: Array[Boolean] => array(index) = value
    case _                     => mismatch(array)
  }

  def setChar(array: AnyRef, index: Int, value: Char): Unit = array match {
    case array: Array[Char]   => array(index) = value
    case array: Array[Int]    => array(index) = value
    case array: Array[Long]   => array(index) = value
    case array: Array[Float]  => array(index) = value
    case array: Array[Double] => array(index) = value
    case _                    => mismatch(array)
  }

  def setByte(array: AnyRef, index: Int, value: Byte): Unit = array match {
    case array: Array[Byte]   => array(index) = value
    case array: Array[Short]  => array(index) = value
    case array: Array[Int]    => array(index) = value
    case array: Array[Long]   => array(index) = value
    case array: Array[Float]  => array(index) = value
    case array: Array[Double] => array(index) = value
    case _                    => mismatch(array)
  }

  def setShort(array: AnyRef, index: Int, value: Short): Unit = array match {
    case array: Array[Short]  => array(index) = value
    case array: Array[Int]    => array(index) = value
    case array: Array[Long]   => array(index) = value
    case array: Array[Float]  => array(index) = value
    case array: Array[Double] => array(index) = value
    case _                    => mismatch(array)
  }

  def setInt(array: AnyRef, index: Int, value: Int): Unit = array match {
    case array: Array[Int]    => array(index) = value
    case array: Array[Long]   => array(index) = value
    case array: Array[Float]  => array(index) = value.toFloat
    case array: Array[Double] => array(index) = value
    case _                    => mismatch(array)
  }

  def setLong(array: AnyRef, index: Int, value: Long): Unit = array match {
    case array: Array[Long]   => array(index) = value
    case array: Array[Float]  => array(index) = value.toFloat
    case array: Array[Double] => array(index) = value.toDouble
    case _                    => mismatch(array)
  }

  def setFloat(array: AnyRef, index: Int, value: Float): Unit = array match {
    case array: Array[Float]  => array(index) = value
    case array: Array[Double] => array(index) = value
    case _                    => mismatch(array)
  }

  def setDouble(array: AnyRef, index: Int, value: Double): Unit = array match {
    case array: Array[Double] => array(index) = value
    case _                    => mismatch(array)
  }

  private def mismatch(array: AnyRef): Nothing = {
    array.getClass() // null check
    throw new IllegalArgumentException("argument type mismatch")
  }
}
