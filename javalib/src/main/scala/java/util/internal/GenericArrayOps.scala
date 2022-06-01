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

package java.util.internal

import java.lang.{reflect => jlr}
import java.util.Comparator

/** Typeclasses for generic operations on `Array`s. */
object GenericArrayOps {

  /** A typeclass for operations to manipulate existing arrays. */
  sealed trait ArrayOps[A] {
    def length(a: Array[A]): Int
    def get(a: Array[A], i: Int): A
    def set(a: Array[A], i: Int, v: A): Unit
  }

  /** A typeclass for the ability to create arrays of a given type. */
  sealed trait ArrayCreateOps[A] {
    def create(length: Int): Array[A]
  }

  // ArrayOps and ArrayCreateOps instances for reference types

  private object ReusableAnyRefArrayOps extends ArrayOps[AnyRef] {
    @inline def length(a: Array[AnyRef]): Int = a.length
    @inline def get(a: Array[AnyRef], i: Int): AnyRef = a(i)
    @inline def set(a: Array[AnyRef], i: Int, v: AnyRef): Unit = a(i) = v
  }

  @inline
  implicit def specificAnyRefArrayOps[A <: AnyRef]: ArrayOps[A] =
    ReusableAnyRefArrayOps.asInstanceOf[ArrayOps[A]]

  @inline
  final class ClassArrayOps[A <: AnyRef](clazz: Class[_ <: Array[A]])
      extends ArrayCreateOps[A] {
    @inline def create(length: Int): Array[A] =
      createArrayOfClass(clazz, length)
  }

  @inline
  final class TemplateArrayOps[A <: AnyRef](template: Array[A])
      extends ArrayCreateOps[A] {
    @inline def create(length: Int): Array[A] =
      createArrayOfClass(template.getClass(), length)
  }

  @inline
  def createArrayOfClass[A <: AnyRef](clazz: Class[_ <: Array[A]], length: Int): Array[A] =
    jlr.Array.newInstance(clazz.getComponentType(), length).asInstanceOf[Array[A]]

  implicit object AnyRefArrayCreateOps extends ArrayCreateOps[AnyRef] {
    @inline def create(length: Int): Array[AnyRef] = new Array[AnyRef](length)
  }

  /* ArrayOps and ArrayCreateOps instances for primitive types.
   *
   * With the exception of the one for Boolean, they also implement
   * `java.util.Comparator` for the same element type. In a perfect design, we
   * would define separate objects for that, but it would result in more
   * generated code for no good reason.
   */

  implicit object BooleanArrayOps
      extends ArrayOps[Boolean] with ArrayCreateOps[Boolean] {
    @inline def length(a: Array[Boolean]): Int = a.length
    @inline def get(a: Array[Boolean], i: Int): Boolean = a(i)
    @inline def set(a: Array[Boolean], i: Int, v: Boolean): Unit = a(i) = v
    @inline def create(length: Int): Array[Boolean] = new Array[Boolean](length)
  }

  implicit object CharArrayOps
      extends ArrayOps[Char] with ArrayCreateOps[Char] with Comparator[Char] {
    @inline def length(a: Array[Char]): Int = a.length
    @inline def get(a: Array[Char], i: Int): Char = a(i)
    @inline def set(a: Array[Char], i: Int, v: Char): Unit = a(i) = v
    @inline def create(length: Int): Array[Char] = new Array[Char](length)
    @inline def compare(x: Char, y: Char): Int = java.lang.Character.compare(x, y)
  }

  implicit object ByteArrayOps
      extends ArrayOps[Byte] with ArrayCreateOps[Byte] with Comparator[Byte] {
    @inline def length(a: Array[Byte]): Int = a.length
    @inline def get(a: Array[Byte], i: Int): Byte = a(i)
    @inline def set(a: Array[Byte], i: Int, v: Byte): Unit = a(i) = v
    @inline def create(length: Int): Array[Byte] = new Array[Byte](length)
    @inline def compare(x: Byte, y: Byte): Int = java.lang.Byte.compare(x, y)
  }

  implicit object ShortArrayOps
      extends ArrayOps[Short] with ArrayCreateOps[Short] with Comparator[Short] {
    @inline def length(a: Array[Short]): Int = a.length
    @inline def get(a: Array[Short], i: Int): Short = a(i)
    @inline def set(a: Array[Short], i: Int, v: Short): Unit = a(i) = v
    @inline def create(length: Int): Array[Short] = new Array[Short](length)
    @inline def compare(x: Short, y: Short): Int = java.lang.Short.compare(x, y)
  }

  implicit object IntArrayOps
      extends ArrayOps[Int] with ArrayCreateOps[Int] with Comparator[Int] {
    @inline def length(a: Array[Int]): Int = a.length
    @inline def get(a: Array[Int], i: Int): Int = a(i)
    @inline def set(a: Array[Int], i: Int, v: Int): Unit = a(i) = v
    @inline def create(length: Int): Array[Int] = new Array[Int](length)
    @inline def compare(x: Int, y: Int): Int = java.lang.Integer.compare(x, y)
  }

  implicit object LongArrayOps
      extends ArrayOps[Long] with ArrayCreateOps[Long] with Comparator[Long] {
    @inline def length(a: Array[Long]): Int = a.length
    @inline def get(a: Array[Long], i: Int): Long = a(i)
    @inline def set(a: Array[Long], i: Int, v: Long): Unit = a(i) = v
    @inline def create(length: Int): Array[Long] = new Array[Long](length)
    @inline def compare(x: Long, y: Long): Int = java.lang.Long.compare(x, y)
  }

  implicit object FloatArrayOps
      extends ArrayOps[Float] with ArrayCreateOps[Float] with Comparator[Float] {
    @inline def length(a: Array[Float]): Int = a.length
    @inline def get(a: Array[Float], i: Int): Float = a(i)
    @inline def set(a: Array[Float], i: Int, v: Float): Unit = a(i) = v
    @inline def create(length: Int): Array[Float] = new Array[Float](length)
    @inline def compare(x: Float, y: Float): Int = java.lang.Float.compare(x, y)
  }

  implicit object DoubleArrayOps
      extends ArrayOps[Double] with ArrayCreateOps[Double] with Comparator[Double] {
    @inline def length(a: Array[Double]): Int = a.length
    @inline def get(a: Array[Double], i: Int): Double = a(i)
    @inline def set(a: Array[Double], i: Int, v: Double): Unit = a(i) = v
    @inline def create(length: Int): Array[Double] = new Array[Double](length)
    @inline def compare(x: Double, y: Double): Int = java.lang.Double.compare(x, y)
  }

}
