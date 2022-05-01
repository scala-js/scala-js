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

package java.util

import java.lang.{reflect => jlr}

import scala.scalajs.js

import scala.annotation.tailrec

import ScalaOps._

object Arrays {

  private object NaturalComparator extends Comparator[AnyRef] {
    @inline
    def compare(o1: AnyRef, o2: AnyRef): Int =
      o1.asInstanceOf[Comparable[AnyRef]].compareTo(o2)
  }

  @inline def ifNullUseNaturalComparator[T <: AnyRef](comparator: Comparator[_ >: T]): Comparator[_ >: T] =
    if (comparator == null) NaturalComparator
    else comparator

  /** A custom typeclass for the operations we need in `Arrays` to implement
   *  the algorithms generically.
   */
  private sealed trait ArrayOps[A] {
    def length(a: Array[A]): Int
    def get(a: Array[A], i: Int): A
    def set(a: Array[A], i: Int, v: A): Unit
  }

  /** A custom typeclass for the ability to create arrays of a given type. */
  private sealed trait ArrayCreateOps[A] {
    def create(length: Int): Array[A]
  }

  // ArrayOps and ArrayCreateOps instances for reference types

  private object ReusableAnyRefArrayOps extends ArrayOps[AnyRef] {
    @inline def length(a: Array[AnyRef]): Int = a.length
    @inline def get(a: Array[AnyRef], i: Int): AnyRef = a(i)
    @inline def set(a: Array[AnyRef], i: Int, v: AnyRef): Unit = a(i) = v
  }

  @inline
  private implicit def specificAnyRefArrayOps[A <: AnyRef]: ArrayOps[A] =
    ReusableAnyRefArrayOps.asInstanceOf[ArrayOps[A]]

  @inline
  private final class ClassArrayOps[A <: AnyRef](clazz: Class[_ <: Array[A]])
      extends ArrayCreateOps[A] {
    @inline def create(length: Int): Array[A] =
      createArrayOfClass(clazz, length)
  }

  @inline
  private final class TemplateArrayOps[A <: AnyRef](template: Array[A])
      extends ArrayCreateOps[A] {
    @inline def create(length: Int): Array[A] =
      createArrayOfClass(template.getClass(), length)
  }

  @inline
  private def createArrayOfClass[A <: AnyRef](clazz: Class[_ <: Array[A]], length: Int): Array[A] =
    jlr.Array.newInstance(clazz.getComponentType(), length).asInstanceOf[Array[A]]

  private implicit object AnyRefArrayCreateOps extends ArrayCreateOps[AnyRef] {
    @inline def create(length: Int): Array[AnyRef] = new Array[AnyRef](length)
  }

  /* ArrayOps and ArrayCreateOps instances for primitive types.
   *
   * With the exception of the one for Boolean, they also implement
   * `java.util.Comparator` for the same element type. In a perfect design, we
   * would define separate objects for that, but it would result in more
   * generated code for no good reason.
   */

  private implicit object BooleanArrayOps
      extends ArrayOps[Boolean] with ArrayCreateOps[Boolean] {
    @inline def length(a: Array[Boolean]): Int = a.length
    @inline def get(a: Array[Boolean], i: Int): Boolean = a(i)
    @inline def set(a: Array[Boolean], i: Int, v: Boolean): Unit = a(i) = v
    @inline def create(length: Int): Array[Boolean] = new Array[Boolean](length)
  }

  private implicit object CharArrayOps
      extends ArrayOps[Char] with ArrayCreateOps[Char] with Comparator[Char] {
    @inline def length(a: Array[Char]): Int = a.length
    @inline def get(a: Array[Char], i: Int): Char = a(i)
    @inline def set(a: Array[Char], i: Int, v: Char): Unit = a(i) = v
    @inline def create(length: Int): Array[Char] = new Array[Char](length)
    @inline def compare(x: Char, y: Char): Int = java.lang.Character.compare(x, y)
  }

  private implicit object ByteArrayOps
      extends ArrayOps[Byte] with ArrayCreateOps[Byte] with Comparator[Byte] {
    @inline def length(a: Array[Byte]): Int = a.length
    @inline def get(a: Array[Byte], i: Int): Byte = a(i)
    @inline def set(a: Array[Byte], i: Int, v: Byte): Unit = a(i) = v
    @inline def create(length: Int): Array[Byte] = new Array[Byte](length)
    @inline def compare(x: Byte, y: Byte): Int = java.lang.Byte.compare(x, y)
  }

  private implicit object ShortArrayOps
      extends ArrayOps[Short] with ArrayCreateOps[Short] with Comparator[Short] {
    @inline def length(a: Array[Short]): Int = a.length
    @inline def get(a: Array[Short], i: Int): Short = a(i)
    @inline def set(a: Array[Short], i: Int, v: Short): Unit = a(i) = v
    @inline def create(length: Int): Array[Short] = new Array[Short](length)
    @inline def compare(x: Short, y: Short): Int = java.lang.Short.compare(x, y)
  }

  private implicit object IntArrayOps
      extends ArrayOps[Int] with ArrayCreateOps[Int] with Comparator[Int] {
    @inline def length(a: Array[Int]): Int = a.length
    @inline def get(a: Array[Int], i: Int): Int = a(i)
    @inline def set(a: Array[Int], i: Int, v: Int): Unit = a(i) = v
    @inline def create(length: Int): Array[Int] = new Array[Int](length)
    @inline def compare(x: Int, y: Int): Int = java.lang.Integer.compare(x, y)
  }

  private implicit object LongArrayOps
      extends ArrayOps[Long] with ArrayCreateOps[Long] with Comparator[Long] {
    @inline def length(a: Array[Long]): Int = a.length
    @inline def get(a: Array[Long], i: Int): Long = a(i)
    @inline def set(a: Array[Long], i: Int, v: Long): Unit = a(i) = v
    @inline def create(length: Int): Array[Long] = new Array[Long](length)
    @inline def compare(x: Long, y: Long): Int = java.lang.Long.compare(x, y)
  }

  private implicit object FloatArrayOps
      extends ArrayOps[Float] with ArrayCreateOps[Float] with Comparator[Float] {
    @inline def length(a: Array[Float]): Int = a.length
    @inline def get(a: Array[Float], i: Int): Float = a(i)
    @inline def set(a: Array[Float], i: Int, v: Float): Unit = a(i) = v
    @inline def create(length: Int): Array[Float] = new Array[Float](length)
    @inline def compare(x: Float, y: Float): Int = java.lang.Float.compare(x, y)
  }

  private implicit object DoubleArrayOps
      extends ArrayOps[Double] with ArrayCreateOps[Double] with Comparator[Double] {
    @inline def length(a: Array[Double]): Int = a.length
    @inline def get(a: Array[Double], i: Int): Double = a(i)
    @inline def set(a: Array[Double], i: Int, v: Double): Unit = a(i) = v
    @inline def create(length: Int): Array[Double] = new Array[Double](length)
    @inline def compare(x: Double, y: Double): Int = java.lang.Double.compare(x, y)
  }

  // Implementation of the API

  @noinline def sort(a: Array[Int]): Unit =
    sortImpl(a)(IntArrayOps)

  @noinline def sort(a: Array[Int], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl(a, fromIndex, toIndex)(IntArrayOps)

  @noinline def sort(a: Array[Long]): Unit =
    sortImpl(a)(LongArrayOps)

  @noinline def sort(a: Array[Long], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl(a, fromIndex, toIndex)(LongArrayOps)

  @noinline def sort(a: Array[Short]): Unit =
    sortImpl(a)(ShortArrayOps)

  @noinline def sort(a: Array[Short], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl(a, fromIndex, toIndex)(ShortArrayOps)

  @noinline def sort(a: Array[Char]): Unit =
    sortImpl(a)(CharArrayOps)

  @noinline def sort(a: Array[Char], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl(a, fromIndex, toIndex)(CharArrayOps)

  @noinline def sort(a: Array[Byte]): Unit =
    sortImpl(a)(ByteArrayOps)

  @noinline def sort(a: Array[Byte], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl(a, fromIndex, toIndex)(ByteArrayOps)

  @noinline def sort(a: Array[Float]): Unit =
    sortImpl(a)(FloatArrayOps)

  @noinline def sort(a: Array[Float], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl(a, fromIndex, toIndex)(FloatArrayOps)

  @noinline def sort(a: Array[Double]): Unit =
    sortImpl(a)(DoubleArrayOps)

  @noinline def sort(a: Array[Double], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl(a, fromIndex, toIndex)(DoubleArrayOps)

  @noinline def sort(a: Array[AnyRef]): Unit =
    sortImpl(a)(NaturalComparator)

  @noinline def sort(a: Array[AnyRef], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl(a, fromIndex, toIndex)(NaturalComparator)

  @noinline def sort[T <: AnyRef](array: Array[T], comparator: Comparator[_ >: T]): Unit = {
    implicit val createOps = new TemplateArrayOps(array)
    sortImpl(array)(ifNullUseNaturalComparator(comparator))
  }

  @noinline def sort[T <: AnyRef](array: Array[T], fromIndex: Int, toIndex: Int,
      comparator: Comparator[_ >: T]): Unit = {
    implicit val createOps = new TemplateArrayOps(array)
    sortRangeImpl(array, fromIndex, toIndex)(ifNullUseNaturalComparator(comparator))
  }

  @inline
  private def sortRangeImpl[T](a: Array[T], fromIndex: Int, toIndex: Int)(
      comparator: Comparator[_ >: T])(
      implicit ops: ArrayOps[T], createOps: ArrayCreateOps[T]): Unit = {
    checkRangeIndices(a, fromIndex, toIndex)(ops)
    stableMergeSort[T](a, fromIndex, toIndex)(comparator)
  }

  @inline
  private def sortImpl[T](a: Array[T])(comparator: Comparator[_ >: T])(
      implicit ops: ArrayOps[T], createOps: ArrayCreateOps[T]): Unit = {
    stableMergeSort[T](a, 0, ops.length(a))(comparator)
  }

  private final val inPlaceSortThreshold = 16

  /** Sort array `a` with merge sort and insertion sort. */
  @inline
  private def stableMergeSort[T](a: Array[T], start: Int, end: Int)(
      comparator: Comparator[_ >: T])(
      implicit ops: ArrayOps[T], createOps: ArrayCreateOps[T]): Unit = {
    if (end - start > inPlaceSortThreshold)
      stableSplitMerge(a, createOps.create(ops.length(a)), start, end)(comparator)
    else
      insertionSort(a, start, end)(comparator)
  }

  @noinline
  private def stableSplitMerge[T](a: Array[T], temp: Array[T], start: Int,
      end: Int)(
      comparator: Comparator[_ >: T])(
      implicit ops: ArrayOps[T]): Unit = {
    val length = end - start
    if (length > inPlaceSortThreshold) {
      val middle = start + (length / 2)
      stableSplitMerge(a, temp, start, middle)(comparator)
      stableSplitMerge(a, temp, middle, end)(comparator)
      stableMerge(a, temp, start, middle, end)(comparator)
      System.arraycopy(temp, start, a, start, length)
    } else {
      insertionSort(a, start, end)(comparator)
    }
  }

  @inline
  private def stableMerge[T](a: Array[T], temp: Array[T], start: Int,
      middle: Int, end: Int)(
      comparator: Comparator[_ >: T])(
      implicit ops: ArrayOps[T]): Unit = {
    var outIndex = start
    var leftInIndex = start
    var rightInIndex = middle
    while (outIndex < end) {
      if (leftInIndex < middle &&
          (rightInIndex >= end || comparator.compare(ops.get(a, leftInIndex), ops.get(a, rightInIndex)) <= 0)) {
        ops.set(temp, outIndex, ops.get(a, leftInIndex))
        leftInIndex += 1
      } else {
        ops.set(temp, outIndex, ops.get(a, rightInIndex))
        rightInIndex += 1
      }
      outIndex += 1
    }
  }

  /* ArrayOps[T] and Comparator[T] might be slow especially for boxed
   * primitives, so use a binary search variant of insertion sort.
   * The caller must pass end >= start or math will fail. Also, start >= 0.
   */
  @noinline
  private final def insertionSort[T](a: Array[T], start: Int, end: Int)(
      comparator: Comparator[_ >: T])(
      implicit ops: ArrayOps[T]): Unit = {
    val n = end - start
    if (n >= 2) {
      val aStart = ops.get(a, start)
      val aStartPlusOne = ops.get(a, start + 1)
      if (comparator.compare(aStart, aStartPlusOne) > 0) {
        ops.set(a, start, aStartPlusOne)
        ops.set(a, start + 1, aStart)
      }

      var m = 2
      while (m < n) {
        // Speed up already-sorted case by checking last element first
        val next = ops.get(a, start + m)
        if (comparator.compare(next, ops.get(a, start + m - 1)) < 0) {
          var iA = start
          var iB = start + m - 1
          while (iB - iA > 1) {
            val ix = (iA + iB) >>> 1 // Use bit shift to get unsigned div by 2
            if (comparator.compare(next, ops.get(a, ix)) < 0)
              iB = ix
            else
              iA = ix
          }
          val ix = iA + (if (comparator.compare(next, ops.get(a, iA)) < 0) 0 else 1)
          var i = start + m
          while (i > ix) {
            ops.set(a, i, ops.get(a, i - 1))
            i -= 1
          }
          ops.set(a, ix, next)
        }
        m += 1
      }
    }
  }

  @noinline def binarySearch(a: Array[Long], key: Long): Int =
    binarySearchImpl(a, 0, a.length, key)(LongArrayOps)

  @noinline def binarySearch(a: Array[Long], startIndex: Int, endIndex: Int, key: Long): Int = {
    checkRangeIndices(a, startIndex, endIndex)
    binarySearchImpl(a, startIndex, endIndex, key)(LongArrayOps)
  }

  @noinline def binarySearch(a: Array[Int], key: Int): Int =
    binarySearchImpl(a, 0, a.length, key)(IntArrayOps)

  @noinline def binarySearch(a: Array[Int], startIndex: Int, endIndex: Int, key: Int): Int = {
    checkRangeIndices(a, startIndex, endIndex)
    binarySearchImpl(a, startIndex, endIndex, key)(IntArrayOps)
  }

  @noinline def binarySearch(a: Array[Short], key: Short): Int =
    binarySearchImpl(a, 0, a.length, key)(ShortArrayOps)

  @noinline def binarySearch(a: Array[Short], startIndex: Int, endIndex: Int, key: Short): Int = {
    checkRangeIndices(a, startIndex, endIndex)
    binarySearchImpl(a, startIndex, endIndex, key)(ShortArrayOps)
  }

  @noinline def binarySearch(a: Array[Char], key: Char): Int =
    binarySearchImpl(a, 0, a.length, key)(CharArrayOps)

  @noinline def binarySearch(a: Array[Char], startIndex: Int, endIndex: Int, key: Char): Int = {
    checkRangeIndices(a, startIndex, endIndex)
    binarySearchImpl(a, startIndex, endIndex, key)(CharArrayOps)
  }

  @noinline def binarySearch(a: Array[Byte], key: Byte): Int =
    binarySearchImpl(a, 0, a.length, key)(ByteArrayOps)

  @noinline def binarySearch(a: Array[Byte], startIndex: Int, endIndex: Int, key: Byte): Int = {
    checkRangeIndices(a, startIndex, endIndex)
    binarySearchImpl(a, startIndex, endIndex, key)(ByteArrayOps)
  }

  @noinline def binarySearch(a: Array[Double], key: Double): Int =
    binarySearchImpl(a, 0, a.length, key)(DoubleArrayOps)

  @noinline def binarySearch(a: Array[Double], startIndex: Int, endIndex: Int, key: Double): Int = {
    checkRangeIndices(a, startIndex, endIndex)
    binarySearchImpl(a, startIndex, endIndex, key)(DoubleArrayOps)
  }

  @noinline def binarySearch(a: Array[Float], key: Float): Int =
    binarySearchImpl(a, 0, a.length, key)(FloatArrayOps)

  @noinline def binarySearch(a: Array[Float], startIndex: Int, endIndex: Int, key: Float): Int = {
    checkRangeIndices(a, startIndex, endIndex)
    binarySearchImpl(a, startIndex, endIndex, key)(FloatArrayOps)
  }

  @noinline def binarySearch(a: Array[AnyRef], key: AnyRef): Int =
    binarySearchImpl(a, 0, a.length, key)(NaturalComparator)

  @noinline def binarySearch(a: Array[AnyRef], startIndex: Int, endIndex: Int, key: AnyRef): Int = {
    checkRangeIndices(a, startIndex, endIndex)
    binarySearchImpl(a, startIndex, endIndex, key)(NaturalComparator)
  }

  @noinline def binarySearch[T <: AnyRef](a: Array[T], key: T, c: Comparator[_ >: T]): Int =
    binarySearchImpl[T](a, 0, a.length, key)(ifNullUseNaturalComparator(c))

  @noinline def binarySearch[T <: AnyRef](a: Array[T], startIndex: Int, endIndex: Int, key: T,
      c: Comparator[_ >: T]): Int = {
    checkRangeIndices(a, startIndex, endIndex)
    binarySearchImpl[T](a, startIndex, endIndex, key)(ifNullUseNaturalComparator(c))
  }

  @inline
  @tailrec
  private def binarySearchImpl[T](a: Array[T], startIndex: Int, endIndex: Int,
      key: T)(
      comparator: Comparator[_ >: T])(
      implicit ops: ArrayOps[T]): Int = {
    if (startIndex == endIndex) {
      // Not found
      -startIndex - 1
    } else {
      // Indices are unsigned 31-bit integer, so this does not overflow
      val mid = (startIndex + endIndex) >>> 1
      val elem = ops.get(a, mid)
      val cmp = comparator.compare(key, elem)
      if (cmp < 0) {
        binarySearchImpl(a, startIndex, mid, key)(comparator)
      } else if (cmp == 0) {
        // Found
        mid
      } else {
        binarySearchImpl(a, mid + 1, endIndex, key)(comparator)
      }
    }
  }

  @noinline def equals(a: Array[Long], b: Array[Long]): Boolean =
    equalsImpl(a, b)

  @noinline def equals(a: Array[Int], b: Array[Int]): Boolean =
    equalsImpl(a, b)

  @noinline def equals(a: Array[Short], b: Array[Short]): Boolean =
    equalsImpl(a, b)

  @noinline def equals(a: Array[Char], b: Array[Char]): Boolean =
    equalsImpl(a, b)

  @noinline def equals(a: Array[Byte], b: Array[Byte]): Boolean =
    equalsImpl(a, b)

  @noinline def equals(a: Array[Boolean], b: Array[Boolean]): Boolean =
    equalsImpl(a, b)

  @noinline def equals(a: Array[Double], b: Array[Double]): Boolean =
    equalsImpl(a, b)

  @noinline def equals(a: Array[Float], b: Array[Float]): Boolean =
    equalsImpl(a, b)

  @noinline def equals(a: Array[AnyRef], b: Array[AnyRef]): Boolean =
    equalsImpl(a, b)

  @inline
  private def equalsImpl[T](a: Array[T], b: Array[T])(
      implicit ops: ArrayOps[T]): Boolean = {
    // scalastyle:off return
    if (a eq b)
      return true
    if (a == null || b == null)
      return false
    val len = ops.length(a)
    if (ops.length(b) != len)
      return false
    var i = 0
    while (i != len) {
      if (!ops.get(a, i).equals(ops.get(b, i)))
        return false
      i += 1
    }
    true
    // scalastyle:on return
  }

  @noinline def fill(a: Array[Long], value: Long): Unit =
    fillImpl(a, 0, a.length, value, checkIndices = false)

  @noinline def fill(a: Array[Long], fromIndex: Int, toIndex: Int, value: Long): Unit =
    fillImpl(a, fromIndex, toIndex, value)

  @noinline def fill(a: Array[Int], value: Int): Unit =
    fillImpl(a, 0, a.length, value, checkIndices = false)

  @noinline def fill(a: Array[Int], fromIndex: Int, toIndex: Int, value: Int): Unit =
    fillImpl(a, fromIndex, toIndex, value)

  @noinline def fill(a: Array[Short], value: Short): Unit =
    fillImpl(a, 0, a.length, value, checkIndices = false)

  @noinline def fill(a: Array[Short], fromIndex: Int, toIndex: Int, value: Short): Unit =
    fillImpl(a, fromIndex, toIndex, value)

  @noinline def fill(a: Array[Char], value: Char): Unit =
    fillImpl(a, 0, a.length, value, checkIndices = false)

  @noinline def fill(a: Array[Char], fromIndex: Int, toIndex: Int, value: Char): Unit =
    fillImpl(a, fromIndex, toIndex, value)

  @noinline def fill(a: Array[Byte], value: Byte): Unit =
    fillImpl(a, 0, a.length, value, checkIndices = false)

  @noinline def fill(a: Array[Byte], fromIndex: Int, toIndex: Int, value: Byte): Unit =
    fillImpl(a, fromIndex, toIndex, value)

  @noinline def fill(a: Array[Boolean], value: Boolean): Unit =
    fillImpl(a, 0, a.length, value, checkIndices = false)

  @noinline def fill(a: Array[Boolean], fromIndex: Int, toIndex: Int, value: Boolean): Unit =
    fillImpl(a, fromIndex, toIndex, value)

  @noinline def fill(a: Array[Double], value: Double): Unit =
    fillImpl(a, 0, a.length, value, checkIndices = false)

  @noinline def fill(a: Array[Double], fromIndex: Int, toIndex: Int, value: Double): Unit =
    fillImpl(a, fromIndex, toIndex, value)

  @noinline def fill(a: Array[Float], value: Float): Unit =
    fillImpl(a, 0, a.length, value, checkIndices = false)

  @noinline def fill(a: Array[Float], fromIndex: Int, toIndex: Int, value: Float): Unit =
    fillImpl(a, fromIndex, toIndex, value)

  @noinline def fill(a: Array[AnyRef], value: AnyRef): Unit =
    fillImpl(a, 0, a.length, value, checkIndices = false)

  @noinline def fill(a: Array[AnyRef], fromIndex: Int, toIndex: Int, value: AnyRef): Unit =
    fillImpl(a, fromIndex, toIndex, value)

  @inline
  private def fillImpl[T](a: Array[T], fromIndex: Int, toIndex: Int,
      value: T, checkIndices: Boolean = true)(
      implicit ops: ArrayOps[T]): Unit = {
    if (checkIndices)
      checkRangeIndices(a, fromIndex, toIndex)
    var i = fromIndex
    while (i != toIndex) {
      ops.set(a, i, value)
      i += 1
    }
  }

  @noinline def copyOf[T <: AnyRef](original: Array[T], newLength: Int): Array[T] = {
    implicit val tops = new TemplateArrayOps(original)
    copyOfImpl(original, newLength)
  }

  @noinline def copyOf[T <: AnyRef, U <: AnyRef](original: Array[U], newLength: Int,
      newType: Class[_ <: Array[T]]): Array[T] = {
    implicit val tops = new ClassArrayOps(newType)
    copyOfImpl(original, newLength)
  }

  @noinline def copyOf(original: Array[Byte], newLength: Int): Array[Byte] =
    copyOfImpl(original, newLength)

  @noinline def copyOf(original: Array[Short], newLength: Int): Array[Short] =
    copyOfImpl(original, newLength)

  @noinline def copyOf(original: Array[Int], newLength: Int): Array[Int] =
    copyOfImpl(original, newLength)

  @noinline def copyOf(original: Array[Long], newLength: Int): Array[Long] =
    copyOfImpl(original, newLength)

  @noinline def copyOf(original: Array[Char], newLength: Int): Array[Char] =
    copyOfImpl(original, newLength)

  @noinline def copyOf(original: Array[Float], newLength: Int): Array[Float] =
    copyOfImpl(original, newLength)

  @noinline def copyOf(original: Array[Double], newLength: Int): Array[Double] =
    copyOfImpl(original, newLength)

  @noinline def copyOf(original: Array[Boolean], newLength: Int): Array[Boolean] =
    copyOfImpl(original, newLength)

  @inline
  private def copyOfImpl[U, T](original: Array[U], newLength: Int)(
      implicit uops: ArrayOps[U], tops: ArrayCreateOps[T]): Array[T] = {
    checkArrayLength(newLength)
    val copyLength = Math.min(newLength, uops.length(original))
    val ret = tops.create(newLength)
    System.arraycopy(original, 0, ret, 0, copyLength)
    ret
  }

  @noinline def copyOfRange[T <: AnyRef](original: Array[T], from: Int, to: Int): Array[T] = {
    implicit val tops = new TemplateArrayOps(original)
    copyOfRangeImpl(original, from, to)
  }

  @noinline def copyOfRange[T <: AnyRef, U <: AnyRef](original: Array[U],
      from: Int, to: Int, newType: Class[_ <: Array[T]]): Array[T] = {
    implicit val tops = new ClassArrayOps(newType)
    copyOfRangeImpl(original, from, to)
  }

  @noinline def copyOfRange(original: Array[Byte], start: Int, end: Int): Array[Byte] =
    copyOfRangeImpl(original, start, end)

  @noinline def copyOfRange(original: Array[Short], start: Int, end: Int): Array[Short] =
    copyOfRangeImpl(original, start, end)

  @noinline def copyOfRange(original: Array[Int], start: Int, end: Int): Array[Int] =
    copyOfRangeImpl(original, start, end)

  @noinline def copyOfRange(original: Array[Long], start: Int, end: Int): Array[Long] =
    copyOfRangeImpl(original, start, end)

  @noinline def copyOfRange(original: Array[Char], start: Int, end: Int): Array[Char] =
    copyOfRangeImpl(original, start, end)

  @noinline def copyOfRange(original: Array[Float], start: Int, end: Int): Array[Float] =
    copyOfRangeImpl(original, start, end)

  @noinline def copyOfRange(original: Array[Double], start: Int, end: Int): Array[Double] =
    copyOfRangeImpl(original, start, end)

  @noinline def copyOfRange(original: Array[Boolean], start: Int, end: Int): Array[Boolean] =
    copyOfRangeImpl(original, start, end)

  @inline
  private def copyOfRangeImpl[T, U](original: Array[U], start: Int, end: Int)(
      implicit uops: ArrayOps[U], tops: ArrayCreateOps[T]): Array[T] = {
    if (start > end)
      throw new IllegalArgumentException("" + start + " > " + end)

    val len = uops.length(original)
    val retLength = end - start
    val copyLength = Math.min(retLength, len - start)
    val ret = tops.create(retLength)
    System.arraycopy(original, start, ret, 0, copyLength)
    ret
  }

  @inline private def checkArrayLength(len: Int): Unit = {
    if (len < 0)
      throw new NegativeArraySizeException
  }

  @noinline def asList[T <: AnyRef](a: Array[T]): List[T] = {
    new AbstractList[T] with RandomAccess {
      def size(): Int =
        a.length

      def get(index: Int): T =
        a(index)

      override def set(index: Int, element: T): T = {
        val ret = a(index)
        a(index) = element
        ret
      }
    }
  }

  @noinline def hashCode(a: Array[Long]): Int =
    hashCodeImpl(a)

  @noinline def hashCode(a: Array[Int]): Int =
    hashCodeImpl(a)

  @noinline def hashCode(a: Array[Short]): Int =
    hashCodeImpl(a)

  @noinline def hashCode(a: Array[Char]): Int =
    hashCodeImpl(a)

  @noinline def hashCode(a: Array[Byte]): Int =
    hashCodeImpl(a)

  @noinline def hashCode(a: Array[Boolean]): Int =
    hashCodeImpl(a)

  @noinline def hashCode(a: Array[Float]): Int =
    hashCodeImpl(a)

  @noinline def hashCode(a: Array[Double]): Int =
    hashCodeImpl(a)

  @noinline def hashCode(a: Array[AnyRef]): Int =
    hashCodeImpl(a)

  @inline
  private def hashCodeImpl[T](a: Array[T])(implicit ops: ArrayOps[T]): Int = {
    if (a == null) {
      0
    } else {
      var acc = 1
      val len = ops.length(a)
      var i = 0
      while (i != len) {
        acc = 31 * acc + Objects.hashCode(ops.get(a, i))
        i += 1
      }
      acc
    }
  }

  @noinline def deepHashCode(a: Array[AnyRef]): Int = {
    def rec(a: Array[AnyRef]): Int = {
      var acc = 1
      val len = a.length
      var i = 0
      while (i != len) {
        acc = 31 * acc + (a(i) match {
          case elem: Array[AnyRef]  => rec(elem)
          case elem: Array[Long]    => hashCode(elem)
          case elem: Array[Int]     => hashCode(elem)
          case elem: Array[Short]   => hashCode(elem)
          case elem: Array[Char]    => hashCode(elem)
          case elem: Array[Byte]    => hashCode(elem)
          case elem: Array[Boolean] => hashCode(elem)
          case elem: Array[Float]   => hashCode(elem)
          case elem: Array[Double]  => hashCode(elem)
          case elem                 => Objects.hashCode(elem)
        })
        i += 1
      }
      acc
    }

    if (a == null) 0
    else rec(a)
  }

  @noinline def deepEquals(a1: Array[AnyRef], a2: Array[AnyRef]): Boolean = {
    // scalastyle:off return
    if (a1 eq a2)
      return true
    if (a1 == null || a2 == null)
      return false
    val len = a1.length
    if (a2.length != len)
      return false
    var i = 0
    while (i != len) {
      if (!Objects.deepEquals(a1(i), a2(i)))
        return false
      i += 1
    }
    true
    // scalastyle:on return
  }

  @noinline def toString(a: Array[Long]): String =
    toStringImpl[Long](a)

  @noinline def toString(a: Array[Int]): String =
    toStringImpl[Int](a)

  @noinline def toString(a: Array[Short]): String =
    toStringImpl[Short](a)

  @noinline def toString(a: Array[Char]): String =
    toStringImpl[Char](a)

  @noinline def toString(a: Array[Byte]): String =
    toStringImpl[Byte](a)

  @noinline def toString(a: Array[Boolean]): String =
    toStringImpl[Boolean](a)

  @noinline def toString(a: Array[Float]): String =
    toStringImpl[Float](a)

  @noinline def toString(a: Array[Double]): String =
    toStringImpl[Double](a)

  @noinline def toString(a: Array[AnyRef]): String =
    toStringImpl[AnyRef](a)

  @inline
  private def toStringImpl[T](a: Array[T])(implicit ops: ArrayOps[T]): String = {
    if (a == null) {
      "null"
    } else {
      var result = "["
      val len = ops.length(a)
      var i = 0
      while (i != len) {
        if (i != 0)
          result += ", "
        result += ops.get(a, i)
        i += 1
      }
      result + "]"
    }
  }

  def deepToString(a: Array[AnyRef]): String = {
    /* The following array represents a set of the `Array[AnyRef]` that have
     * already been seen in the current recursion. We use a JS array instead of
     * a full-blown `HashSet` because it will likely stay very short (its size
     * is O(h) where h is the height of the tree of non-cyclical paths starting
     * at `a`), so the cost of using `System.identityHashCode` will probably
     * outweigh the benefits of the time complexity guarantees provided by a
     * hash-set.
     */
    val seen = js.Array[Array[AnyRef]]()

    @inline def wasSeen(a: Array[AnyRef]): Boolean = {
      // JavaScript's indexOf uses `===`
      seen.asInstanceOf[js.Dynamic].indexOf(a.asInstanceOf[js.Any]).asInstanceOf[Int] >= 0
    }

    def rec(a: Array[AnyRef]): String = {
      var result = "["
      val len = a.length
      var i = 0
      while (i != len) {
        if (i != 0)
          result += ", "
        a(i) match {
          case e: Array[AnyRef]  =>
            if ((e eq a) || wasSeen(e)) {
              result += "[...]"
            } else {
              seen.push(a)
              result += rec(e)
              seen.pop()
            }

          case e: Array[Long]    => result += toString(e)
          case e: Array[Int]     => result += toString(e)
          case e: Array[Short]   => result += toString(e)
          case e: Array[Byte]    => result += toString(e)
          case e: Array[Char]    => result += toString(e)
          case e: Array[Boolean] => result += toString(e)
          case e: Array[Float]   => result += toString(e)
          case e: Array[Double]  => result += toString(e)
          case e                 => result += e // handles null
        }
        i += 1
      }
      result + "]"
    }

    if (a == null) "null"
    else rec(a)
  }

  @inline
  private def checkRangeIndices[T](a: Array[T], start: Int, end: Int)(
      implicit ops: ArrayOps[T]): Unit = {
    if (start > end)
      throw new IllegalArgumentException("fromIndex(" + start + ") > toIndex(" + end + ")")

    // bounds checks
    if (start < 0)
      ops.get(a, start)

    if (end > 0)
      ops.get(a, end - 1)
  }
}
