package java.util

import scala.scalajs.js
import scala.scalajs.runtime.SemanticsUtils

import scala.annotation.tailrec

import scala.reflect.ClassTag

import scala.collection.immutable

object Arrays {

  @inline
  private final implicit def naturalOrdering[T <: AnyRef]: Ordering[T] = {
    new Ordering[T] {
      def compare(x: T, y: T): Int = x.asInstanceOf[Comparable[T]].compareTo(y)
    }
  }

  @noinline def sort(a: Array[Int]): Unit =
    sortImpl(a)

  @noinline def sort(a: Array[Int], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl[Int](a, fromIndex, toIndex)

  @noinline def sort(a: Array[Long]): Unit =
    sortImpl(a)

  @noinline def sort(a: Array[Long], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl[Long](a, fromIndex, toIndex)

  @noinline def sort(a: Array[Short]): Unit =
    sortImpl(a)

  @noinline def sort(a: Array[Short], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl[Short](a, fromIndex, toIndex)

  @noinline def sort(a: Array[Char]): Unit =
    sortImpl(a)

  @noinline def sort(a: Array[Char], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl[Char](a, fromIndex, toIndex)

  @noinline def sort(a: Array[Byte]): Unit =
    sortImpl(a)

  @noinline def sort(a: Array[Byte], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl[Byte](a, fromIndex, toIndex)

  @noinline def sort(a: Array[Float]): Unit =
    sortImpl(a)

  @noinline def sort(a: Array[Float], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl[Float](a, fromIndex, toIndex)

  @noinline def sort(a: Array[Double]): Unit =
    sortImpl(a)

  @noinline def sort(a: Array[Double], fromIndex: Int, toIndex: Int): Unit =
    sortRangeImpl[Double](a, fromIndex, toIndex)

  @noinline def sort(a: Array[AnyRef]): Unit =
    sortAnyRefImpl(a)

  @noinline def sort(a: Array[AnyRef], fromIndex: Int, toIndex: Int): Unit =
    sortRangeAnyRefImpl(a, fromIndex, toIndex)

  @noinline def sort[T <: AnyRef](array: Array[T], comparator: Comparator[_ >: T]): Unit = {
    implicit val ord = toOrdering(comparator).asInstanceOf[Ordering[AnyRef]]
    sortAnyRefImpl(array.asInstanceOf[Array[AnyRef]])
  }

  @noinline def sort[T <: AnyRef](array: Array[T], fromIndex: Int, toIndex: Int,
      comparator: Comparator[_ >: T]): Unit = {
    implicit val ord = toOrdering(comparator).asInstanceOf[Ordering[AnyRef]]
    sortRangeAnyRefImpl(array.asInstanceOf[Array[AnyRef]], fromIndex, toIndex)
  }

  @inline
  private def sortRangeImpl[@specialized T: ClassTag](
      a: Array[T], fromIndex: Int, toIndex: Int)(implicit ord: Ordering[T]): Unit = {
    checkIndicesForCopyOfRange(a.length, fromIndex, toIndex)
    stableMergeSort[T](a, fromIndex, toIndex)
  }

  @inline
  private def sortRangeAnyRefImpl(a: Array[AnyRef], fromIndex: Int, toIndex: Int)(
      implicit ord: Ordering[AnyRef]): Unit = {
    checkIndicesForCopyOfRange(a.length, fromIndex, toIndex)
    stableMergeSortAnyRef(a, fromIndex, toIndex)
  }

  @inline
  private def sortImpl[@specialized T: ClassTag: Ordering](a: Array[T]): Unit =
    stableMergeSort[T](a, 0, a.length)

  @inline
  private def sortAnyRefImpl(a: Array[AnyRef])(implicit ord: Ordering[AnyRef]): Unit =
    stableMergeSortAnyRef(a, 0, a.length)

  private final val inPlaceSortThreshold = 16

  /** Sort array `a` with merge sort and insertion sort,
   *  using the Ordering on its elements.
   */
  @inline
  private def stableMergeSort[@specialized K: ClassTag](a: Array[K],
      start: Int, end: Int)(implicit ord: Ordering[K]): Unit = {
    if (end - start > inPlaceSortThreshold)
      stableSplitMerge(a, new Array[K](a.length), start, end)
    else
      insertionSort(a, start, end)
  }

  @noinline
  private def stableSplitMerge[@specialized K](a: Array[K], temp: Array[K],
      start: Int, end: Int)(implicit ord: Ordering[K]): Unit = {
    val length = end - start
    if (length > inPlaceSortThreshold) {
      val middle = start + (length / 2)
      stableSplitMerge(a, temp, start, middle)
      stableSplitMerge(a, temp, middle, end)
      stableMerge(a, temp, start, middle, end)
      System.arraycopy(temp, start, a, start, length)
    } else {
      insertionSort(a, start, end)
    }
  }

  @inline
  private def stableMerge[@specialized K](a: Array[K], temp: Array[K],
      start: Int, middle: Int, end: Int)(implicit ord: Ordering[K]): Unit = {
    var outIndex = start
    var leftInIndex = start
    var rightInIndex = middle
    while (outIndex < end) {
      if (leftInIndex < middle &&
          (rightInIndex >= end || ord.lteq(a(leftInIndex), a(rightInIndex)))) {
        temp(outIndex) = a(leftInIndex)
        leftInIndex += 1
      } else {
        temp(outIndex) = a(rightInIndex)
        rightInIndex += 1
      }
      outIndex += 1
    }
  }

  // Ordering[T] might be slow especially for boxed primitives, so use binary
  // search variant of insertion sort
  // Caller must pass end >= start or math will fail.  Also, start >= 0.
  @noinline
  private final def insertionSort[@specialized T](a: Array[T], start: Int,
      end: Int)(implicit ord: Ordering[T]): Unit = {
    val n = end - start
    if (n >= 2) {
      if (ord.compare(a(start), a(start + 1)) > 0) {
        val temp = a(start)
        a(start) = a(start + 1)
        a(start + 1) = temp
      }
      var m = 2
      while (m < n) {
        // Speed up already-sorted case by checking last element first
        val next = a(start + m)
        if (ord.compare(next, a(start + m - 1)) < 0) {
          var iA = start
          var iB = start + m - 1
          while (iB - iA > 1) {
            val ix = (iA + iB) >>> 1 // Use bit shift to get unsigned div by 2
            if (ord.compare(next, a(ix)) < 0)
              iB = ix
            else
              iA = ix
          }
          val ix = iA + (if (ord.compare(next, a(iA)) < 0) 0 else 1)
          var i = start + m
          while (i > ix) {
            a(i) = a(i - 1)
            i -= 1
          }
          a(ix) = next
        }
        m += 1
      }
    }
  }

  /** Sort array `a` with merge sort and insertion sort,
   *  using the Ordering on its elements.
   */
  @inline
  private def stableMergeSortAnyRef(a: Array[AnyRef], start: Int, end: Int)(
      implicit ord: Ordering[AnyRef]): Unit = {
    if (end - start > inPlaceSortThreshold)
      stableSplitMergeAnyRef(a, new Array(a.length), start, end)
    else
      insertionSortAnyRef(a, start, end)
  }

  @noinline
  private def stableSplitMergeAnyRef(a: Array[AnyRef], temp: Array[AnyRef],
      start: Int, end: Int)(implicit ord: Ordering[AnyRef]): Unit = {
    val length = end - start
    if (length > inPlaceSortThreshold) {
      val middle = start + (length / 2)
      stableSplitMergeAnyRef(a, temp, start, middle)
      stableSplitMergeAnyRef(a, temp, middle, end)
      stableMergeAnyRef(a, temp, start, middle, end)
      System.arraycopy(temp, start, a, start, length)
    } else {
      insertionSortAnyRef(a, start, end)
    }
  }

  @inline
  private def stableMergeAnyRef(a: Array[AnyRef], temp: Array[AnyRef],
      start: Int, middle: Int, end: Int)(implicit ord: Ordering[AnyRef]): Unit = {
    var outIndex = start
    var leftInIndex = start
    var rightInIndex = middle
    while (outIndex < end) {
      if (leftInIndex < middle &&
          (rightInIndex >= end || ord.lteq(a(leftInIndex), a(rightInIndex)))) {
        temp(outIndex) = a(leftInIndex)
        leftInIndex += 1
      } else {
        temp(outIndex) = a(rightInIndex)
        rightInIndex += 1
      }
      outIndex += 1
    }
  }

  @noinline
  private final def insertionSortAnyRef(a: Array[AnyRef], start: Int, end: Int)(
      implicit ord: Ordering[AnyRef]): Unit = {
    val n = end - start
    if (n >= 2) {
      if (ord.compare(a(start), a(start + 1)) > 0) {
        val temp = a(start)
        a(start) = a(start + 1)
        a(start + 1) = temp
      }
      var m = 2
      while (m < n) {
        // Speed up already-sorted case by checking last element first
        val next = a(start + m)
        if (ord.compare(next, a(start + m - 1)) < 0) {
          var iA = start
          var iB = start + m - 1
          while (iB - iA > 1) {
            val ix = (iA + iB) >>> 1 // Use bit shift to get unsigned div by 2
            if (ord.compare(next, a(ix)) < 0)
              iB = ix
            else
              iA = ix
          }
          val ix = iA + (if (ord.compare(next, a(iA)) < 0) 0 else 1)
          var i = start + m
          while (i > ix) {
            a(i) = a(i - 1)
            i -= 1
          }
          a(ix) = next
        }
        m += 1
      }
    }
  }

  @noinline def binarySearch(a: Array[Long], key: Long): Int =
    binarySearchImpl[Long](a, 0, a.length, key, _ < _)

  @noinline def binarySearch(a: Array[Long], startIndex: Int, endIndex: Int, key: Long): Int = {
    checkRangeIndices(a.length, startIndex, endIndex)
    binarySearchImpl[Long](a, startIndex, endIndex, key, _ < _)
  }

  @noinline def binarySearch(a: Array[Int], key: Int): Int =
    binarySearchImpl[Int](a, 0, a.length, key, _ < _)

  @noinline def binarySearch(a: Array[Int], startIndex: Int, endIndex: Int, key: Int): Int = {
    checkRangeIndices(a.length, startIndex, endIndex)
    binarySearchImpl[Int](a, startIndex, endIndex, key, _ < _)
  }

  @noinline def binarySearch(a: Array[Short], key: Short): Int =
    binarySearchImpl[Short](a, 0, a.length, key, _ < _)

  @noinline def binarySearch(a: Array[Short], startIndex: Int, endIndex: Int, key: Short): Int = {
    checkRangeIndices(a.length, startIndex, endIndex)
    binarySearchImpl[Short](a, startIndex, endIndex, key, _ < _)
  }

  @noinline def binarySearch(a: Array[Char], key: Char): Int =
    binarySearchImpl[Char](a, 0, a.length, key, _ < _)

  @noinline def binarySearch(a: Array[Char], startIndex: Int, endIndex: Int, key: Char): Int = {
    checkRangeIndices(a.length, startIndex, endIndex)
    binarySearchImpl[Char](a, startIndex, endIndex, key, _ < _)
  }

  @noinline def binarySearch(a: Array[Byte], key: Byte): Int =
    binarySearchImpl[Byte](a, 0, a.length, key, _ < _)

  @noinline def binarySearch(a: Array[Byte], startIndex: Int, endIndex: Int, key: Byte): Int = {
    checkRangeIndices(a.length, startIndex, endIndex)
    binarySearchImpl[Byte](a, startIndex, endIndex, key, _ < _)
  }

  @noinline def binarySearch(a: Array[Double], key: Double): Int =
    binarySearchImpl[Double](a, 0, a.length, key, _ < _)

  @noinline def binarySearch(a: Array[Double], startIndex: Int, endIndex: Int, key: Double): Int = {
    checkRangeIndices(a.length, startIndex, endIndex)
    binarySearchImpl[Double](a, startIndex, endIndex, key, _ < _)
  }

  @noinline def binarySearch(a: Array[Float], key: Float): Int =
    binarySearchImpl[Float](a, 0, a.length, key, _ < _)

  @noinline def binarySearch(a: Array[Float], startIndex: Int, endIndex: Int, key: Float): Int = {
    checkRangeIndices(a.length, startIndex, endIndex)
    binarySearchImpl[Float](a, startIndex, endIndex, key, _ < _)
  }

  @noinline def binarySearch(a: Array[AnyRef], key: AnyRef): Int =
    binarySearchImplRef(a, 0, a.length, key)

  @noinline def binarySearch(a: Array[AnyRef], startIndex: Int, endIndex: Int, key: AnyRef): Int = {
    checkRangeIndices(a.length, startIndex, endIndex)
    binarySearchImplRef(a, startIndex, endIndex, key)
  }

  @noinline def binarySearch[T](a: Array[T], key: T, c: Comparator[_ >: T]): Int =
    binarySearchImpl[T](a, 0, a.length, key, (a, b) => c.compare(a, b) < 0)

  @noinline def binarySearch[T](a: Array[T], startIndex: Int, endIndex: Int, key: T,
      c: Comparator[_ >: T]): Int = {
    checkRangeIndices(a.length, startIndex, endIndex)
    binarySearchImpl[T](a, startIndex, endIndex, key, (a, b) => c.compare(a, b) < 0)
  }

  @inline
  @tailrec
  private def binarySearchImpl[T](a: Array[T],
      startIndex: Int, endIndex: Int, key: T, lt: (T, T) => Boolean): Int = {
    if (startIndex == endIndex) {
      // Not found
      -startIndex - 1
    } else {
      // Indices are unsigned 31-bit integer, so this does not overflow
      val mid = (startIndex + endIndex) >>> 1
      val elem = a(mid)
      if (lt(key, elem)) {
        binarySearchImpl(a, startIndex, mid, key, lt)
      } else if (key == elem) {
        // Found
        mid
      } else {
        binarySearchImpl(a, mid + 1, endIndex, key, lt)
      }
    }
  }

  @inline
  @tailrec
  def binarySearchImplRef(a: Array[AnyRef],
      startIndex: Int, endIndex: Int, key: AnyRef): Int = {
    if (startIndex == endIndex) {
      // Not found
      -startIndex - 1
    } else {
      // Indices are unsigned 31-bit integer, so this does not overflow
      val mid = (startIndex + endIndex) >>> 1
      val cmp = key.asInstanceOf[Comparable[AnyRef]].compareTo(a(mid))
      if (cmp < 0) {
        binarySearchImplRef(a, startIndex, mid, key)
      } else if (cmp == 0) {
        // Found
        mid
      } else {
        binarySearchImplRef(a, mid + 1, endIndex, key)
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
  private def equalsImpl[T](a: Array[T], b: Array[T]): Boolean = {
    (a eq b) || (a != null && b != null && a.length == b.length &&
        a.indices.forall(i => a(i) == b(i)))
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
      value: T, checkIndices: Boolean = true): Unit = {
    if (checkIndices)
      checkRangeIndices(a.length, fromIndex, toIndex)
    var i = fromIndex
    while (i != toIndex) {
      a(i) = value
      i += 1
    }
  }

  @noinline def copyOf[T <: AnyRef](original: Array[T], newLength: Int): Array[T] = {
    implicit val tagT = ClassTag[T](original.getClass.getComponentType)
    copyOfImpl(original, newLength)
  }

  @noinline def copyOf[T <: AnyRef, U <: AnyRef](original: Array[U], newLength: Int,
      newType: Class[_ <: Array[T]]): Array[T] = {
    implicit val tag = ClassTag[T](newType.getComponentType)
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
  private def copyOfImpl[U, T: ClassTag](original: Array[U], newLength: Int): Array[T] = {
    checkArrayLength(newLength)
    val copyLength = Math.min(newLength, original.length)
    val ret = new Array[T](newLength)
    System.arraycopy(original, 0, ret, 0, copyLength)
    ret
  }

  @noinline def copyOfRange[T <: AnyRef](original: Array[T], from: Int, to: Int): Array[T] = {
    copyOfRangeImpl[T](original, from, to)(ClassTag(original.getClass.getComponentType)).asInstanceOf[Array[T]]
  }

  @noinline def copyOfRange[T <: AnyRef, U <: AnyRef](original: Array[U], from: Int, to: Int,
      newType: Class[_ <: Array[T]]): Array[T] = {
    copyOfRangeImpl[AnyRef](original.asInstanceOf[Array[AnyRef]], from, to)(
        ClassTag(newType.getComponentType)).asInstanceOf[Array[T]]
  }

  @noinline def copyOfRange(original: Array[Byte], start: Int, end: Int): Array[Byte] =
    copyOfRangeImpl[Byte](original, start, end)

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
  private def copyOfRangeImpl[T: ClassTag](original: Array[T],
      start: Int, end: Int): Array[T] = {
    checkIndicesForCopyOfRange(original.length, start, end)
    val retLength = end - start
    val copyLength = Math.min(retLength, original.length - start)
    val ret = new Array[T](retLength)
    System.arraycopy(original, start, ret, 0, copyLength)
    ret
  }

  @inline private def checkArrayLength(len: Int): Unit = {
    if (len < 0)
      throw new NegativeArraySizeException
  }

  @inline private def checkIndicesForCopyOfRange(
      len: Int, start: Int, end: Int): Unit = {
    if (start > end)
      throw new IllegalArgumentException(start + " > " + end)
    SemanticsUtils.arrayIndexOutOfBoundsCheck(start < 0 || start > len,
        new ArrayIndexOutOfBoundsException)
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
    hashCodeImpl[Long](a)

  @noinline def hashCode(a: Array[Int]): Int =
    hashCodeImpl[Int](a)

  @noinline def hashCode(a: Array[Short]): Int =
    hashCodeImpl[Short](a)

  @noinline def hashCode(a: Array[Char]): Int =
    hashCodeImpl[Char](a)

  @noinline def hashCode(a: Array[Byte]): Int =
    hashCodeImpl[Byte](a)

  @noinline def hashCode(a: Array[Boolean]): Int =
    hashCodeImpl[Boolean](a)

  @noinline def hashCode(a: Array[Float]): Int =
    hashCodeImpl[Float](a)

  @noinline def hashCode(a: Array[Double]): Int =
    hashCodeImpl[Double](a)

  @noinline def hashCode(a: Array[AnyRef]): Int =
    hashCodeImpl[AnyRef](a)

  @inline
  private def hashCodeImpl[T](a: Array[T],
      elementHashCode: T => Int = (x: T) => x.asInstanceOf[AnyRef].hashCode): Int = {
    if (a == null) 0
    else a.foldLeft(1)((acc, x) => 31*acc + (if (x == null) 0 else elementHashCode(x)))
  }

  @noinline def deepHashCode(a: Array[AnyRef]): Int = {
    @inline
    def getHash(elem: AnyRef): Int = {
      elem match {
        case elem: Array[AnyRef]  => deepHashCode(elem)
        case elem: Array[Long]    => hashCode(elem)
        case elem: Array[Int]     => hashCode(elem)
        case elem: Array[Short]   => hashCode(elem)
        case elem: Array[Char]    => hashCode(elem)
        case elem: Array[Byte]    => hashCode(elem)
        case elem: Array[Boolean] => hashCode(elem)
        case elem: Array[Float]   => hashCode(elem)
        case elem: Array[Double]  => hashCode(elem)
        case _                    => elem.hashCode
      }
    }
    hashCodeImpl(a, getHash)
  }

  @noinline def deepEquals(a1: Array[AnyRef], a2: Array[AnyRef]): Boolean = {
    if (a1 eq a2) true
    else if (a1 == null || a2 == null || a1.length != a2.length) false
    else a1.indices.forall(i => Objects.deepEquals(a1(i), a2(i)))
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
  private def toStringImpl[T](a: Array[T]): String = {
    if (a == null) "null"
    else a.mkString("[", ", ", "]")
  }

  @noinline def deepToString(a: Array[AnyRef]): String =
    deepToStringImpl(a, immutable.HashSet.empty[AsRef])

  private def deepToStringImpl(a: Array[AnyRef], branch: immutable.Set[AsRef]): String = {
    @inline
    def valueToString(e: AnyRef): String = {
      if (e == null) "null"
      else {
        e match {
          case e: Array[AnyRef]  => deepToStringImpl(e, branch + new AsRef(a))
          case e: Array[Long]    => toString(e)
          case e: Array[Int]     => toString(e)
          case e: Array[Short]   => toString(e)
          case e: Array[Byte]    => toString(e)
          case e: Array[Char]    => toString(e)
          case e: Array[Boolean] => toString(e)
          case e: Array[Float]   => toString(e)
          case e: Array[Double]  => toString(e)
          case _                 => String.valueOf(e)
        }
      }
    }
    if (a == null) "null"
    else if (branch.contains(new AsRef(a))) "[...]"
    else a.iterator.map(valueToString).mkString("[", ", ", "]")
  }

  @inline
  private def checkRangeIndices(length: Int, start: Int, end: Int): Unit = {
    if (start > end)
      throw new IllegalArgumentException("fromIndex(" + start + ") > toIndex(" + end + ")")
    SemanticsUtils.arrayIndexOutOfBoundsCheck(start < 0,
        new ArrayIndexOutOfBoundsException("Array index out of range: " + start))
    SemanticsUtils.arrayIndexOutOfBoundsCheck(end > length,
        new ArrayIndexOutOfBoundsException("Array index out of range: " + end))
  }

  @inline
  private def toOrdering[T](cmp: Comparator[T]): Ordering[T] = {
    new Ordering[T] {
      def compare(x: T, y: T): Int = cmp.compare(x, y)
    }
  }

  private final class AsRef(val inner: AnyRef) {
    override def hashCode(): Int =
      System.identityHashCode(inner)

    override def equals(obj: Any): Boolean = {
      obj match {
        case obj: AsRef => obj.inner eq inner
        case _          => false
      }
    }
  }
}
