package java.util

import scala.annotation.tailrec

object Arrays {
  def sort[T <: Object](array: Array[Object], comparator: Comparator[T]): Unit = {
    scala.util.Sorting.stableSort[Object](array,
        (a: Object, b: Object) =>
          comparator.compare(a.asInstanceOf[T], b.asInstanceOf[T]) < 0)
  }

  def fill(a: Array[Boolean], value: Boolean): Unit =
    fillImpl(a, value)

  def fill(a: Array[Boolean], fromIndex: Int, toIndex: Int, value: Boolean): Unit =
    fillImpl(a, fromIndex, toIndex, value)

  def fill(a: Array[Byte], value: Byte): Unit =
    fillImpl(a, value)

  def fill(a: Array[Byte], fromIndex: Int, toIndex: Int, value: Byte): Unit =
    fillImpl(a, fromIndex, toIndex, value)

  def fill(a: Array[Char], value: Char): Unit =
    fillImpl(a, value)

  def fill(a: Array[Char], fromIndex: Int, toIndex: Int, value: Char): Unit =
    fillImpl(a, fromIndex, toIndex, value)

  def fill(a: Array[Short], value: Short): Unit =
    fillImpl(a, value)

  def fill(a: Array[Short], fromIndex: Int, toIndex: Int, value: Short): Unit =
    fillImpl(a, fromIndex, toIndex, value)

  def fill(a: Array[Int], value: Int): Unit =
    fillImpl(a, value)

  def fill(a: Array[Int], fromIndex: Int, toIndex: Int, value: Int): Unit =
    fillImpl(a, fromIndex, toIndex, value)

  def fill(a: Array[Long], value: Long): Unit =
    fillImpl(a, value)

  def fill(a: Array[Long], fromIndex: Int, toIndex: Int, value: Long): Unit =
    fillImpl(a, fromIndex, toIndex, value)

  def fill(a: Array[Float], value: Float): Unit =
    fillImpl(a, value)

  def fill(a: Array[Float], fromIndex: Int, toIndex: Int, value: Float): Unit =
    fillImpl(a, fromIndex, toIndex, value)

  def fill(a: Array[Double], value: Double): Unit =
    fillImpl(a, value)

  def fill(a: Array[Double], fromIndex: Int, toIndex: Int, value: Double): Unit =
    fillImpl(a, fromIndex, toIndex, value)

  private def fillImpl[@specialized T](a: Array[T], value: T): Unit = {
    var i = 0
    while (i != a.length) {
      a(i) = value
      i += 1
    }
  }

  private def fillImpl[@specialized T](a: Array[T],
      fromIndex: Int, toIndex: Int, value: T): Unit = {
    if (fromIndex > toIndex)
      throw new IllegalArgumentException
    if (fromIndex < 0 || toIndex > a.length)
      throw new ArrayIndexOutOfBoundsException

    var i = fromIndex
    while (i != toIndex) {
      a(i) = value
      i += 1
    }
  }

  def fill(a: Array[AnyRef], value: AnyRef): Unit = {
    var i = 0
    while (i < a.length) {
      a(i) = value
      i += 1
    }
  }

  def fill(a: Array[AnyRef],
      fromIndex: Int, toIndex: Int, value: AnyRef): Unit = {
    if (fromIndex > toIndex)
      throw new IllegalArgumentException
    if (fromIndex < 0 || toIndex > a.length)
      throw new ArrayIndexOutOfBoundsException

    var i = fromIndex
    while (i < toIndex) {
      a(i) = value
      i += 1
    }
  }

  @inline private def checkIndexForBinarySearch(
      length: Int, start: Int, end: Int): Unit = {
    if (start > end)
      throw new IllegalArgumentException("fromIndex(" + start + ") > toIndex(" + end + ")")
    if (start < 0)
      throw new ArrayIndexOutOfBoundsException("Array index out of range: " + start)
    if (end > length)
      throw new ArrayIndexOutOfBoundsException("Array index out of range: " + end)
  }

  def binarySearch(a: Array[Char], key: Char): Int =
    binarySearchImpl[Char](a, 0, a.length, key, _ < _)

  def binarySearch(a: Array[Char],
      startIndex: Int, endIndex: Int, key: Char): Int = {
    checkIndexForBinarySearch(a.length, startIndex, endIndex)
    binarySearchImpl[Char](a, startIndex, endIndex, key, _ < _)
  }

  def binarySearch(a: Array[Short], key: Short): Int =
    binarySearchImpl[Short](a, 0, a.length, key, _ < _)

  def binarySearch(a: Array[Short],
      startIndex: Int, endIndex: Int, key: Short): Int = {
    checkIndexForBinarySearch(a.length, startIndex, endIndex)
    binarySearchImpl[Short](a, startIndex, endIndex, key, _ < _)
  }

  def binarySearch(a: Array[Int], key: Int): Int =
    binarySearchImpl[Int](a, 0, a.length, key, _ < _)

  def binarySearch(a: Array[Int],
      startIndex: Int, endIndex: Int, key: Int): Int = {
    checkIndexForBinarySearch(a.length, startIndex, endIndex)
    binarySearchImpl[Int](a, startIndex, endIndex, key, _ < _)
  }

  def binarySearch(a: Array[Long], key: Long): Int =
    binarySearchImpl[Long](a, 0, a.length, key, _ < _)

  def binarySearch(a: Array[Long],
      startIndex: Int, endIndex: Int, key: Long): Int = {
    checkIndexForBinarySearch(a.length, startIndex, endIndex)
    binarySearchImpl[Long](a, startIndex, endIndex, key, _ < _)
  }

  def binarySearch(a: Array[Float], key: Float): Int =
    binarySearchImpl[Float](a, 0, a.length, key, _ < _)

  def binarySearch(a: Array[Float],
      startIndex: Int, endIndex: Int, key: Float): Int = {
    checkIndexForBinarySearch(a.length, startIndex, endIndex)
    binarySearchImpl[Float](a, startIndex, endIndex, key, _ < _)
  }

  def binarySearch(a: Array[Double], key: Double): Int =
    binarySearchImpl[Double](a, 0, a.length, key, _ < _)

  def binarySearch(a: Array[Double],
      startIndex: Int, endIndex: Int, key: Double): Int = {
    checkIndexForBinarySearch(a.length, startIndex, endIndex)
    binarySearchImpl[Double](a, startIndex, endIndex, key, _ < _)
  }

  @inline
  @tailrec
  private def binarySearchImpl[@specialized T](a: Array[T],
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

  def binarySearch(a: Array[AnyRef], key: AnyRef): Int =
    binarySearchImplRef(a, 0, a.length, key)

  def binarySearch(a: Array[AnyRef],
      startIndex: Int, endIndex: Int, key: AnyRef): Int = {
    checkIndexForBinarySearch(a.length, startIndex, endIndex)
    binarySearchImplRef(a, startIndex, endIndex, key)
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

  def copyOf(original: Array[Boolean], newLength: Int): Array[Boolean] =
    copyOfImpl(original, newLength, new Array(_))

  def copyOfRange(original: Array[Boolean], start: Int, end: Int): Array[Boolean] =
    copyOfRangeImpl(original, start, end, new Array(_))

  def copyOf(original: Array[Char], newLength: Int): Array[Char] =
    copyOfImpl(original, newLength, new Array(_))

  def copyOfRange(original: Array[Char], start: Int, end: Int): Array[Char] =
    copyOfRangeImpl(original, start, end, new Array(_))

  def copyOf(original: Array[Byte], newLength: Int): Array[Byte] =
    copyOfImpl(original, newLength, new Array(_))

  def copyOfRange(original: Array[Byte], start: Int, end: Int): Array[Byte] =
    copyOfRangeImpl(original, start, end, new Array(_))

  def copyOf(original: Array[Short], newLength: Int): Array[Short] =
    copyOfImpl(original, newLength, new Array(_))

  def copyOfRange(original: Array[Short], start: Int, end: Int): Array[Short] =
    copyOfRangeImpl(original, start, end, new Array(_))

  def copyOf(original: Array[Int], newLength: Int): Array[Int] =
    copyOfImpl(original, newLength, new Array(_))

  def copyOfRange(original: Array[Int], start: Int, end: Int): Array[Int] =
    copyOfRangeImpl(original, start, end, new Array(_))

  def copyOf(original: Array[Long], newLength: Int): Array[Long] =
    copyOfImpl(original, newLength, new Array(_))

  def copyOfRange(original: Array[Long], start: Int, end: Int): Array[Long] =
    copyOfRangeImpl(original, start, end, new Array(_))

  def copyOf(original: Array[Float], newLength: Int): Array[Float] =
    copyOfImpl(original, newLength, new Array(_))

  def copyOfRange(original: Array[Float], start: Int, end: Int): Array[Float] =
    copyOfRangeImpl(original, start, end, new Array(_))

  def copyOf(original: Array[Double], newLength: Int): Array[Double] =
    copyOfImpl(original, newLength, new Array(_))

  def copyOfRange(original: Array[Double], start: Int, end: Int): Array[Double] =
    copyOfRangeImpl(original, start, end, new Array(_))

  @inline private def copyOfImpl[@specialized T](original: Array[T],
      newLength: Int, newArray: Int => Array[T]): Array[T] = {
    checkArrayLength(newLength)
    val copyLength = Math.min(newLength, original.length)
    val ret = newArray(newLength)
    System.arraycopy(original, 0, ret, 0, copyLength)
    ret
  }

  @inline private def copyOfRangeImpl[@specialized T](original: Array[T],
      start: Int, end: Int, newArray: Int => Array[T]): Array[T] = {
    checkIndicesForCopyOfRange(original.length, start, end)
    val retLength = end - start
    val copyLength = Math.min(retLength, original.length - start)
    val ret = newArray(retLength)
    System.arraycopy(original, start, ret, 0, copyLength)
    ret
  }

  def copyOf(original: Array[AnyRef], newLength: Int): Array[AnyRef] = {
    checkArrayLength(newLength)
    val copyLength = Math.min(newLength, original.length)
    val ret = java.lang.reflect.Array.newInstance(
        original.getClass.getComponentType, newLength).asInstanceOf[Array[AnyRef]]
    System.arraycopy(original, 0, ret, 0, copyLength)
    ret
  }

  def copyOfRange(original: Array[AnyRef], start: Int, end: Int): Array[AnyRef] = {
    checkIndicesForCopyOfRange(original.length, start, end)
    val retLength = end - start
    val copyLength = Math.min(retLength, original.length - start)
    val ret = java.lang.reflect.Array.newInstance(
        original.getClass.getComponentType, retLength).asInstanceOf[Array[AnyRef]]
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
    if (start < 0 || start > len)
      throw new ArrayIndexOutOfBoundsException
  }

  def hashCode(a: Array[Boolean]): Int = {
    if (a == null) 0
    else a.foldLeft(1)((acc, x) => 31*acc + new java.lang.Boolean(x).hashCode)
  }

  def hashCode(a: Array[Char]): Int = {
    if (a == null) 0
    else a.foldLeft(1)((acc, x) => 31*acc + new java.lang.Character(x).hashCode)
  }

  def hashCode(a: Array[Byte]): Int = {
    if (a == null) 0
    else a.foldLeft(1)((acc, x) => 31*acc + new java.lang.Byte(x).hashCode)
  }

  def hashCode(a: Array[Short]): Int = {
    if (a == null) 0
    else a.foldLeft(1)((acc, x) => 31*acc + new java.lang.Short(x).hashCode)
  }

  def hashCode(a: Array[Int]): Int = {
    if (a == null) 0
    else a.foldLeft(1)((acc, x) => 31*acc + new java.lang.Integer(x).hashCode)
  }

  def hashCode(a: Array[Long]): Int = {
    if (a == null) 0
    else a.foldLeft(1)((acc, x) => 31*acc + new java.lang.Long(x).hashCode)
  }

  def hashCode(a: Array[Float]): Int = {
    if (a == null) 0
    else a.foldLeft(1)((acc, x) => 31*acc + new java.lang.Float(x).hashCode)
  }

  def hashCode(a: Array[Double]): Int = {
    if (a == null) 0
    else a.foldLeft(1)((acc, x) => 31*acc + new java.lang.Double(x).hashCode)
  }

  def hashCode(a: Array[AnyRef]): Int = {
    if (a == null) 0
    else a.foldLeft(1)((acc, x) => 31*acc + (if (x == null) 0 else x.hashCode))
  }

  def equals(a: Array[Boolean], b: Array[Boolean]): Boolean =
    (a eq b) || (a != null && b != null && a.length == b.length &&
        (0 until a.size).forall(i => a(i) == b(i)))

  def equals(a: Array[Char], b: Array[Char]): Boolean =
    (a eq b) || (a != null && b != null && a.length == b.length &&
        (0 until a.size).forall(i => a(i) == b(i)))

  def equals(a: Array[Byte], b: Array[Byte]): Boolean =
    (a eq b) || (a != null && b != null && a.length == b.length &&
        (0 until a.size).forall(i => a(i) == b(i)))

  def equals(a: Array[Short], b: Array[Short]): Boolean =
    (a eq b) || (a != null && b != null && a.length == b.length &&
        (0 until a.size).forall(i => a(i) == b(i)))

  def equals(a: Array[Int], b: Array[Int]): Boolean =
    (a eq b) || (a != null && b != null && a.length == b.length &&
        (0 until a.size).forall(i => a(i) == b(i)))

  def equals(a: Array[Long], b: Array[Long]): Boolean =
    (a eq b) || (a != null && b != null && a.length == b.length &&
        (0 until a.size).forall(i => a(i) == b(i)))

  def equals(a: Array[Float], b: Array[Float]): Boolean =
    (a eq b) || (a != null && b != null && a.length == b.length &&
        (0 until a.size).forall(i => a(i) == b(i)))

  def equals(a: Array[Double], b: Array[Double]): Boolean =
    (a eq b) || (a != null && b != null && a.length == b.length &&
        (0 until a.size).forall(i => a(i) == b(i)))

  def equals(a: Array[AnyRef], b: Array[AnyRef]): Boolean =
    (a eq b) || (a != null && b != null && a.length == b.length &&
        (0 until a.size).forall(i => a(i) == b(i)))

}
