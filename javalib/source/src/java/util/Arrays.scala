package java.util

import scala.scalajs.js

object Arrays {
  def sort[T](array: Array[Any], comparator: Comparator[T]): Unit = {
    def compareFn(o1: T, o2: T): js.Number =
      comparator.compare(o1, o2)

    val jsArray: js.Array[T] =
        array.asInstanceOf[js.Dynamic].underlying.asInstanceOf[js.Array[T]]
    jsArray.sort(compareFn _)
  }

  def fill(a: Array[Boolean], value: Boolean): Unit = {
    var i = 0
    while (i < a.length) {
      a(i) = value
      i += 1
    }
  }

  def fill(a: Array[Boolean], fromIndex: Int, toIndex: Int, value: Boolean): Unit = {
    var i = fromIndex
    while (i < toIndex) {
      a(i) = value
      i += 1
    }
  }

  def fill(a: Array[Byte], value: Byte): Unit = {
    var i = 0
    while (i < a.length) {
      a(i) = value
      i += 1
    }
  }

  def fill(a: Array[Byte], fromIndex: Int, toIndex: Int, value: Byte): Unit = {
    var i = fromIndex
    while (i < toIndex) {
      a(i) = value
      i += 1
    }
  }

  def fill(a: Array[Char], value: Char): Unit = {
    var i = 0
    while (i < a.length) {
      a(i) = value
      i += 1
    }
  }

  def fill(a: Array[Char], fromIndex: Int, toIndex: Int, value: Char): Unit = {
    var i = fromIndex
    while (i < toIndex) {
      a(i) = value
      i += 1
    }
  }

  def fill(a: Array[Short], value: Short): Unit = {
    var i = 0
    while (i < a.length) {
      a(i) = value
      i += 1
    }
  }

  def fill(a: Array[Short], fromIndex: Int, toIndex: Int, value: Short): Unit = {
    var i = fromIndex
    while (i < toIndex) {
      a(i) = value
      i += 1
    }
  }

  def fill(a: Array[Int], value: Int): Unit = {
    var i = 0
    while (i < a.length) {
      a(i) = value
      i += 1
    }
  }

  def fill(a: Array[Int], fromIndex: Int, toIndex: Int, value: Int): Unit = {
    var i = fromIndex
    while (i < toIndex) {
      a(i) = value
      i += 1
    }
  }

  def fill(a: Array[Long], value: Long): Unit = {
    var i = 0
    while (i < a.length) {
      a(i) = value
      i += 1
    }
  }

  def fill(a: Array[Long], fromIndex: Int, toIndex: Int, value: Long): Unit = {
    var i = fromIndex
    while (i < toIndex) {
      a(i) = value
      i += 1
    }
  }

  def fill(a: Array[Float], value: Float): Unit = {
    var i = 0
    while (i < a.length) {
      a(i) = value
      i += 1
    }
  }

  def fill(a: Array[Float], fromIndex: Int, toIndex: Int, value: Float): Unit = {
    var i = fromIndex
    while (i < toIndex) {
      a(i) = value
      i += 1
    }
  }

  def fill(a: Array[Double], value: Double): Unit = {
    var i = 0
    while (i < a.length) {
      a(i) = value
      i += 1
    }
  }

  def fill(a: Array[Double], fromIndex: Int, toIndex: Int, value: Double): Unit = {
    var i = fromIndex
    while (i < toIndex) {
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

  def fill(a: Array[AnyRef], fromIndex: Int, toIndex: Int, value: AnyRef): Unit = {
    var i = fromIndex
    while (i < toIndex) {
      a(i) = value
      i += 1
    }
  }

  private def checkIndexForBinarySearch(length: Int, start: Int, end: Int): Unit = {
    if (start > end) {
      throw new IllegalArgumentException("fromIndex(" + start + ") > toIndex(" + end + ")")
    }
    if (0 > start) {
      throw new ArrayIndexOutOfBoundsException("Array index out of range: " + start)
    }
    if (length < end) {
      throw new ArrayIndexOutOfBoundsException("Array index out of range: " + end)
    }
  }

  def binarySearch(a: Array[Long], key: Long): Int =
    binarySearch(a, 0, a.length, key)

  def binarySearch(a: Array[Long], startIndex: Int, endIndex:Int, key: Long): Int = {
    checkIndexForBinarySearch(a.length, startIndex, endIndex)
    var low = startIndex
    var mid = -1
    var high = endIndex - 1
    while (low <= high) {
      mid = (low + high) >>> 1
      if (key > a(mid))
        low = mid + 1
      else if (key == a(mid))
        return mid
      else
        high = mid -1
    }
    return -low - 1
  }

  def binarySearch(a: Array[Int], key: Int): Int =
    binarySearch(a, 0, a.length, key)

  def binarySearch(a: Array[Int], startIndex: Int, endIndex:Int, key: Int): Int = {
    checkIndexForBinarySearch(a.length, startIndex, endIndex)
    var low = startIndex
    var mid = -1
    var high = endIndex - 1
    while (low <= high) {
      mid = (low + high) >>> 1
      if (key > a(mid))
        low = mid + 1
      else if (key == a(mid))
        return mid
      else
        high = mid -1
    }
    return -low - 1
  }

  def binarySearch(a: Array[Short], key: Short): Int =
    binarySearch(a, 0, a.length, key)

  def binarySearch(a: Array[Short], startIndex: Int, endIndex:Int, key: Short): Int = {
    checkIndexForBinarySearch(a.length, startIndex, endIndex)
    var low = startIndex
    var mid = -1
    var high = endIndex - 1
    while (low <= high) {
      mid = (low + high) >>> 1
      if (key > a(mid))
        low = mid + 1
      else if (key == a(mid))
        return mid
      else
        high = mid -1
    }
    return -low - 1
  }

  def binarySearch(a: Array[Char], key: Char): Int =
    binarySearch(a, 0, a.length, key)

  def binarySearch(a: Array[Char], startIndex: Int, endIndex:Int, key: Char): Int = {
    checkIndexForBinarySearch(a.length, startIndex, endIndex)
    var low = startIndex
    var mid = -1
    var high = endIndex - 1
    while (low <= high) {
      mid = (low + high) >>> 1
      if (key > a(mid))
        low = mid + 1
      else if (key == a(mid))
        return mid
      else
        high = mid -1
    }
    return -low - 1
  }

  def binarySearch(a: Array[Float], key: Float): Int =
      binarySearch(a, 0, a.length, key)

  def binarySearch(a: Array[Float], startIndex: Int, endIndex:Int, key: Float): Int = {
    checkIndexForBinarySearch(a.length, startIndex, endIndex)
    var low = startIndex
    var mid = -1
    var high = endIndex - 1
    while (low <= high) {
      mid = (low + high) >>> 1
      if (key > a(mid))
        low = mid + 1
      else if (key == a(mid))
        return mid
      else
        high = mid -1
    }
    return -low - 1
  }

  def binarySearch(a: Array[Double], key: Double): Int =
      binarySearch(a, 0, a.length, key)

  def binarySearch(a: Array[Double], startIndex: Int, endIndex:Int, key: Double): Int = {
    checkIndexForBinarySearch(a.length, startIndex, endIndex)
    var low = startIndex
    var mid = -1
    var high = endIndex - 1
    while (low <= high) {
      mid = (low + high) >>> 1
      if (key > a(mid))
        low = mid + 1
      else if (key == a(mid))
        return mid
      else
        high = mid -1
    }
    return -low - 1
  }

  def binarySearch(a: Array[AnyRef], key: AnyRef): Int =
      binarySearch(a, 0, a.length, key)

  def binarySearch(a: Array[AnyRef], startIndex: Int, endIndex:Int, key: AnyRef): Int = {
    checkIndexForBinarySearch(a.length, startIndex, endIndex)
    var low = startIndex
    var mid = -1
    var high = endIndex - 1
    while (low <= high) {
      mid = (low + high) >>> 1
      if (key.asInstanceOf[Comparable[AnyRef]].compareTo(a(mid)) > 0)
        low = mid + 1
      else if (key.asInstanceOf[Comparable[AnyRef]].compareTo(a(mid)) == 0)
        return mid
      else
        high = mid -1
    }
    return -low - 1
  }

  def copyOf(original: Array[Int], newLen: Int): Array[Int] = {
    if (newLen >= 0)
      return copyOfRange(original, 0, newLen)
    throw new NegativeArraySizeException();
  }

  def copyOfRange(original: Array[Int], start: Int, end: Int): Array[Int] = {
    if (start <= end) {
      if (0 <= start && start <= original.length) {
        val retLength = end - start
        val copyLength = Math.min(retLength, original.length - start)
        val ret = new Array[Int](retLength)
        System.arraycopy(original, start, ret, 0, copyLength)
        return ret
      }
      throw new ArrayIndexOutOfBoundsException()
    }
    throw new IllegalArgumentException()
  }

  def copyOf(original: Array[Long], newLen: Int): Array[Long] = {
    if (newLen >= 0)
      return copyOfRange(original, 0, newLen)
    throw new NegativeArraySizeException();
  }

  def copyOfRange(original: Array[Long], start: Int, end: Int): Array[Long] = {
    if (start <= end) {
      if (0 <= start && start <= original.length) {
        val retLength = end - start
        val copyLength = Math.min(retLength, original.length - start)
        val ret = new Array[Long](retLength)
        System.arraycopy(original, start, ret, 0, copyLength)
        return ret
      }
      throw new ArrayIndexOutOfBoundsException()
    }
    throw new IllegalArgumentException()
  }

  def copyOf(original: Array[Short], newLen: Int): Array[Short] = {
    if (newLen >= 0)
      return copyOfRange(original, 0, newLen)
    throw new NegativeArraySizeException();
  }

  def copyOfRange(original: Array[Short], start: Int, end: Int): Array[Short] = {
    if (start <= end) {
      if (0 <= start && start <= original.length) {
        val retLength = end - start
        val copyLength = Math.min(retLength, original.length - start)
        val ret = new Array[Short](retLength)
        System.arraycopy(original, start, ret, 0, copyLength)
        return ret
      }
      throw new ArrayIndexOutOfBoundsException()
    }
    throw new IllegalArgumentException()
  }

  def copyOf(original: Array[Byte], newLen: Int): Array[Byte] = {
    if (newLen >= 0)
      return copyOfRange(original, 0, newLen)
    throw new NegativeArraySizeException();
  }

  def copyOfRange(original: Array[Byte], start: Int, end: Int): Array[Byte] = {
    if (start <= end) {
      if (0 <= start && start <= original.length) {
        val retLength = end - start
        val copyLength = Math.min(retLength, original.length - start)
        val ret = new Array[Byte](retLength)
        System.arraycopy(original, start, ret, 0, copyLength)
        return ret
      }
      throw new ArrayIndexOutOfBoundsException()
    }
    throw new IllegalArgumentException()
  }

  def copyOf(original: Array[Char], newLen: Int): Array[Char] = {
    if (newLen >= 0)
      return copyOfRange(original, 0, newLen)
    throw new NegativeArraySizeException();
  }

  def copyOfRange(original: Array[Char], start: Int, end: Int): Array[Char] = {
    if (start <= end) {
      if (0 <= start && start <= original.length) {
        val retLength = end - start
        val copyLength = Math.min(retLength, original.length - start)
        val ret = new Array[Char](retLength)
        System.arraycopy(original, start, ret, 0, copyLength)
        return ret
      }
      throw new ArrayIndexOutOfBoundsException()
    }
    throw new IllegalArgumentException()
  }

  def copyOf(original: Array[Float], newLen: Int): Array[Float] = {
    if (newLen >= 0)
      return copyOfRange(original, 0, newLen)
    throw new NegativeArraySizeException();
  }

  def copyOfRange(original: Array[Float], start: Int, end: Int): Array[Float] = {
    if (start <= end) {
      if (0 <= start && start <= original.length) {
        val retLength = end - start
        val copyLength = Math.min(retLength, original.length - start)
        val ret = new Array[Float](retLength)
        System.arraycopy(original, start, ret, 0, copyLength)
        return ret
      }
      throw new ArrayIndexOutOfBoundsException()
    }
    throw new IllegalArgumentException()
  }

  def copyOf(original: Array[Double], newLen: Int): Array[Double] = {
    if (newLen >= 0)
      return copyOfRange(original, 0, newLen)
    throw new NegativeArraySizeException();
  }

  def copyOfRange(original: Array[Double], start: Int, end: Int): Array[Double] = {
    if (start <= end) {
      if (0 <= start && start <= original.length) {
        val retLength = end - start
        val copyLength = Math.min(retLength, original.length - start)
        val ret = new Array[Double](retLength)
        System.arraycopy(original, start, ret, 0, copyLength)
        return ret
      }
      throw new ArrayIndexOutOfBoundsException()
    }
    throw new IllegalArgumentException()
  }

  def copyOf(original: Array[Boolean], newLen: Int): Array[Boolean] = {
    if (newLen >= 0)
      return copyOfRange(original, 0, newLen)
    throw new NegativeArraySizeException();
  }

  def copyOfRange(original: Array[Boolean], start: Int, end: Int): Array[Boolean] = {
    if (start <= end) {
      if (0 <= start && start <= original.length) {
        val retLength = end - start
        val copyLength = Math.min(retLength, original.length - start)
        val ret = new Array[Boolean](retLength)
        System.arraycopy(original, start, ret, 0, copyLength)
        return ret
      }
      throw new ArrayIndexOutOfBoundsException()
    }
    throw new IllegalArgumentException()
  }

}
