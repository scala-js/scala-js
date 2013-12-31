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

}
