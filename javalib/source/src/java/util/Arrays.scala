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

  def fill(a: Array[Int], value: Int): Unit = {
    var i = 0
    while (i < a.length) {
      a(i) = value
      i += 1
    }
  }
}
