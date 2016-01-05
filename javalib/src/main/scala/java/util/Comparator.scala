package java.util

import scala.scalajs.js.annotation.JavaDefaultMethod

// scalastyle:off equals.hash.code

trait Comparator[A] {
  def compare(o1: A, o2: A): Int
  def equals(obj: Any): Boolean

  @JavaDefaultMethod
  def reversed(): Comparator[A] =
    Collections.reverseOrder(this)
}
