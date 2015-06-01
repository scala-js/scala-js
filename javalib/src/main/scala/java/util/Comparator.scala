package java.util

// scalastyle:off equals.hash.code

trait Comparator[A] {
  def compare(o1: A, o2: A): Int
  def equals(obj: Any): Boolean
}
