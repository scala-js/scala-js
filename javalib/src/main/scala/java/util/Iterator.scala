package java.util

trait Iterator[E] {
  def hasNext(): Boolean
  def next(): E
  def remove(): Unit
}
