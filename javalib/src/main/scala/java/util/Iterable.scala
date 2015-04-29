package java.util

trait Iterable[T] {
  def iterator(): Iterator[T]
}
