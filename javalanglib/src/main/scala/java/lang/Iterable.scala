package java.lang

import java.util.Iterator

trait Iterable[T] {
  def iterator(): Iterator[T]
}
