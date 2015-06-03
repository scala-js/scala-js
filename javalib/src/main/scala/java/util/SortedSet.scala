package java.util

trait SortedSet[E] extends Set[E] {
  def comparator(): Comparator[_ >: E]
  def subSet(fromElement: E, toElement: E): SortedSet[E]
  def headSet(toElement: E): SortedSet[E]
  def tailSet(fromElement: E): SortedSet[E]
  def first(): E
  def last(): E
}
