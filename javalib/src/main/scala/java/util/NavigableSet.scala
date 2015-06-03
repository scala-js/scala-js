package java.util

trait NavigableSet[E] extends SortedSet[E] {
  def lower(e: E): E
  def floor(e: E): E
  def ceiling(e: E): E
  def higher(e: E): E
  def pollFirst(): E
  def pollLast(): E
  def iterator(): Iterator[E]
  def descendingSet(): NavigableSet[E]
  def descendingIterator(): Iterator[E]
  def subSet(fromElement: E, fromInclusive: Boolean, toElement: E, toInclusive: Boolean): NavigableSet[E]
  def headSet(toElement: E, inclusive: Boolean): NavigableSet[E]
  def tailSet(fromElement: E, inclusive: Boolean): NavigableSet[E]
  def subSet(fromElement: E, toElement: E): SortedSet[E]
  def headSet(toElement: E): SortedSet[E]
  def tailSet(fromElement: E): SortedSet[E]
}
