package java.util

trait Deque[E] extends Queue[E] {
  def addFirst(e: E): Unit
  def addLast(e: E): Unit
  def offerFirst(e: E): Boolean
  def offerLast(e: E): Boolean
  def removeFirst(): E
  def removeLast(): E
  def pollFirst(): E
  def pollLast(): E
  def getFirst(): E
  def getLast(): E
  def peekFirst(): E
  def peekLast(): E
  def removeFirstOccurrence(o: Any): Boolean
  def removeLastOccurrence(o: Any): Boolean
  def add(e: E): Boolean
  def offer(e: E): Boolean
  def remove(): E
  def poll(): E
  def element(): E
  def peek(): E
  def push(e: E): Unit
  def pop(): E
  def remove(o: Any): Boolean
  def contains(o: Any): Boolean
  def size(): Int
  def iterator(): Iterator[E]
  def descendingIterator(): Iterator[E]
}
