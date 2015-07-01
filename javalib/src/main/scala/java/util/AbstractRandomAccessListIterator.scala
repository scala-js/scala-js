package java.util

abstract private[util] class AbstractRandomAccessListIterator[E](private var i: Int,
    start: Int, protected var end: Int) extends ListIterator[E] with SizeChangeEvent {

  private var last = -1

  def hasNext(): Boolean =
    i < end

  def next(): E = {
    last = i
    i += 1
    get(last)
  }

  def hasPrevious(): Boolean =
    start < i

  def previous(): E = {
    i -= 1
    last = i
    get(last)
  }

  def nextIndex(): Int = i

  def previousIndex(): Int = i - 1

  def remove(): Unit = {
    checkThatHasLast()
    remove(last)
    if (last < i)
      i -= 1
    last = -1
    changeSize(-1)
  }

  def set(e: E): Unit = {
    checkThatHasLast()
    set(last, e)
  }

  def add(e: E): Unit = {
    add(i, e)
    changeSize(1)
    last = -1
    i += 1
  }

  protected def get(index: Int): E

  protected def remove(index: Int): Unit

  protected def set(index: Int, e: E): Unit

  protected def add(index: Int, e: E): Unit

  private def checkThatHasLast(): Unit = {
    if (last == -1)
      throw new IllegalStateException
  }
}
