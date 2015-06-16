package java.util

abstract class AbstractSequentialList[E] protected ()
    extends AbstractList[E] {

  def get(index: Int): E = {
    val iter = listIterator(index)
    if (iter.hasNext) iter.next()
    else throw new IndexOutOfBoundsException(index.toString)
  }

  override def set(index: Int, element: E): E = {
    val iter = listIterator(index)
    if (!iter.hasNext)
      throw new IndexOutOfBoundsException
    val ret = iter.next()
    iter.set(element)
    ret
  }

  override def add(index: Int, element: E): Unit =
    listIterator(index).add(element)

  override def remove(index: Int): E = {
    val iter = listIterator(index)
    if (!iter.hasNext)
      throw new IndexOutOfBoundsException
    val ret = iter.next()
    iter.remove
    ret
  }

  override def addAll(index: Int, c: Collection[_ <: E]): Boolean = {
    val iter = listIterator(index)
    val citer = c.iterator()
    val changed = citer.hasNext()
    while (citer.hasNext()) {
      iter.add(citer.next())
    }
    changed
  }

  def listIterator(index: Int): ListIterator[E]
}
