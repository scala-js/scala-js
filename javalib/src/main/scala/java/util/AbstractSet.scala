package java.util

import scala.annotation.tailrec

import scala.collection.JavaConversions._

abstract class AbstractSet[E] protected () extends AbstractCollection[E]
                                              with Set[E] {
  override def equals(that: Any): Boolean = {
    if (that.asInstanceOf[AnyRef] eq this) true
    else {
      that match {
        case that: Collection[_] => that.size == this.size && containsAll(that)
        case _                   => false
      }
    }
  }

  override def hashCode(): Int =
    asScalaIterator(iterator).foldLeft(0)((prev, item) => item.hashCode + prev)

  override def removeAll(c: Collection[_]): Boolean = {
    @tailrec
    def remAll(iter: Iterator[_], comp: Collection[_],
        remFun: (Iterator[_], Any) => Unit, res: Boolean = false): Boolean = {
      if (iter.hasNext) {
        val elem = iter.next()
        if (comp.contains(elem)) {
          remFun(iter, elem)
          remAll(iter, comp, remFun, true)
        } else remAll(iter, comp, remFun, res)
      } else res
    }

    if (size > c.size)
      remAll(c.iterator, this, (i, _) => i.remove())
    else
      remAll(iterator, c, (_, e) => remove(e))
  }
}
