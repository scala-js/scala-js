/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package java.util

import scala.annotation.tailrec

import ScalaOps._

abstract class AbstractSet[E] protected () extends AbstractCollection[E] with Set[E] {
  override def equals(that: Any): Boolean = {
    if (that.asInstanceOf[AnyRef] eq this) true
    else {
      that match {
        case that: Collection[_] => that.size() == this.size() && containsAll(that)
        case _                   => false
      }
    }
  }

  override def hashCode(): Int =
    this.scalaOps.foldLeft(0)((prev, item) => item.hashCode + prev)

  override def removeAll(c: Collection[_]): Boolean = {
    if (size() > c.size()) {
      c.scalaOps.foldLeft(false)((prev, elem) => this.remove(elem) || prev)
    } else {
      @tailrec
      def removeAll(iter: Iterator[E], modified: Boolean): Boolean = {
        if (iter.hasNext()) {
          if (c.contains(iter.next())) {
            iter.remove()
            removeAll(iter, true)
          } else {
            removeAll(iter, modified)
          }
        } else {
          modified
        }
      }
      removeAll(this.iterator(), false)
    }
  }
}
