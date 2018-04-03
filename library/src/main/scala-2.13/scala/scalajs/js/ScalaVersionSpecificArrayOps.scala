/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package scala.scalajs.js

import scala.collection.{mutable, StrictOptimizedSeqOps}

// There is no equivalent of ArrayLike in 2.13, so we
// redefine it here
trait ScalaVersionSpecificArrayOps[A]
  extends mutable.AbstractSeq[A]
    with StrictOptimizedSeqOps[A, ArrayOps, ArrayOps[A]]
    with mutable.IndexedSeq[A]
    with mutable.IndexedOptimizedSeq[A]
    with mutable.IndexedSeqOps[A, ArrayOps, ArrayOps[A]]
    with Serializable { this: ArrayOps[A] =>

//  override def iterableFactory = ???

  def repr: Array[A] = array

  @inline final def addOne(elem: A): this.type = {
    array.push(elem)
    this
  }

}