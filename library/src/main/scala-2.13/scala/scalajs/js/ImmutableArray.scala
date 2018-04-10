/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package scala.scalajs.js

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.{IterableOnce, SeqFactory, StrictOptimizedSeqFactory, immutable, mutable}


/** Equivalent of sci.ImmutableArray for js.Array */
final class ImmutableArray[+A] private (array: Array[A])
  extends immutable.IndexedSeq[A]
    with immutable.IndexedSeqOps[A, ImmutableArray, ImmutableArray[A]]
    with immutable.StrictOptimizedSeqOps[A, ImmutableArray, ImmutableArray[A]]
    with Serializable {

  /** Creates a new empty [[ImmutableArray]]. */
  def this() = this(Array())

  override def iterableFactory: SeqFactory[ImmutableArray] = ImmutableArray

  def apply(idx: Int): A = array(idx)

  def length: Int = array.length

  @inline override def className: String = "ImmutableArray"

}

object ImmutableArray extends StrictOptimizedSeqFactory[ImmutableArray] {

  private val emptyInstance = new ImmutableArray[Nothing]()

  def empty[A]: ImmutableArray[A] = emptyInstance

  def from[A](source: IterableOnce[A]): ImmutableArray[A] =
    (newBuilder[A]() ++= source).result()

  def newBuilder[A](): mutable.Builder[A, ImmutableArray[A]] =
    Array[A]().mapResult(new ImmutableArray(_))

}
