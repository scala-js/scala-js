/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package scala.scalajs.js

import scala.language.implicitConversions

import scala.collection.mutable
import scala.collection.{SeqFactory, StrictOptimizedSeqFactory, StrictOptimizedSeqOps}

import scala.scalajs.js

/** Equivalent of scm.WrappedArray for js.Array */
@inline
final class WrappedArray[A](val array: js.Array[A])
    extends mutable.AbstractBuffer[A]
    with StrictOptimizedSeqOps[A, WrappedArray, WrappedArray[A]]
    with mutable.IndexedSeq[A]
    with mutable.IndexedSeqOps[A, WrappedArray, WrappedArray[A]]
    with mutable.IndexedOptimizedBuffer[A]
    with mutable.Builder[A, WrappedArray[A]]
    with Serializable {

  /** Creates a new empty [[WrappedArray]]. */
  def this() = this(js.Array())

  override def iterableFactory: SeqFactory[WrappedArray] = WrappedArray

  // IndexedSeq interface

  @inline def update(index: Int, elem: A): Unit = array(index) = elem
  @inline def apply(index: Int): A = array(index)
  @inline def length: Int = array.length

  // Builder interface

  @inline def addOne(elem: A): this.type = {
    array.push(elem)
    this
  }

  @inline def clear(): Unit =
    array.length = 0

  @inline def result(): WrappedArray[A] = this

  // Rest of Buffer interface

  @inline def prepend(elem: A): this.type = {
    array.unshift(elem)
    this
  }

  @inline override def prependAll(xs: IterableOnce[A]): this.type = {
    array.unshift(xs.iterator.toSeq: _*)
    this
  }

  @inline def subtractOne(elem: A): this.type = {
    val i = indexOf(elem)
    if (i != -1) remove(i)
    this
  }

  @inline def insert(idx: Int, elem: A): Unit =
    array.splice(idx, 0, elem)

  @inline
  def insertAll(n: Int, elems: scala.collection.IterableOnce[A]): Unit =
    array.splice(n, 0, elems.iterator.toSeq: _*)

  @inline def remove(n: Int): A =
    array.splice(n, 1)(0)

  @inline override def remove(n: Int, count: Int): Unit =
    array.splice(n, count)

  @inline override def className: String = "WrappedArray"

}

/** Factory for [[WrappedArray]]. Provides implicit conversion to [[Array]].
 */
object WrappedArray extends StrictOptimizedSeqFactory[WrappedArray] {

  def empty[A]: WrappedArray[A] = new WrappedArray[A]()

  def newBuilder[A]: mutable.Builder[A, WrappedArray[A]] = new WrappedArray[A]

  def from[A](source: IterableOnce[A]): WrappedArray[A] =
    (newBuilder[A] ++= source).result()

  implicit def toJSArray[A](wrappedArray: WrappedArray[A]): js.Array[A] =
    wrappedArray.array

}
