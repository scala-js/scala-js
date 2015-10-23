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
import mutable.Builder

import scala.collection.generic.{CanBuildFrom, GenericCompanion, SeqFactory}

/** Equivalent of scm.WrappedArray for js.Array */
@inline
final class WrappedArray[A](val array: Array[A])
    extends mutable.AbstractBuffer[A]
       with scala.collection.generic.GenericTraversableTemplate[A, WrappedArray]
       with mutable.IndexedSeq[A]
       with mutable.BufferLike[A, WrappedArray[A]]
       with mutable.ArrayLike[A, WrappedArray[A]]
       with Builder[A, WrappedArray[A]] {

  /** Creates a new empty [[WrappedArray]]. */
  def this() = this(Array())

  override def companion: GenericCompanion[WrappedArray] = WrappedArray

  // IndexedSeq interface

  @inline def update(index: Int, elem: A): Unit = array(index) = elem
  @inline def apply(index: Int): A = array(index)
  @inline def length: Int = array.length

  // Builder interface

  @inline def +=(elem: A): this.type = {
    array.push(elem)
    this
  }

  @inline def clear(): Unit =
    array.length = 0

  @inline def result(): WrappedArray[A] = this

  // Rest of BufferLike interface

  @inline def +=:(elem: A): this.type = {
    array.unshift(elem)
    this
  }

  @inline override def ++=:(xs: TraversableOnce[A]): this.type = {
    array.unshift(xs.toSeq: _*)
    this
  }

  @inline def insertAll(n: Int,
      elems: scala.collection.Traversable[A]): Unit = {
    array.splice(n, 0, elems.toSeq: _*)
  }

  @inline def remove(n: Int): A =
    array.splice(n, 1)(0)

  @inline override def remove(n: Int, count: Int): Unit =
    array.splice(n, count)

  @inline override def stringPrefix: String = "WrappedArray"

}

/** Factory for [[WrappedArray]]. Mainly provides the relevant
 *  [[scala.collection.generic.CanBuildFrom CanBuildFroms]]s and implicit
 *  conversions.
 */
object WrappedArray extends SeqFactory[WrappedArray] {
  /** Standard CBF for [[WrappedArray]] */
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, WrappedArray[A]] =
    ReusableCBF.asInstanceOf[GenericCanBuildFrom[A]]

  def newBuilder[A]: Builder[A, WrappedArray[A]] = new WrappedArray[A]

  implicit def toJSArray[A](wrappedArray: WrappedArray[A]): Array[A] =
    wrappedArray.array

}
