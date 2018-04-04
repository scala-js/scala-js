/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package scala.scalajs.js

import scala.collection.{mutable, IterableOnce, IterableFactory, StrictOptimizedSeqOps}

// There is no equivalent of ArrayLike in 2.13, so we
// redefine it here
trait ScalaVersionSpecificArrayOps[A]
  extends StrictOptimizedSeqOps[A, Array, Array[A]]
    with mutable.IndexedSeqOps[A, Array, Array[A]]
    with Serializable { this: ArrayOps[A] =>

  object iterableFactory extends IterableFactory[Array] {
    def empty[E]: Array[E] = new Array()
    def from[E](source: IterableOnce[E]): Array[E] = (newBuilder[E]() ++= source).result()
    def newBuilder[E](): mutable.Builder[E, Array[E]] = new ArrayOps[E]
  }

  protected[this] def coll: Array[A] = array

  protected[this] def fromSpecificIterable(coll: scala.collection.Iterable[A]): Array[A] =
    iterableFactory.from(coll)

  protected[this] def newSpecificBuilder(): mutable.Builder[A, Array[A]] =
    iterableFactory.newBuilder()

  def toIterable: scala.collection.Iterable[A] = array

  def repr: Array[A] = array

  @inline final def addOne(elem: A): this.type = {
    array.push(elem)
    this
  }

  def mapInPlace(f: A => A): this.type = {
    var i = 0
    val siz = size
    while (i < siz) { this(i) = f(this(i)); i += 1 }
    this
  }

}