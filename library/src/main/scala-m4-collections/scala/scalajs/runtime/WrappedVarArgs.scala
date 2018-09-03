package scala.scalajs.runtime

import scala.collection.immutable
import scala.collection.mutable
import scala.collection.{IterableOnce, SeqFactory, StrictOptimizedSeqFactory}

import scala.scalajs.js

/** An immutable wrapper for `js.Array` to show it as an `s.c.i.Seq`.
 *
 *  TODO Eventually we should expose this in the public API under
 *  `scala.scalajs.js`, but the naming should be carefully thought through,
 *  especially wrt. the existing `js.WrappedArray`.
 */
@inline
private[runtime] final class WrappedVarArgs[+A] private (array: js.Array[A])
    extends immutable.IndexedSeq[A]
    with immutable.IndexedSeqOps[A, WrappedVarArgs, WrappedVarArgs[A]]
    with immutable.StrictOptimizedSeqOps[A, WrappedVarArgs, WrappedVarArgs[A]]
    with Serializable {

  /** Creates a new empty [[WrappedVarArgs]]. */
  def this() = this(js.Array())

  override def iterableFactory: SeqFactory[WrappedVarArgs] = WrappedVarArgs

  def length: Int = array.length

  def apply(idx: Int): A = array(idx)

  @inline override def className: String = "WrappedVarArgs"

  // This method must stay private when we make the class itself public.
  @inline def unwrap: js.Array[_ <: A] = array

}

private[runtime] object WrappedVarArgs
    extends StrictOptimizedSeqFactory[WrappedVarArgs] {

  // This method must stay private when we make the class itself public.
  @inline private[runtime] def wrap[A](array: js.Array[A]): WrappedVarArgs[A] =
    new WrappedVarArgs(array)

  def empty[A]: WrappedVarArgs[A] = new WrappedVarArgs[Nothing]()

  def from[A](source: IterableOnce[A]): WrappedVarArgs[A] =
    (newBuilder[A] ++= source).result()

  def newBuilder[A]: mutable.Builder[A, WrappedVarArgs[A]] =
    js.Array[A]().mapResult(new WrappedVarArgs(_))
}
