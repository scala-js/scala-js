/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.js

import scala.language.implicitConversions
import scala.scalajs.js.|.Evidence

/** Value of type A or the JS undefined value.
 *
 *  `js.UndefOr[A]` is the type of a value that can be either `undefined` or
 *  an `A`. It provides an API similar to that of [[scala.Option]] through the
 *  [[UndefOrOps]] implicit class, where `undefined` take the role of [[None]].
 *
 *  By extension, this type is also suited to typing optional fields in native
 *  JS types, i.e., fields that may not exist on the object.
 */
sealed trait UndefOr[+A]

sealed abstract class UndefOrLowPrioImplicits {
  /** Upcast `A` to `UndefOr[B1 | B2]`.
   *
   *  This needs evidence that `A <: B1 | B2`.
   */
  implicit def any2undefOrUnion[A, B1, B2](a: A)(
      implicit ev: Evidence[A, B1 | B2]): UndefOr[B1 | B2] = {
    a.asInstanceOf[UndefOr[B1 | B2]]
  }
}

object UndefOr extends UndefOrLowPrioImplicits {
  implicit def any2undefOrA[A](value: A): UndefOr[A] =
    value.asInstanceOf[UndefOr[A]]

  implicit def undefOr2ops[A](value: UndefOr[A]): UndefOrOps[A] =
    new UndefOrOps(value)

  implicit def undefOr2jsAny[A](value: UndefOr[A])(implicit ev: A => Any): Any =
    value.map(ev).asInstanceOf[Any]
}

/** @define option [[UndefOr]]
 *  @define none [[undefined]]
 */
final class UndefOrOps[A](val self: UndefOr[A]) extends AnyVal {
  import UndefOrOps._

  /** Returns true if the option is `undefined`, false otherwise.
   */
  @inline final def isEmpty: Boolean = isUndefined(self)

  /** Returns true if the option is not `undefined`, false otherwise.
   */
  @inline final def isDefined: Boolean = !isEmpty

  /** Returns the option's value.
   *  @note The option must be nonEmpty.
   *  @throws java.util.NoSuchElementException if the option is empty.
   */
  @inline final def get: A =
    if (isEmpty) throw new NoSuchElementException("undefined.get")
    else self.asInstanceOf[A]

  @inline final private def forceGet: A = self.asInstanceOf[A]

  /** Returns the option's value if the option is nonempty, otherwise
   *  return the result of evaluating `default`.
   *
   *  @param default  the default expression.
   */
  @inline final def getOrElse[B >: A](default: => B): B =
    if (isEmpty) default else this.forceGet

  /** Returns the option's value if it is nonempty,
   *  or `null` if it is empty.
   *  Although the use of null is discouraged, code written to use
   *  $option must often interface with code that expects and returns nulls.
   *  @example {{{
   *  val initalText: Option[String] = getInitialText
   *  val textField = new JComponent(initalText.orNull,20)
   *  }}}
   */
  @inline final def orNull[A1 >: A](implicit ev: Null <:< A1): A1 =
    this getOrElse ev(null)

  /** Returns the result of applying `f` to this $option's
   *  value if this $option is nonempty.
   *  Otherwise return $none.
   *
   *  @note This is similar to `flatMap` except here,
   *  `f` does not need to wrap its result in an $option.
   *
   *  @param  f   the function to apply
   *  @see flatMap
   *  @see foreach
   */
  @inline final def map[B](f: A => B): UndefOr[B] =
    if (isEmpty) undefined else f(this.forceGet)

  /** Returns the result of applying `f` to this $option's
   *  value if the $option is nonempty.  Otherwise, evaluates
   *  expression `ifEmpty`.
   *
   *  @note This is equivalent to `$option map f getOrElse ifEmpty`.
   *
   *  @param  ifEmpty the expression to evaluate if empty.
   *  @param  f       the function to apply if nonempty.
   */
  @inline final def fold[B](ifEmpty: => B)(f: A => B): B =
    if (isEmpty) ifEmpty else f(this.forceGet)

  /** Returns the result of applying `f` to this $option's value if
   *  this $option is nonempty.
   *  Returns $none if this $option is empty.
   *  Slightly different from `map` in that `f` is expected to
   *  return an $option (which could be $none).
   *
   *  @param  f   the function to apply
   *  @see map
   *  @see foreach
   */
  @inline final def flatMap[B](f: A => UndefOr[B]): UndefOr[B] =
    if (isEmpty) undefined else f(this.forceGet)

  def flatten[B](implicit ev: A <:< UndefOr[B]): UndefOr[B] =
    if (isEmpty) undefined else ev(this.forceGet)

  /** Returns this $option if it is nonempty '''and''' applying the predicate `p` to
   *  this $option's value returns true. Otherwise, return $none.
   *
   *  @param  p   the predicate used for testing.
   */
  @inline final def filter(p: A => Boolean): UndefOr[A] =
    if (isEmpty || p(this.forceGet)) self else undefined

  /** Returns this $option if it is nonempty '''and''' applying the predicate `p` to
   *  this $option's value returns false. Otherwise, return $none.
   *
   *  @param  p   the predicate used for testing.
   */
  @inline final def filterNot(p: A => Boolean): UndefOr[A] =
    if (isEmpty || !p(this.forceGet)) self else undefined

  /** Returns false if the option is $none, true otherwise.
   *  @note   Implemented here to avoid the implicit conversion to Iterable.
   */
  final def nonEmpty: Boolean = isDefined

  /** Necessary to keep $option from being implicitly converted to
   *  [[scala.collection.Iterable]] in `for` comprehensions.
   */
  @inline final def withFilter(p: A => Boolean): WithFilter[A] =
    new WithFilter(self, p)

  /** Tests whether the $option contains a given value as an element.
   *
   *  `x.contains(y)` differs from `x == y` only in the fact that it will
   *  return `false` when `x` and `y` are both `undefined`.
   *
   *  @param elem the element to test.
   *  @return `true` if the $option has an element that is equal (as
   *  determined by `==`) to `elem`, `false` otherwise.
   */
  @inline final def contains[A1 >: A](elem: A1): Boolean =
    !isEmpty && elem == this.forceGet

  /** Returns true if this option is nonempty '''and''' the predicate
   *  `p` returns true when applied to this $option's value.
   *  Otherwise, returns false.
   *
   *  @param  p   the predicate to test
   */
  @inline final def exists(p: A => Boolean): Boolean =
    !isEmpty && p(this.forceGet)

  /** Returns true if this option is empty '''or''' the predicate
   *  `p` returns true when applied to this $option's value.
   *
   *  @param  p   the predicate to test
   */
  @inline final def forall(p: A => Boolean): Boolean =
    isEmpty || p(this.forceGet)

  /** Apply the given procedure `f` to the option's value,
   *  if it is nonempty. Otherwise, do nothing.
   *
   *  @param  f   the procedure to apply.
   *  @see map
   *  @see flatMap
   */
  @inline final def foreach[U](f: A => U): Unit =
    if (!isEmpty) f(this.forceGet)

  /** Returns the result of applying `pf` to this $option's contained
   *  value, '''if''' this option is
   *  nonempty '''and''' `pf` is defined for that value.
   *  Returns $none otherwise.
   *
   *  @param  pf   the partial function.
   *  @return the result of applying `pf` to this $option's
   *  value (if possible), or $none.
   */
  @inline final def collect[B](pf: PartialFunction[A, B]): UndefOr[B] =
    if (isEmpty) undefined
    else pf.applyOrElse(this.forceGet, (_: A) => undefined).asInstanceOf[UndefOr[B]]

  /** Returns this $option if it is nonempty,
   *  otherwise return the result of evaluating `alternative`.
   *  @param alternative the alternative expression.
   */
  @inline final def orElse[B >: A](alternative: => UndefOr[B]): UndefOr[B] =
    if (isEmpty) alternative else self

  /** Returns a singleton iterator returning the $option's value
   *  if it is nonempty, or an empty iterator if the option is empty.
   */
  def iterator: Iterator[A] =
    if (isEmpty) scala.collection.Iterator.empty
    else scala.collection.Iterator.single(this.forceGet)

  /** Returns a singleton list containing the $option's value
   *  if it is nonempty, or the empty list if the $option is empty.
   */
  def toList: List[A] =
    if (isEmpty) Nil else this.forceGet :: Nil

  // Can't link doc to Left or Right - #1969
  /** Returns a `Left` containing the given argument `left` if this $option is
   *  empty, or a `Right` containing this $option's value if this is nonempty.
   *
   *  @param left the expression to evaluate and return if this is empty
   *  @see toLeft
   */
  @inline final def toRight[X](left: => X): Either[X, A] =
    if (isEmpty) Left(left) else Right(this.forceGet)

  // Can't link doc to Left or Right - #1969
  /** Returns a `Right` containing the given argument `right` if this is empty,
   *  or a `Left` containing this $option's value if this $option is nonempty.
   *
   *  @param right the expression to evaluate and return if this is empty
   *  @see toRight
   */
  @inline final def toLeft[X](right: => X): Either[A, X] =
    if (isEmpty) Right(right) else Left(this.forceGet)

  // Can't link doc to Some - #1969
  /** Returns a `Some` containing this $option's value
   *  if this $option is nonempty, [[scala.None None]] otherwise.
   */
  @inline final def toOption: Option[A] =
    if (isEmpty) None else Some(this.forceGet)
}

object UndefOrOps {

  /** We need a whole WithFilter class to honor the "doesn't create a new
   *  collection" contract even though it seems unlikely to matter much in a
   *  collection with max size 1.
   */
  class WithFilter[A](self: UndefOr[A], p: A => Boolean) {
    def map[B](f: A => B): UndefOr[B] = self filter p map f
    def flatMap[B](f: A => UndefOr[B]): UndefOr[B] = self filter p flatMap f
    def foreach[U](f: A => U): Unit = self filter p foreach f
    def withFilter(q: A => Boolean): WithFilter[A] =
      new WithFilter[A](self, x => p(x) && q(x))
  }
}
