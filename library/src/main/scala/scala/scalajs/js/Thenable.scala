/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.js

import scala.language.{higherKinds, implicitConversions}
import scala.scalajs.js
import js.annotation._
import scala.concurrent.Future

/** A thing on which one can call the `then` method.
 *
 *  `Thenable`s are automatically transitively flattened by the `then` method
 *  of `Thenable`s. In particular, this is true for [[Promise]]s.
 *
 *  Attention! The nature of this interface, from the ECMAScript specification,
 *  makes it inherently un-typeable, because it is not type parametric.
 *
 *  The signature of the `then` method is only valid <i>provided that</i> the
 *  values of `B` do not have a `then` method.
 */
@ScalaJSDefined
trait Thenable[+A] extends js.Object {
  def `then`[B, C](
      onFulfilled: js.Function1[A, B],
      onRejected: js.UndefOr[js.Function1[scala.Any, B]]
  )(implicit
      ev: Thenable.Returning[B, C]
  ): Thenable[C]

  def `then`[B >: A, C](
      onFulfilled: Unit,
      onRejected: js.UndefOr[js.Function1[scala.Any, B]]
  )(implicit
      ev: Thenable.Returning[B, C]
  ): Thenable[C]
}

object Thenable {
  implicit class ThenableOps[+A](val p: Thenable[A]) extends AnyVal {
    /** Converts the [[Thenable]] into a Scala [[scala.concurrent.Future Future]].
     *
     *  Unlike when calling the `then` methods of [[Thenable]], the resulting
     *  [[scala.concurrent.Future Future]] is always properly typed, and
     *  operations on it will be well-typed in turn.
     */
    def toFuture: Future[A] = {
      val p2 = scala.concurrent.Promise[A]()
      p.`then`[Unit, Unit](
          { (v: A) =>
            p2.success(v)
            ()
          },
          js.defined { (e: scala.Any) =>
            p2.failure(e match {
              case th: Throwable => th
              case _             => JavaScriptException(e)
            })
            ()
          })
      p2.future
    }
  }

  /** Implicits for [[Thenable]]s.
   *
   *  Import `Implicits._` to enable [[scala.concurrent.Future Future]]'s
   *  operations directly on [[Thenable]]s, without needing to convert them
   *  with [[ThenableOps.toFuture toFuture]].
   *
   *  Unlike the `then` methods in [[Thenable]],
   *  [[scala.concurrent.Future Future]]'s operations are always well-typed.
   */
  object Implicits {
    implicit def thenable2future[A](p: Thenable[A]): Future[A] =
      p.toFuture
  }

  sealed trait Returning[A, B]

  trait Returning1 {
    implicit def default[A]: Returning[A, A] = new Returning[A, A] {}
  }

  object Returning extends Returning1 {

    implicit def flattened[A, T[X] <: Thenable[X]]: Returning[T[A], A] =
      new Returning[T[A], A] {}

    implicit def union[A, B, C, D](implicit
      left: Returning[A, C],
      right: Returning[B, D]
    ): Returning[A | B, C | D] =
      new Returning[A | B, C | D] {}

  }
}
