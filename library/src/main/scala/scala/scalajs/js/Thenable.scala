/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.js

import scala.language.implicitConversions

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
  def `then`[B](
      onFulfilled: js.Function1[A, B | Thenable[B]],
      onRejected: js.UndefOr[js.Function1[scala.Any, B | Thenable[B]]]): Thenable[B]

  def `then`[B >: A](
      onFulfilled: Unit,
      onRejected: js.UndefOr[js.Function1[scala.Any, B | Thenable[B]]]): Thenable[B]
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
      p.`then`[Unit](
          { (v: A) =>
            p2.success(v)
            (): Unit | Thenable[Unit]
          },
          js.defined { (e: scala.Any) =>
            p2.failure(e match {
              case th: Throwable => th
              case _             => JavaScriptException(e)
            })
            (): Unit | Thenable[Unit]
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
}
