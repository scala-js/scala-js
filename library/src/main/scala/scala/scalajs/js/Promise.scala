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

import scala.concurrent.Future

/** <span class="badge badge-ecma6" style="float: right;">ECMAScript 6</span>
 *  Promise of an asynchronous result.
 *
 *  Attention! The nature of this class, from the ECMAScript specification,
 *  makes it inherently un-typeable, because it is not type parametric.
 *
 *  The signatures of the constructor and the methods `then` and `catch` are
 *  only valid <i>provided that</i> the values of `A` and `B` are not
 *  [[Thenable]]s.
 *
 *  We recommend to use Scala's `Future`s instead of `Promise` as much as
 *  possible. A `Promise` can be converted to a `Future` with `.toFuture` and
 *  back with `.toJSPromise` (provided by [[JSConverters]]).
 *
 *  With
 *  {{{
 *  import scala.scalajs.js.Thenable.Implicits._
 *  }}}
 *  you can implicitly convert a `Promise` to a `Future`, and therefore you can
 *  directly use the methods of `Future` on `Promise`s.
 */
@js.native
class Promise[+A](
    executor: js.Function2[js.Function1[A | Thenable[A], _], js.Function1[scala.Any, _], _])
    extends js.Object with js.Thenable[A] {

  def `then`[B](
      onFulfilled: js.Function1[A, B | Thenable[B]],
      onRejected: js.UndefOr[js.Function1[scala.Any, B | Thenable[B]]] = js.undefined): Thenable[B] = js.native

  def `then`[B >: A](
      onFulfilled: Unit,
      onRejected: js.UndefOr[js.Function1[scala.Any, B | Thenable[B]]]): Thenable[B] = js.native

  def `catch`[B >: A](
      onRejected: js.UndefOr[js.Function1[scala.Any, B | Thenable[B]]] = js.undefined): Promise[B] = js.native
}

@js.native
object Promise extends js.Object {
  /** Returns a new [[Promise]] completed with the specified `value`. */
  def resolve[A](value: A | Thenable[A]): Promise[A] = js.native

  /** Returns a new [[Promise]] failed with the specified `reason`. */
  def reject(reason: scala.Any): Promise[Nothing] = js.native

  // TODO Use js.Iterable
  def all[A](promises: js.Array[_ <: Promise[A]]): Promise[js.Array[A]] = js.native

  // TODO Use js.Iterable
  def race[A](promises: js.Array[_ <: Promise[A]]): Promise[A] = js.native
}
