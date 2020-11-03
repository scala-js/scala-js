/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.scalajs.js

import scala.language.implicitConversions

import scala.scalajs.js
import scala.scalajs.js.annotation._

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
@JSGlobal
class Promise[+A](
    executor: js.Function2[js.Function1[A | js.Thenable[A], _], js.Function1[scala.Any, _], _])
    extends js.Object with js.Thenable[A] {

  def `then`[B](onFulfilled: js.Function1[A, B | js.Thenable[B]],
      onRejected: js.UndefOr[js.Function1[scala.Any, B | js.Thenable[B]]] =
        js.undefined): js.Promise[B] = js.native

  def `then`[B >: A](onFulfilled: Unit,
      onRejected: js.UndefOr[
          js.Function1[scala.Any, B | js.Thenable[B]]]): js.Promise[B] = js.native

  def `catch`[B >: A](
      onRejected: js.UndefOr[js.Function1[scala.Any, B | js.Thenable[B]]] =
        js.undefined): js.Promise[B] = js.native
}

@js.native
@JSGlobal
object Promise extends js.Object {

  /** Returns a new [[Promise]] completed with the specified `value`. */
  def resolve[A](value: A | js.Thenable[A]): js.Promise[A] = js.native

  /** Returns a new [[Promise]] failed with the specified `reason`. */
  def reject(reason: scala.Any): js.Promise[Nothing] = js.native

  def all[A](promises: js.Iterable[js.Promise[A]]): js.Promise[js.Array[A]] = js.native

  def race[A](promises: js.Iterable[js.Promise[A]]): js.Promise[A] = js.native
}
