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

import scala.collection._
import scala.concurrent.{ExecutionContext, Future}

sealed abstract class JSConvertersLowPrioImplicits { this: JSConverters.type =>

  @inline
  implicit def JSRichFutureNonThenable[A](f: Future[A]): JSRichFuture[A] =
    newJSRichFuture[A](f)

}

/** A collection of decorators that allow converting Scala types to
 *  corresponding JS facade types
 */
object JSConverters extends JSConvertersLowPrioImplicits {

  implicit class JSRichOption[T] private[JSConverters] (
      private val opt: Option[T])
      extends AnyVal {

    @inline final def orUndefined: js.UndefOr[T] =
      opt.fold[js.UndefOr[T]](undefined)(v => v)
  }

  implicit class JSRichIterableOnce[T] private[JSConverters] (
      private val col: IterableOnce[T])
      extends AnyVal {

    final def toJSArray: js.Array[T] = {
      col match {
        case col: js.WrappedArray[T] =>
          // Avoiding a copy is consistent with Scala behavior for Arrays.
          WrappedArray.toJSArray(col)

        case _ =>
          val result = new js.Array[T]
          col.iterator.foreach(x => result.push(x))
          result
      }
    }
  }

  implicit class JSRichIterable[T] private[JSConverters] (
      private val self: scala.collection.Iterable[T])
      extends AnyVal {

    @inline final def toJSIterable: js.Iterable[T] = new IterableAdapter(self)
  }

  implicit class JSRichIterator[T] private[JSConverters] (
      private val self: scala.collection.Iterator[T])
      extends AnyVal {

    @inline final def toJSIterator: js.Iterator[T] = new IteratorAdapter(self)
  }

  private class IterableAdapter[+T](col: collection.Iterable[T])
      extends js.Iterable[T] {

    @JSName(js.Symbol.iterator)
    final def jsIterator(): js.Iterator[T] = col.iterator.toJSIterator
  }

  private class IteratorAdapter[+T](it: scala.collection.Iterator[T])
      extends js.Iterator[T] {

    final def next(): js.Iterator.Entry[T] = {
      if (it.hasNext) {
        new js.Iterator.Entry[T] {
          val done: Boolean = false
          val value: T = it.next()
        }
      } else {
        new js.Iterator.Entry[T] {
          val done: Boolean = true
          // Evil cast to work around typing. By specification, reading `value`
          // is undefined behavior, so this is ok.
          val value: T = js.undefined.asInstanceOf[T]
        }
      }
    }
  }

  implicit class JSRichMap[T] private[JSConverters] (
      private val map: Map[String, T])
      extends AnyVal {

    @inline final def toJSDictionary: js.Dictionary[T] = {
      val result = js.Dictionary.empty[T]
      map.foreach { case (key, value) => result(key) = value }
      result
    }
  }

  implicit final class JSRichGenMapKV[K, V] private[JSConverters] (
      private val self: Map[K, V])
      extends AnyVal {

    @inline final def toJSMap: js.Map[K, V] = {
      val result = js.Map.empty[K, V]
      self.foreach { case (key, value) =>
        result.asInstanceOf[js.Map.Raw[K, V]].set(key, value)
      }
      result
    }
  }

  implicit final class JSRichSet[T] private[JSConverters] (
      private val self: Set[T])
      extends AnyVal {

    @inline final def toJSSet: js.Set[T] = {
      val result = js.Set.empty[T]
      self.foreach { value => result.add(value) }
      result
    }
  }

  @inline
  implicit def iterableOnceConvertible2JSRichIterableOnce[T, C](coll: C)(
      implicit ev: C => IterableOnce[T]): JSRichIterableOnce[T] =
    new JSRichIterableOnce(coll)

  @inline
  implicit def JSRichFutureThenable[A](f: Future[js.Thenable[A]]): JSRichFuture[A] =
    new JSRichFuture[A](f)

  // For access in JSConvertersLowPrioImplicits
  @inline
  protected[this] def newJSRichFuture[A](
      f: Future[A | js.Thenable[A]]): JSRichFuture[A] = {
    new JSRichFuture[A](f)
  }

  final class JSRichFuture[A] private[JSConverters] (
      private val self: Future[A | js.Thenable[A]])
      extends AnyVal {

    /** Converts the Future to a JavaScript [[Promise]].
     *
     *  Attention! The nature of the [[Promise]] class, from the ECMAScript
     *  specification, makes this method inherently un-typeable, because it is
     *  not type parametric.
     *
     *  The signature of the `toJSPromise` method is only valid
     *  <i>provided that</i> the values of `A` do not have a `then` method.
     */
    def toJSPromise(implicit ec: ExecutionContext): js.Promise[A] = {
      new js.Promise[A]({
        (resolve: js.Function1[A | js.Thenable[A], _], reject: js.Function1[scala.Any, _]) =>
          self onComplete {
            case scala.util.Success(value) =>
              resolve(value)

            case scala.util.Failure(th) =>
              reject(th match {
                case js.JavaScriptException(e) => e
                case _                         => th
              })
          }
      })
    }
  }

}
