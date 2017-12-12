/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.js

import scala.language.implicitConversions

import scala.scalajs.js
import scala.scalajs.js.annotation._

import scala.collection._
import scala.concurrent.{ExecutionContext, Future}

import scala.scalajs.runtime.genTraversableOnce2jsArray

sealed abstract class JSConvertersLowPrioImplicits {
  this: js.JSConverters.type =>

  @inline
  implicit def JSRichFutureNonThenable[A](f: Future[A]): JSRichFuture[A] =
    new JSRichFuture[A](f)

}

/** A collection of decorators that allow converting Scala types to
 *  corresponding JS facade types
 */
object JSConverters extends js.JSConvertersLowPrioImplicits {

  implicit class JSRichOption[T](val opt: Option[T]) extends AnyVal {
    @inline final def orUndefined: js.UndefOr[T] =
      opt.fold[js.UndefOr[T]](undefined)(v => v)
  }

  implicit class JSRichGenTraversableOnce[T](
      val col: GenTraversableOnce[T]) extends AnyVal {
    @inline final def toJSArray: js.Array[T] = genTraversableOnce2jsArray(col)
  }

  implicit class JSRichGenIterable[T](
      val __self: GenIterable[T]) extends AnyVal {
    @inline final def toJSIterable: js.Iterable[T] = new IterableAdapter(__self)
  }

  implicit class JSRichIterator[T](
      val __self: scala.collection.Iterator[T]) extends AnyVal {
    @inline final def toJSIterator: js.Iterator[T] = new IteratorAdapter(__self)
  }

  private class IterableAdapter[+T](col: GenIterable[T]) extends js.Iterable[T] {
    @JSName(js.Symbol.iterator)
    final def jsIterator(): js.Iterator[T] = col.iterator.toJSIterator
  }

  private class IteratorAdapter[+T](
      it: scala.collection.Iterator[T]) extends js.Iterator[T] {
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

  implicit class JSRichGenMap[T](val map: GenMap[String, T]) extends AnyVal {
    @inline final def toJSDictionary: js.Dictionary[T] = {
      val result = js.Dictionary.empty[T]
      map.foreach { case (key, value) => result(key) = value }
      result
    }
  }

  @inline
  implicit def genTravConvertible2JSRichGenTrav[T, C](coll: C)(
      implicit ev: C => GenTraversableOnce[T]): JSRichGenTraversableOnce[T] =
    new JSRichGenTraversableOnce(coll)

  @inline
  implicit def JSRichFutureThenable[A](f: Future[js.Thenable[A]]): JSRichFuture[A] =
    new JSRichFuture[A](f)

  final class JSRichFuture[A](val self: Future[A | js.Thenable[A]]) extends AnyVal {
    /** Converts the Future to a JavaScript [[Promise]].
     *
     *  Attention! The nature of the [[Promise]] class, from the ECMAScript
     *  specification, makes this method inherently un-typeable, because it is
     *  not type parametric.
     *
     *  The signature of the `toJSPromise` method is only valid
     *  <i>provided that</i> the values of `A` do not have a `then` method.
     */
    def toJSPromise(implicit executor: ExecutionContext): js.Promise[A] = {
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
