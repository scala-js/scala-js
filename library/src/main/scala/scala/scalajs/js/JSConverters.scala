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

import scala.collection._
import scala.concurrent.{ExecutionContext, Future}

import scala.scalajs.runtime.genTraversableOnce2jsArray

sealed abstract class JSConvertersLowPrioImplicits { this: JSConverters.type =>

  @inline
  implicit def JSRichFutureNonThenable[A](f: Future[A]): JSRichFuture[A] =
    new JSRichFuture[A](f.asInstanceOf[Future[A | Thenable[A]]])

}

/** A collection of decorators that allow converting Scala types to
 *  corresponding JS facade types
 */
object JSConverters extends JSConvertersLowPrioImplicits {

  implicit class JSRichOption[T](val opt: Option[T]) extends AnyVal {
    @inline final def orUndefined: UndefOr[T] =
      opt.fold[UndefOr[T]](undefined)(v => v)
  }

  implicit class JSRichGenTraversableOnce[T](
      val col: GenTraversableOnce[T]) extends AnyVal {
    @inline final def toJSArray: Array[T] = genTraversableOnce2jsArray(col)
  }

  implicit class JSRichGenMap[T](val map: GenMap[String, T]) extends AnyVal {
    @inline final def toJSDictionary: Dictionary[T] = {
      val result = Dictionary.empty[T]
      map.foreach { case (key, value) => result(key) = value }
      result
    }
  }

  @inline
  implicit def genTravConvertible2JSRichGenTrav[T, C](coll: C)(
      implicit ev: C => GenTraversableOnce[T]): JSRichGenTraversableOnce[T] =
    new JSRichGenTraversableOnce(coll)

  /** Special case for scala.Array of [[genTravConvertible2JSRichGenTrav]].
   *  Needed for the 2.10.x series.
   */
  @inline
  implicit def array2JSRichGenTrav[T](
      arr: scala.Array[T]): JSRichGenTraversableOnce[T] =
    new JSRichGenTraversableOnce(arr)

  @inline
  implicit def JSRichFutureThenable[A](f: Future[Thenable[A]]): JSRichFuture[A] =
    new JSRichFuture[A](f.asInstanceOf[Future[A | Thenable[A]]])

  final class JSRichFuture[A](val self: Future[A | Thenable[A]]) extends AnyVal {
    /** Converts the Future to a JavaScript [[Promise]].
     *
     *  Attention! The nature of the [[Promise]] class, from the ECMAScript
     *  specification, makes this method inherently un-typeable, because it is
     *  not type parametric.
     *
     *  The signature of the `toJSPromise` method is only valid
     *  <i>provided that</i> the values of `A` do not have a `then` method.
     */
    def toJSPromise(implicit executor: ExecutionContext): Promise[A] = {
      new Promise[A]({
        (resolve: js.Function1[A | Thenable[A], _], reject: js.Function1[scala.Any, _]) =>
          self onComplete {
            case scala.util.Success(value) =>
              resolve(value)

            case scala.util.Failure(th) =>
              reject(th match {
                case JavaScriptException(e) => e
                case _                      => th
              })
          }
      })
    }
  }

}
