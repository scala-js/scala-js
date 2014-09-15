/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.js

import scala.language.implicitConversions

import scala.collection._

import scala.scalajs.runtime.genTraversableOnce2jsArray

/** A collection of decorators that allow converting Scala types to
 *  corresponding JS facade types
 */
object JSConverters {

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

}
