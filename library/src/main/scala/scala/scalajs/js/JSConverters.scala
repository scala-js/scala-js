/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.js

/** A collection of decorators that allow converting Scala types to
 *  corresponding JS facade types
 */ 
object JSConverters {

  implicit class JSRichOption[T](val opt: Option[T]) extends AnyVal {
    @inline final def orUndefined: UndefOr[T] =
      opt.fold[UndefOr[T]](undefined)(v => v)
  }

}
