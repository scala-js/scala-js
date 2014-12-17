/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.js

import scala.language.implicitConversions

/** Provides implicit conversions and operations to write in JavaScript
 *  style with [[Dynamic js.Dynamic]].
 *
 *  Be **very** careful when importing members of this object. You may want
 *  to selectively import the implicits that you want to reduce the likelihood
 *  of making mistakes.
 */
object DynamicImplicits {
  @inline implicit def truthValue(x: Dynamic): Boolean =
    (!(!x)).asInstanceOf[Boolean]

  // Useful for Scala 2.10
  implicit def number2dynamic(x: Int): Dynamic =
    x.asInstanceOf[Dynamic]

  implicit def number2dynamic(x: Double): Dynamic =
    x.asInstanceOf[Dynamic]

  implicit def boolean2dynamic(x: Boolean): Dynamic =
    x.asInstanceOf[Dynamic]
}
