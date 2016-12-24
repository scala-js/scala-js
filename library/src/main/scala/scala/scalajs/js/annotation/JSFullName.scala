/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package scala.scalajs.js.annotation

/** IMPLEMENTATION DETAIL: Saves the fully qualified JS name of a symbol.
 *
 *  This annotation was used prior to Scala.js 0.6.13. It is only kept for
 *  backwards binary compatibility, and should not be used anymore.
 *
 *  Do not use this annotation yourself.
 */
@deprecated("Replaced by internal.JSNativeLoadSpec.", "0.6.13")
class JSFullName(fullName: String) extends scala.annotation.StaticAnnotation
