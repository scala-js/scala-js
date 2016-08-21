/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2016, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.js.annotation.internal

/** IMPLEMENTATION DETAIL: Marks this JS native module class as having stored
 *  a loading spec in its IR.
 *
 *  This is true iff the module class was compiled with Scala.js 0.6.13 or
 *  later.
 *
 *  Do not use this annotation yourself.
 */
class HasJSNativeLoadSpec extends scala.annotation.StaticAnnotation
