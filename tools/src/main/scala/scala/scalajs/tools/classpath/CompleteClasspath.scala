/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.classpath

import scala.collection.immutable.Seq

import scala.scalajs.tools.io.VirtualJSFile

/** A classpath where nothing is missing. Therefore:
 *  - All JS libraries are resolved and ordered
 *  - The CoreJSLibs are present
 *  - Nothing can be added anymore
 */
abstract class CompleteClasspath(val version: Option[String]) {
  /** All code in this complete classpath */
  def allCode: Seq[VirtualJSFile]
}
