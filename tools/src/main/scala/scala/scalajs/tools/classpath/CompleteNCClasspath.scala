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
import scala.scalajs.tools.jsdep.ResolutionInfo

/** A complete classpath with non-Closure intended code: Any code that may not
 *  go through the Google Closure Compiler (in advanced mode).
 *  The ScalaJSClosureOptimizer emits such a classpath.
 */
class CompleteNCClasspath(
    jsLibs: Seq[(VirtualJSFile, ResolutionInfo)],
    val ncjsCode: Seq[VirtualJSFile],
    version: Option[String]
) extends CompleteClasspath(jsLibs, version) {
  override def scalaJSCode: Seq[VirtualJSFile] = ncjsCode
}
