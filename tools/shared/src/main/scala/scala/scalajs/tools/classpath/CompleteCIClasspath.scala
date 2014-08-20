/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.classpath

import scala.scalajs.tools.io.VirtualJSFile
import scala.scalajs.tools.jsdep.ResolutionInfo

import scala.collection.immutable.Seq

/** A complete classpath with Closure intended code: Any code that can/should go
 *  through the Google Closure Compiler (in advanced mode). This is (in general)
 *  all Scala.js generated code (maybe even in IR form, see CompleteIRClasspath)
 */
abstract class CompleteCIClasspath(
    jsLibs: Seq[(VirtualJSFile, ResolutionInfo)], requiresDOM: Boolean,
    version: Option[String]
) extends CompleteClasspath(jsLibs, requiresDOM, version) {

  final def scalaJSCode: Seq[VirtualJSFile] = cijsCode
  def cijsCode: Seq[VirtualJSFile]
}

object CompleteCIClasspath {

  private class SimpleCompleteCIClasspath(
      jsLibs: Seq[(VirtualJSFile, ResolutionInfo)],
      val cijsCode: Seq[VirtualJSFile],
      requiresDOM: Boolean,
      version: Option[String]
  ) extends CompleteCIClasspath(jsLibs, requiresDOM, version)

  /** Creates a CompleteCIClasspath with the specified contents. */
  def apply(jsLibs: Seq[(VirtualJSFile, ResolutionInfo)],
      cijsCode: Seq[VirtualJSFile], requiresDOM: Boolean,
      version: Option[String]): CompleteCIClasspath =
    new SimpleCompleteCIClasspath(jsLibs, cijsCode, requiresDOM, version)

}
