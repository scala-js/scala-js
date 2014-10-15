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

/** A classpath where nothing is missing. Therefore:
 *  - All JS libraries are resolved and ordered
 *  - The CoreJSLibs are present
 *  - Nothing can be added anymore
 */
abstract class CompleteClasspath(
    /** Resolved JS libraries */
    val jsLibs: Seq[(VirtualJSFile, ResolutionInfo)],
    val requiresDOM: Boolean,
    val version: Option[String]
) {

  /** Fully linked Scala.js code */
  def scalaJSCode: VirtualJSFile

  /** All code in this complete classpath */
  final def allCode: Seq[VirtualJSFile] =
    jsLibs.map(_._1) :+ scalaJSCode
}
