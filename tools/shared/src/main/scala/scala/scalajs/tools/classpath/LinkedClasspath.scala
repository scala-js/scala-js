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

/** A [[CompleteClasspath]] that is fully linked (either with the
 *  [[ScalaJSOptimizer]] or the Closure Optimizer. It contains only a single
 *  file that is scalaJSCode.
 */
final class LinkedClasspath(
    jsLibs: Seq[(VirtualJSFile, ResolutionInfo)],
    val scalaJSCode: VirtualJSFile,
    requiresDOM: Boolean,
    version: Option[String]
) extends CompleteClasspath(jsLibs, requiresDOM, version)
