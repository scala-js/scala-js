/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.classpath.builder

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.jsdep._
import org.scalajs.core.tools.classpath._

import java.io._

import scala.collection.immutable.Seq

/** Base-trait used by traversers to handle content with callbacks */
trait ClasspathContentHandler {
  protected def handleIR(relPath: String, ir: => VirtualScalaJSIRFile): Unit
  protected def handleJS(relPath: String, js: => VirtualJSFile): Unit
  protected def handleDepManifest(m: => JSDependencyManifest): Unit
}
