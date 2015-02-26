/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.classpath.builder

import org.scalajs.core.tools.jsdep.JSDependencyManifest
import org.scalajs.core.tools.classpath._
import org.scalajs.core.tools.io._

import scala.collection.mutable
import scala.collection.immutable.Seq

trait AbstractPartialClasspathBuilder extends ClasspathContentHandler
                                         with ClasspathElementsTraverser {

  private val jsDepManifests = mutable.ListBuffer.empty[JSDependencyManifest]
  private val irFiles = mutable.Map.empty[String, VirtualScalaJSIRFile]
  private val otherJSFiles = mutable.Map.empty[String, VirtualJSFile]

  override protected def handleIR(relPath: String,
      ir: => VirtualScalaJSIRFile): Unit = {
    if (!irFiles.contains(relPath))
      irFiles += relPath -> ir
  }

  override protected def handleJS(relPath: String,
      js: => VirtualJSFile): Unit = {
    val file = js
    if (!otherJSFiles.contains(relPath))
      otherJSFiles += relPath -> file
  }

  override protected def handleDepManifest(m: => JSDependencyManifest): Unit = {
    jsDepManifests += m
  }

  def build(cp: Seq[File]): PartialClasspath = {
    val version = traverseClasspathElements(cp)
    new PartialClasspath(jsDepManifests.toList, otherJSFiles.toMap,
        irFiles.values.toList, Some(version))
  }
}
