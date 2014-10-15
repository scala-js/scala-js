/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.classpath.builder

import scala.scalajs.tools.jsdep.JSDependencyManifest
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.io._

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

  override protected def handleJS(js: => VirtualJSFile): Unit = {
    val file = js
    if (!otherJSFiles.contains(file.name))
      otherJSFiles += file.name -> file
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
