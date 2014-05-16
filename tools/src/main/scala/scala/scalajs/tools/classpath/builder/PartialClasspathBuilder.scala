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

import java.io._

import scala.collection.mutable
import scala.collection.immutable.Seq

class PartialClasspathBuilder(cp: Seq[File]) extends ClasspathContentHandler
                                                with ClasspathElementsTraverser {

  private val jsDepManifests = mutable.ListBuffer.empty[JSDependencyManifest]
  private val irFiles = mutable.Map.empty[String, VirtualScalaJSIRFile]
  private val topLvlFiles = mutable.ListBuffer.empty[VirtualJSFile]
  private val otherJSFiles = mutable.Map.empty[String, VirtualJSFile]

  private def useIR: Boolean = topLvlFiles.isEmpty

  override protected def handleIR(relPath: String,
      ir: => VirtualScalaJSIRFile): Unit = {
    if (useIR && !irFiles.contains(relPath))
      irFiles += relPath -> ir
  }

  override protected def handleJS(js: => VirtualJSFile): Unit = {
    val file = js
    if (!otherJSFiles.contains(file.name))
      otherJSFiles += file.name -> file
  }

  override protected def handleTopLvlJS(js: => VirtualJSFile): Unit = {
    topLvlFiles += js
  }

  override protected def handleDepManifest(m: => JSDependencyManifest): Unit = {
    jsDepManifests += m
  }

  def build(): PartialClasspath = {
    val version = traverseClasspathElements(cp)
    if (useIR)
      mkIRCP(version)
    else
      mkCP(version)
  }

  def buildIR(): PartialIRClasspath = {
    val version = traverseClasspathElements(cp)
    if (!useIR) sys.error("This is not a PartialIRClasspath")
    mkIRCP(version)
  }

  private def mkIRCP(version: String) = {
    new PartialIRClasspath(
        jsDepManifests.toList, otherJSFiles.toMap,
        irFiles.values.toList, Some(version))
  }

  private def mkCP(version: String) = {
    PartialClasspath(
        jsDepManifests.toList, otherJSFiles.toMap,
        topLvlFiles.toList, Some(version))
  }
}

object PartialClasspathBuilder {

  /**
   * Create a PartialClasspathBuilder from the given (filesystem) classpath
   *
   * Rules for classpath reading:
   * - If top-level JS is present, all IR is ignored
   * - Top-level JS goes to scalaJSCode / IR goes to scalaJSIR
   * - If top-level JS is present, a PartialClasspath is created, otherwise a
   *   PartialIRClasspath is created
   * - Descends into JARs, no top-level JS in JARs
   * - Entries stay in order of ‘cp‘, IR remains unordered
   * - Earlier IR entries shadow later IR entries with the same relative path
   * - Non-top level JS goes to availableLibs (earlier libs take precedence)
   * - JS_DEPENDENCIES are added to dependencies
   */
  def apply(cp: Seq[File]): PartialClasspathBuilder =
    new PartialClasspathBuilder(cp)
}
