/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.classpath.builder

import scala.collection.mutable

import scala.scalajs.tools.io._
import scala.scalajs.tools.jsdep.JSDependencyManifest
import scala.scalajs.tools.classpath._

/** reads a ScalaJS library JAR into a CP
 *  - IR files go to scalaJSCode
 *  - JS files go to availableLibs
 *  - Reads a potential top-level JS_DEPENDENCIES file
 */
trait AbstractJarLibClasspathBuilder extends JarTraverser {

  private val irFiles = mutable.ListBuffer.empty[VirtualScalaJSIRFile]
  private val jsFiles = mutable.Map.empty[String, VirtualJSFile]
  private var dependency: Option[JSDependencyManifest] = None

  def build(jar: File): PartialClasspath = {
    val v = traverseJar(jar)
    new PartialClasspath(dependency.toList,
        jsFiles.toMap, irFiles.toList, Some(v))
  }

  override protected def handleIR(relPath: String,
      ir: => VirtualScalaJSIRFile): Unit = {
    // We don't need to implement shadowing here: We have only a single JAR
    irFiles += ir
  }

  override protected def handleJS(js: => VirtualJSFile): Unit = {
    val file = js
    if (!jsFiles.contains(file.name))
      jsFiles += file.name -> file
  }

  override protected def handleDepManifest(m: => JSDependencyManifest): Unit = {
    if (dependency.isDefined)
      sys.error("A JAR cannot have multiple JS dependency manifests")
    dependency = Some(m)
  }

}
