/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.classpath.builder

import java.io.File

import scala.scalajs.tools.io._
import scala.scalajs.tools.jsdep.JSDependencyManifest

import scala.collection.mutable

trait DirTraverser extends ClasspathContentHandler {

  /** Traverses elements, returns a version string */
  protected def traverseDir(dir: File): String = {
    val versions = mutable.SortedSet.empty[(String, String)]

    recurseDir(dir, "", versions)

    // Construct version
    CacheUtils.joinVersions(versions.toSeq.flatMap(x => Seq(x._1, x._2)): _*)
  }

  /** Recursively adds the Scala.js classpath entries in a directory */
  private def recurseDir(dir: File, dirPath: String,
      versions: mutable.SortedSet[(String, String)]): Unit = {
    val files = dir.listFiles
    for (file <- files) {
      val name = file.getName
      if (file.isDirectory) {
        recurseDir(file, dirPath + name + "/", versions)
      } else {
        def addToVersion() =
          versions += file.getAbsolutePath -> file.lastModified.toString
        val path = dirPath + name
        path match {
          case JSDependencyManifest.ManifestFileName =>
            addToVersion()
            handleDepManifest(JSDependencyManifest.read(file))

          case _ if name.endsWith(".js") =>
            addToVersion()
            handleJS(FileVirtualJSFile(file))

          case _ if name.endsWith(".sjsir") =>
            addToVersion()
            handleIR(path, FileVirtualScalaJSIRFile(file))

          case _ => // ignore other files
        }
      }
    }
  }

}
