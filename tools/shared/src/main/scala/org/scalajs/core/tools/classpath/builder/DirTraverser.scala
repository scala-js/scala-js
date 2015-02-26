/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.classpath.builder

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.jsdep.JSDependencyManifest

import scala.collection.mutable

trait DirTraverser extends ClasspathContentHandler with FileSystem {

  /** Traverses elements, returns a version string */
  protected def traverseDir(dir: File): String = {
    val versions = mutable.SortedSet.empty[String]

    recurseDir(dir, "", versions)

    // Construct version
    CacheUtils.joinVersions(versions.toSeq: _*)
  }

  /** Recursively adds the Scala.js classpath entries in a directory */
  private def recurseDir(dir: File, dirPath: String,
      versions: mutable.SortedSet[String]): Unit = {
    val files = listFiles(dir)
    for (file <- files) {
      val name = getName(file)
      if (isDirectory(file)) {
        recurseDir(file, dirPath + name + "/", versions)
      } else {
        val path = dirPath + name
        path match {
          case JSDependencyManifest.ManifestFileName =>
            versions += getGlobalVersion(file)
            val reader = toReader(file)
            try handleDepManifest(JSDependencyManifest.read(reader))
            finally reader.close()

          case _ if isJSFile(file) =>
            versions += getGlobalVersion(file)
            handleJS(path, toJSFile(file))

          case _ if isIRFile(file) =>
            versions += getGlobalVersion(file)
            handleIR(path, toIRFile(file))

          case _ => // ignore other files
        }
      }
    }
  }

}
