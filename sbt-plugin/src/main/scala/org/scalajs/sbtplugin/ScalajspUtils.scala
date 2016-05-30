/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.sbtplugin

import scala.collection.mutable

import sbt._
import sbt.complete._

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.jsdep.JSDependencyManifest

private[sbtplugin] object ScalajspUtils {

  /** Creates an [[ExampleSource]] with per-directory tab completion. */
  def relPathsExamples(relPaths: Seq[String]): ExampleSource =
    new ScalaJSIRFilesOnClasspathExamples(relPaths)

  /** An [[ExampleSource]] showing .sjsir files on a classpath. */
  private class ScalaJSIRFilesOnClasspathExamples(allRelPaths: Seq[String],
      prefix: String = "") extends ExampleSource {

    override def apply(): Iterable[String] = {
      val allExamples = (for {
        relPath <- allRelPaths
        if relPath.startsWith(prefix)
      } yield {
        // Returned examples must not include the prefix
        val remaining = relPath.substring(prefix.length)

        /* For files in subdirectories wrt. the current prefix, do not show
         * the entire remaining path. Instead, show only the remaining path
         * to the subdirectory, '/' included. This means that:
         *
         * > scalajsp hello<tab>
         *
         * will complete to
         *
         * > scalajsp helloworld/
         *
         * and further tabs are necessary to show to files and directories
         * under the helloworld/ subdirectory.
         */
        val nextSlashPos = remaining.indexOf('/')
        if (nextSlashPos == -1) remaining
        else remaining.substring(0, nextSlashPos + 1)
      }).distinct

      val (dirs, files) = allExamples.partition(_.endsWith("/"))
      dirs.sorted ++ files.sorted
    }

    override def withAddedPrefix(addedPrefix: String): ExampleSource =
      new ScalaJSIRFilesOnClasspathExamples(allRelPaths, prefix + addedPrefix)
  }

}
