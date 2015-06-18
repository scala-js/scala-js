/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.sbtplugin

import java.io.{File, FileNotFoundException}

import scala.collection.mutable

import sbt._
import sbt.complete._

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.jsdep.JSDependencyManifest
import org.scalajs.core.tools.classpath.builder._

private[sbtplugin] object ScalajspUtils {

  /** Pairs (relPath, loadIR), "elements" of a ClasspathIRTraverser. */
  private type RelPathAndIRFile = (String, () => VirtualScalaJSIRFile)

  /** Lists all the .sjsir files on a classpath. */
  def listSjsirFilesOnClasspath(cp: Seq[File]): List[String] =
    new ClasspathIRTraverser(cp).map(_._1).toList

  /** Creates an [[ExampleSource]] with per-directory tab completion. */
  def relPathsExamples(relPaths: List[String]): ExampleSource =
    new ScalaJSIRFilesOnClasspathExamples(relPaths)

  /** Loads an .sjsir file by its relative path on a classpath. */
  def loadIRFile(cp: Seq[File], relPath: String): VirtualScalaJSIRFile = {
    new ClasspathIRTraverser(cp) collectFirst {
      case (fileRelPath, loadIR) if fileRelPath == relPath => loadIR()
    } getOrElse {
      throw new FileNotFoundException(relPath)
    }
  }

  /** An [[ExampleSource]] showing .sjsir files on a classpath. */
  private class ScalaJSIRFilesOnClasspathExamples(allRelPaths: List[String],
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

  /** A [[Traversable]] listing the .sjsir files in a classpath. */
  private class ClasspathIRTraverser(cp: Seq[File])
      extends ClasspathElementsTraverser
      with PhysicalFileSystem
      with Traversable[RelPathAndIRFile] {

    private var f: RelPathAndIRFile => Any = null

    override protected def handleIR(relPath: String,
        ir: => VirtualScalaJSIRFile): Unit = {
      f((relPath, () => ir))
    }

    override protected def handleJS(relPath: String,
        js: => VirtualJSFile): Unit = {
    }

    override protected def handleDepManifest(
        m: => JSDependencyManifest): Unit = {
    }

    override def foreach[U](f: RelPathAndIRFile => U): Unit = {
      this.f = f
      try traverseClasspathElements(cp)
      finally this.f = null
    }

    /* Backport performance improvement from
     * https://github.com/scala/scala/commit/56c1af90999f0a9a55a90f3dd3bf6d04ddda5943
     */
    override def collectFirst[B](
        pf: PartialFunction[RelPathAndIRFile, B]): Option[B] = {
      this.foreach(pf.runWith(b => return Some(b))) // scalastyle:ignore
      None
    }

  }

}
