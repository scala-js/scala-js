/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.classpath

import java.io._
import scala.scalajs.tools.io._

final case class ScalaJSClasspath(
    coreJSLibFile: VirtualJSFile,
    coreInfoFiles: Seq[VirtualFile],
    /** note that the class files are unordered
     *  use mainJSFiles for ordered class files
     */
    classFiles: Seq[VirtualScalaJSClassfile],
    otherJSFiles: Seq[VirtualJSFile] = Nil
) extends JSClasspath {
  import ScalaJSClasspath.sortScalaJSClassfiles
  lazy val mainJSFiles: Seq[VirtualJSFile] =
    coreJSLibFile +: sortScalaJSClassfiles(classFiles)
}

object ScalaJSClasspath {

  /** Reads and builds the Scala.js classpath entries in a File-based classpath. */
  def fromClasspath(classpath: Seq[File]): ScalaJSClasspath = {
    val builder = new ClasspathBuilder
    builder.readEntriesInClasspath(classpath)
    builder.result
  }

  /** Reads and builds the Scala.js classpath entries in a File-based classpath. */
  def partialFromClasspath(classpath: Seq[File]): ScalaJSClasspath = {
    val builder = new ClasspathBuilder
    builder.readEntriesInClasspath(classpath)
    builder.partialResult
  }

  private def sortScalaJSClassfiles(classFiles: Seq[VirtualScalaJSClassfile]) = {
    val withAncestorCount = classFiles.zip(classFiles.map(ancestorCountOf))
    val sorted = withAncestorCount sortWith { (lhs, rhs) =>
      if (lhs._2 != rhs._2) lhs._2 < rhs._2
      else lhs._1.name.compareTo(rhs._1.name) < 0
    }
    sorted.map(_._1)
  }

  private val AncestorCountLine =
    raw""""ancestorCount": *([0-9]+)""".r.unanchored

  private def ancestorCountOf(classFile: VirtualScalaJSClassfile): Int = {
    classFile.info match {
      case AncestorCountLine(countStr) => countStr.toInt
      case _ =>
        throw new AssertionError(s"Did not find ancestor count in $classFile")
    }
  }

}
