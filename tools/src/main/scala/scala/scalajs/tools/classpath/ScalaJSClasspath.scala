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
    classFiles: Seq[VirtualScalaJSClassfile],
    otherJSFiles: Seq[VirtualJSFile] = Nil
) extends JSClasspath {
  def mainJSFiles: Seq[VirtualJSFile] = coreJSLibFile +: classFiles
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

}
