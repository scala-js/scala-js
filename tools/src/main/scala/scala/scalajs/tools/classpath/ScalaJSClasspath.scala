/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.classpath

import java.io._

import scala.scalajs.ir
import scala.scalajs.tools.io._
import scala.scalajs.tools.sourcemap._

final case class ScalaJSClasspath(
    coreJSLibFile: VirtualJSFile,
    /** note that the class files are unordered
     *  use mainJSFiles for ordered class files
     */
    irFiles: Seq[VirtualScalaJSIRFile],
    otherJSFiles: Seq[VirtualJSFile] = Nil
) extends JSClasspath {
  import ScalaJSClasspath.desugarIRFiles
  lazy val mainJSFiles: Seq[VirtualJSFile] =
    coreJSLibFile +: desugarIRFiles(irFiles)
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

  private def desugarIRFiles(irFiles: Seq[VirtualScalaJSIRFile]): Seq[VirtualJSFile] = {
    val writer = new MemVirtualJSFileWriter
    val builder = new JSFileBuilderWithSourceMap(
        "packaged-file.js",
        writer.contentWriter,
        writer.sourceMapWriter,
        relativizeSourceMapBasePath = None)
    val infoAndTrees = irFiles.map(_.infoAndTree)
    for ((_, tree) <- infoAndTrees.sortBy(_._1.ancestorCount))
      builder.addIRTree(tree)
    builder.complete()
    Seq(writer.toVirtualFile("packaged-file.js"))
  }

}
