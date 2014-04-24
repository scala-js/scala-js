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
    val ancestorCountAndTrees =
      infoAndTrees.map(t => (extractCoreInfo(t._1)._2, t._2))
    for ((_, tree) <- ancestorCountAndTrees.sortBy(_._1))
      builder.addIRTree(tree)
    builder.complete()
    Seq(writer.toVirtualFile("packaged-file.js"))
  }

  /** Retrieves (encodedName, ancestorCount, isExported) from an info tree. */
  def extractCoreInfo(info: ir.Trees.Tree): (String, Int, Boolean) = {
    import ir.Trees._
    (info: @unchecked) match {
      case JSObjectConstr(fields) =>
        var encodedName: String = null
        var ancestorCount: Int = -1
        var isExported: Boolean = false
        fields foreach {
          case (StringLiteral("encodedName", _), StringLiteral(v, _)) =>
            encodedName = v
          case (StringLiteral("ancestorCount", _), IntLiteral(v)) =>
            ancestorCount = v
          case (StringLiteral("isExported", _), BooleanLiteral(v)) =>
            isExported = v
          case _ =>
        }

        if (encodedName == null)
          throw new AssertionError(s"Did not find encoded name in $info")
        if (ancestorCount < 0)
          throw new AssertionError(s"Did not find ancestor count in $info")

        (encodedName, ancestorCount, isExported)
    }
  }

}
