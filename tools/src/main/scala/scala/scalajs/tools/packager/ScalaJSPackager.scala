/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.packager

import java.io._
import java.net.URI

import scala.scalajs.tools.logging._
import scala.scalajs.tools.io._
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.sourcemap._

import ScalaJSPackedClasspath.{ writePackInfo, PackInfoData }

/** Scala.js packager: concatenates blindly all Scala.js class files. */
class ScalaJSPackager {
  import ScalaJSPackager._

  /** Package Scala.js output files as a single .js file.
   *  See [[ScalaJSOptimizer.Inputs]] for details about the required and
   *  optional inputs.
   *  See [[ScalaJSOptimizer.OutputConfig]] for details about the configuration
   *  for the output of this method.
   *  Returns a [[ScalaJSOptimizer.Result]] containing the result of the
   *  packaging. Its output file will contain, in that order:
   *  1. The Scala.js core lib,
   *  2. The Scala.js class files ordered appropriately,
   *  3. The custom .js files, in the same order as they were listed in inputs.
   */
  def packageScalaJS(inputs: Inputs, outputConfig: OutputConfig,
      logger: Logger): Unit = {
    val classpath = inputs.classpath
    val allSortedFiles =
      Seq(classpath.coreJSLibFile) ++
      sortScalaJSClassfiles(classpath.classFiles) ++
      inputs.customScripts

    import outputConfig._

    val builder = {

      if (wantSourceMap)
        new JSFileBuilderWithSourceMap(name,
            writer.contentWriter,
            writer.sourceMapWriter,
            relativizeSourceMapBase)
      else
        new JSFileBuilder(name, writer.contentWriter)
    }

    for (file <- allSortedFiles)
      builder.addFile(file)
    builder.complete()

    writePackInfo(writer, PackInfoData(packOrder))
  }
}

object ScalaJSPackager {
  /** Inputs of the Scala.js optimizer. */
  final case class Inputs(
      /** The (partial) Scala.js classpath entries. */
      classpath: ScalaJSClasspath,
      /** Additional scripts to be appended in the output. */
      customScripts: Seq[VirtualJSFile] = Nil
  )

  /** Configuration for the output of the Scala.js optimizer. */
  final case class OutputConfig(
      /** Name of the output file. (used to refer to sourcemaps) */
      name: String,
      /** Print writer for the output file. */
      writer: VirtualScalaJSPackfileWriter,
      /** Pack order written to the .sjspack file to ensure proper ordering by
       *  other tools (notably when reading a JSClasspath from the packed files)
       */
      packOrder: Int,
      /** Ask to produce source map for the output. */
      wantSourceMap: Boolean = false,
      /** Base path to relativize paths in the source map. */
      relativizeSourceMapBase: Option[URI] = None
  )

  private val AncestorCountLine =
    raw""""ancestorCount": *([0-9]+)""".r.unanchored

  private def sortScalaJSClassfiles(classFiles: Seq[VirtualScalaJSClassfile]) = {
    val withAncestorCount = classFiles.zip(classFiles.map(ancestorCountOf))
    val sorted = withAncestorCount sortWith { (lhs, rhs) =>
      if (lhs._2 != rhs._2) lhs._2 < rhs._2
      else lhs._1.name.compareTo(rhs._1.name) < 0
    }
    sorted.map(_._1)
  }

  private def ancestorCountOf(classFile: VirtualScalaJSClassfile): Int = {
    classFile.info match {
      case AncestorCountLine(countStr) => countStr.toInt
      case _ =>
        throw new AssertionError(s"Did not find ancestor count in $classFile")
    }
  }
}
