/* Scala.js sbt plugin
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.sbtplugin

import scala.annotation.tailrec

import java.io._

import scala.scalajs.tools.io._
import scala.scalajs.tools.sourcemap._

import Utils._

object SourceMapCat {
  /** Concatenate JS files and their respective source maps
   *  In this implementation, source maps are assumed to be named after their
   *  JS file, with an additional .map extension (hence it likely ends in
   *  .js.map).
   */
  def catJSFilesAndTheirSourceMaps(inputs: Seq[File], output: File,
      relativizeSourceMapPaths: Boolean) {

    val outputWriter = new PrintWriter(output, "UTF-8")
    val sourceMapWriter = new PrintWriter(changeExt(output, ".js", ".js.map"))
    try {
      val relativizeSourceMapBase =
        if (relativizeSourceMapPaths) Some(output.getParentFile.toURI)
        else None

      val builder = new JSFileBuilderWithSourceMap(output.getName,
          outputWriter, sourceMapWriter, relativizeSourceMapBase)

      for (input <- inputs)
        builder.addFile(FileVirtualJSFile(input))

      builder.complete()
    } finally {
      outputWriter.close()
      sourceMapWriter.close()
    }
  }
}
