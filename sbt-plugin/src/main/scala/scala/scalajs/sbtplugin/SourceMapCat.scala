/* Scala.js sbt plugin
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.sbtplugin

import sbt._

import java.io.PrintWriter
import java.nio.file.Paths

import com.google.debugging.sourcemap.{ FilePosition, _ }

object SourceMapCat {
  /** Concatenate JS files and their respective source maps
   *  In this implementation, source maps are assumed to be named after their
   *  JS file, with an additional .map extension (hence it likely ends in
   *  .js.map).
   */
  def catJSFilesAndTheirSourceMaps(inputs: Seq[File],
                                   output: File,
                                   relativizeSourceMapPaths: Boolean) {
    val outcode = new PrintWriter(output)
    val outMapFile = sourceMapOf(output)
    val outmap = new PrintWriter(outMapFile)
    val outmapGen = SourceMapGeneratorFactory.getInstance(SourceMapFormat.V3)
    // java.net.URI.relativize doesn't work well with backward paths (../../..),
    // so java.nio.file.Paths is used to do the work
    val basePath = Paths.get(output.getParent)

    var totalLineCount = 0

    for (input <- inputs) {
      val offset = totalLineCount

      val lines = IO.readLines(input)
      val lineCount = lines.size

      // Cat the file - remove references to source maps
      for (line <- lines) {
        if (line startsWith "//@ sourceMappingURL=") outcode.println()
        else outcode.println(line)
      }

      // Cat the source map
      val sourceMapFile = sourceMapOf(input)
      if (sourceMapFile.exists) {
        /* The source map exists.
         * Visit all the mappings in this source map, and add them to the
         * concatenated source map with the appropriate offset.
         */
        val consumer = new SourceMapConsumerV3
        consumer.parse(IO.read(sourceMapFile))

        consumer.visitMappings(new SourceMapConsumerV3.EntryVisitor {
          override def visit(sourceName: String, symbolName: String,
              sourceStartPos: FilePosition,
              startPos: FilePosition, endPos: FilePosition) {

            val offsetStartPos =
              new FilePosition(startPos.getLine+offset, startPos.getColumn)
            val offsetEndPos =
              new FilePosition(endPos.getLine+offset, endPos.getColumn)
            val finalSourceName =
              if (relativizeSourceMapPaths) {
                val fsPath = Paths.get((new java.net.URI(sourceName)).getPath)
                basePath.relativize(fsPath).toString
              } else
                sourceName.toString

            outmapGen.addMapping(finalSourceName, symbolName,
                sourceStartPos, offsetStartPos, offsetEndPos)
          }
        })
      } else {
        /* The source map does not exist.
         * This happens typically for corejslib.js and other helper files
         * written directly in JS.
         * We generate a fake line-by-line source map for these on the fly
         */
        val finalSourceName =
          if (relativizeSourceMapPaths)
            basePath.relativize(Paths.get(input.getPath)).toString
          else
            input.getPath.toString

        for (lineNumber <- 0 until lineCount) {
          val sourceStartPos = new FilePosition(lineNumber, 0)
          val startPos = new FilePosition(offset+lineNumber, 0)
          val endPos = new FilePosition(offset+lineNumber+1, 0)

          outmapGen.addMapping(finalSourceName, null,
              sourceStartPos, startPos, endPos)
        }
      }

      totalLineCount += lineCount
    }

    outcode.println("//@ sourceMappingURL=" + outMapFile.getName)

    outmapGen.appendTo(outmap, output.getName)

    outcode.close()
    outmap.close()
  }

  private def sourceMapOf(jsfile: File): File =
    jsfile.getParentFile / (jsfile.getName+".map")
}
