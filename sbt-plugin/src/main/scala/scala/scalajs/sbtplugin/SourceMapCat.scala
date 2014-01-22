/* Scala.js sbt plugin
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.sbtplugin

import sbt._

import scala.annotation.tailrec

import java.io.PrintWriter

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
    val basePath = output.getParent

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
              if (relativizeSourceMapPaths) 
                relativizePath(basePath, (new java.net.URI(sourceName)).getPath)
              else
                sourceName

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
            relativizePath(basePath, input.getPath)
          else
            input.getPath

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

  private def relativizePath(base: String, path: String): String = {
    import java.io.File

    def getPathSegments(path: String) =
      path.split(File.separator).toList.filter(_.length > 0).filter(_ != ".")
        .foldLeft(List[String]()) { (p, s) => if (s == "..") p.tail else s :: p }
        .reverse

    @tailrec
    def dropCommonSegments(x: List[String], y: List[String]): (List[String], List[String]) =
      if ((x == Nil) || (y == Nil) || (x.head != y.head))
        (x, y)
      else
        dropCommonSegments(x.tail, y.tail)

    val absbase = (new File(base)).getAbsolutePath
    val abspath = (new File(path)).getAbsolutePath

    // On unixes all abs paths starts with '/'.
    // On windows the abs paths starts with drive letter and if the drives aren't
    // the same, there is no relative path. So return abspath.
    if (absbase(0) == abspath(0)) {
      val (restofbase, restofpath) =
        dropCommonSegments(getPathSegments(absbase), getPathSegments(abspath))
      val relative = List.fill(restofbase.length)("..") ::: restofpath

      relative.mkString(File.separator)
    } else {
      abspath
    }
  }
}
