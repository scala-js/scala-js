/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.sourcemap

import scala.annotation.tailrec

import java.io.PrintWriter
import java.util.regex.Pattern

import com.google.debugging.sourcemap.{ FilePosition, _ }

import scala.scalajs.tools.io._

class JSFileBuilder(val name: String, protected val outputWriter: PrintWriter) {
  def addLine(line: String): Unit =
    outputWriter.println(line)

  def addLines(lines: Seq[String]): Unit =
    lines.foreach(addLine)

  def addFile(file: VirtualJSFile): Unit = {
    for (line <- file.readLines()) {
      if (line.startsWith("//@ sourceMappingURL="))
        addLine("")
      else
        addLine(line)
    }
  }

  def complete(): Unit = {
    // Can be overridden by subclasses
  }
}

class JSFileBuilderWithSourceMap(n: String,
    ow: PrintWriter, protected val sourceMapWriter: PrintWriter,
    relativizeSourceMapBasePath: Option[String] = None)
    extends JSFileBuilder(n, ow) {

  import JSFileBuilderWithSourceMap._

  protected val sourceMapGen: SourceMapGenerator =
    SourceMapGeneratorFactory.getInstance(SourceMapFormat.V3)

  protected var totalLineCount = 0

  protected def relPath(path: String): String = {
    relativizeSourceMapBasePath match {
      case Some(base) => relativizePath(base, path)
      case None => path
    }
  }

  override def addLine(line: String): Unit = {
    super.addLine(line)
    totalLineCount += 1
  }

  override def addFile(file: VirtualJSFile): Unit = {
    // Cat the file, record startLine and lineCount
    val startLine = totalLineCount
    super.addFile(file)
    val lineCount = totalLineCount - startLine

    // Cat the source map
    file.sourceMap match {
      case Some(sourceMap) =>
        /* The source map exists.
         * Visit all the mappings in this source map, and add them to the
         * concatenated source map with the appropriate offset.
         */
        val consumer = new SourceMapConsumerV3
        consumer.parse(sourceMap)

        consumer.visitMappings(new SourceMapConsumerV3.EntryVisitor {
          override def visit(sourceName: String, symbolName: String,
              sourceStartPos: FilePosition,
              startPos: FilePosition, endPos: FilePosition) {

            val offsetStartPos =
              new FilePosition(startPos.getLine+startLine, startPos.getColumn)
            val offsetEndPos =
              new FilePosition(endPos.getLine+startLine, endPos.getColumn)
            val relSourceName = relPath(new java.net.URI(sourceName).getPath)

            sourceMapGen.addMapping(relSourceName, symbolName,
                sourceStartPos, offsetStartPos, offsetEndPos)
          }
        })

      case None =>
        /* The source map does not exist.
         * This happens typically for corejslib.js and other helper files
         * written directly in JS.
         * We generate a fake line-by-line source map for these on the fly
         */
        val sourceName = file.name

        for (lineNumber <- 0 until lineCount) {
          val sourceStartPos = new FilePosition(lineNumber, 0)
          val startPos = new FilePosition(startLine+lineNumber, 0)
          val endPos = new FilePosition(startLine+lineNumber+1, 0)

          sourceMapGen.addMapping(sourceName, null,
              sourceStartPos, startPos, endPos)
        }
    }
  }

  override def complete(): Unit = {
    addLine("//@ sourceMappingURL=" + name + ".map")
    super.complete()

    sourceMapGen.appendTo(sourceMapWriter, name)
  }
}

object JSFileBuilderWithSourceMap {
  private def relativizePath(base: String, path: String): String = {
    import java.io.File

    def getPathSegments(path: String) =
      path.split(Pattern.quote(File.separator)).toList.filter(_.length > 0).filter(_ != ".")
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

    // On unixes all abs paths start with '/'.
    // On windows the abs paths starts with drive letter and if the drives
    // are not the same, there is no relative path. So return abspath.
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
