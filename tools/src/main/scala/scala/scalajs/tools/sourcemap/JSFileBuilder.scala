/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.sourcemap

import scala.annotation.tailrec

import scala.collection.mutable

import java.io._
import java.util.regex.Pattern
import java.net.URI

import com.google.debugging.sourcemap.{ FilePosition, _ }

import scala.scalajs.tools.io._

class JSFileBuilder(val name: String, protected val outputWriter: Writer) {
  def addLine(line: String): Unit = {
    outputWriter.write(line)
    outputWriter.write('\n')
  }

  def addLines(lines: Seq[String]): Unit =
    lines.foreach(addLine)

  def addFile(file: VirtualJSFile): Unit =
    addPartsOfFile(file)(!_.startsWith("//@ sourceMappingURL="))

  def addPartsOfFile(file: VirtualJSFile)(selector: String => Boolean): Unit = {
    for (line <- file.readLines() if selector(line))
      addLine(line)
  }

  def complete(): Unit = {
    // Can be overridden by subclasses
  }
}

class JSFileBuilderWithSourceMap(n: String, ow: Writer,
    protected val sourceMapWriter: Writer,
    relativizeSourceMapBasePath: Option[URI] = None)
    extends JSFileBuilder(n, ow) {

  protected val sourceMapGen: SourceMapGenerator =
    SourceMapGeneratorFactory.getInstance(SourceMapFormat.V3)

  protected var totalLineCount = 0

  protected def relPath(path: URI): URI = {
    relativizeSourceMapBasePath match {
      case Some(base) => Utils.relativize(base, path)
      case None => path
    }
  }

  override def addLine(line: String): Unit = {
    super.addLine(line)
    totalLineCount += 1
  }

  private final val NotSelected = -1

  override def addPartsOfFile(file: VirtualJSFile)(
      selector: String => Boolean): Unit = {
    val br = new BufferedReader(file.reader)
    try {
      // Store starting offset
      val startLine = totalLineCount

      // Select lines, and remember offsets
      val offsets = new mutable.ArrayBuffer[Int] // (maybe NotSelected)
      var line: String = br.readLine()
      var selectedCount = 0
      while (line != null) {
        if (selector(line)) {
          addLine(line)
          offsets += selectedCount
          selectedCount += 1
        } else {
          offsets += NotSelected
        }
        line = br.readLine()
      }

      // Add the relevant parts of the source map
      file.sourceMap match {
        case Some(sourceMap) =>
          /* The source map exists.
           * Visit all the mappings in this source map, and add them to the
           * concatenated source map with the appropriate offset.
           * Ignore entries in lines that were not selected
           */
          val consumer = new SourceMapConsumerV3
          consumer.parse(sourceMap)

          consumer.visitMappings(new SourceMapConsumerV3.EntryVisitor {
            override def visit(sourceName: String, symbolName: String,
                sourceStartPos: FilePosition,
                startPos: FilePosition, endPos: FilePosition): Unit = {

              val line = startPos.getLine
              if (line < 0 || line >= offsets.size)
                return
              val offset = offsets(line)
              if (offset == NotSelected)
                return

              val lineSpan = endPos.getLine - startPos.getLine

              val offsetStartPos =
                new FilePosition(startLine+offset, startPos.getColumn)
              val offsetEndPos =
                new FilePosition(startLine+offset+lineSpan, endPos.getColumn)
              val relSourceName = relPath(new URI(sourceName))

              sourceMapGen.addMapping(relSourceName.toASCIIString(), symbolName,
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

          for (lineNumber <- 0 until offsets.size) {
            val offset = offsets(lineNumber)
            if (offset != NotSelected) {
              val sourceStartPos = new FilePosition(lineNumber, 0)
              val startPos = new FilePosition(startLine+offset, 0)
              val endPos = new FilePosition(startLine+offset+1, 0)

              sourceMapGen.addMapping(sourceName, null,
                  sourceStartPos, startPos, endPos)
            }
          }
      }
    } finally {
      br.close()
    }
  }

  override def complete(): Unit = {
    addLine("//@ sourceMappingURL=" + name + ".map")
    super.complete()

    sourceMapGen.appendTo(sourceMapWriter, name)
  }
}
