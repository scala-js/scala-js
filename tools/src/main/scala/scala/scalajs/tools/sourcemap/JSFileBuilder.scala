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
import java.net.{ URI, URISyntaxException }

import com.google.debugging.sourcemap.{ FilePosition, _ }

import scala.scalajs.ir
import scala.scalajs.tools.io._

class JSFileBuilder(val name: String, protected val outputWriter: Writer) {
  def addLine(line: String): Unit = {
    outputWriter.write(line)
    outputWriter.write('\n')
  }

  def addLines(lines: Seq[String]): Unit =
    lines.foreach(addLine)

  def addFile(file: VirtualJSFile): Unit =
    addPartsOfFile(file)(!_.startsWith("//# sourceMappingURL="))

  def addPartsOfFile(file: VirtualJSFile)(selector: String => Boolean): Unit = {
    for (line <- file.readLines() if selector(line))
      addLine(line)
  }

  /** Add an IR tree representing a statement.
   *  The IR is desugared with [[scala.scalajs.ir.JSDesugaring]] before being
   *  emitted.
   */
  def addIRTree(tree: ir.Trees.Tree): Unit =
    addJSTree(ir.JSDesugaring.desugarJavaScript(tree))

  /** Add a JavaScript tree representing a statement.
   *  The tree must be a valid JavaScript tree (typically obtained by
   *  desugaring a full-fledged IR tree).
   */
  def addJSTree(tree: ir.Trees.Tree): Unit = {
    val printer = new ir.Printers.IRTreePrinter(outputWriter)
    printer.printTopLevelTree(tree)
    // Do not close the printer: we do not have ownership of the writers
  }

  /** Completes the builder.
   *  Does not close the underlying writer(s).
   */
  def complete(): Unit = {
    // Can be overridden by subclasses
  }

  /** Closes the underlying writer(s).
   */
  def closeWriters(): Unit = {
    outputWriter.close()
  }
}

class JSFileBuilderWithSourceMapWriter(n: String, ow: Writer,
    protected val sourceMapWriter: ir.SourceMapWriter)
    extends JSFileBuilder(n, ow) {

  override def addLine(line: String): Unit = {
    super.addLine(line)
    sourceMapWriter.nextLine()
  }

  private final val NotSelected = -1

  override def addPartsOfFile(file: VirtualJSFile)(
      selector: String => Boolean): Unit = {
    val br = new BufferedReader(file.reader)
    try {
      // Select lines, and remember offsets
      val offsets = new mutable.ArrayBuffer[Int] // (maybe NotSelected)
      val selectedLineLengths = new mutable.ArrayBuffer[Int]
      var line: String = br.readLine()
      var selectedCount = 0
      while (line != null) {
        if (selector(line)) {
          super.addLine(line) // super call not to advance line in source map
          offsets += selectedCount
          selectedLineLengths += line.length
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

          val entriesByLine = Array.fill(selectedCount)(
              List.newBuilder[(Int, Int, ir.Position, String)])

          consumer.visitMappings(new SourceMapConsumerV3.EntryVisitor {
            override def visit(sourceName: String, symbolName: String,
                sourceStartPos: FilePosition,
                startPos: FilePosition, endPos: FilePosition): Unit = {

              for (line <- startPos.getLine to endPos.getLine) {
                if (line < 0 || line >= offsets.size)
                  return
                val offset = offsets(line)
                if (offset == NotSelected)
                  return

                val startColumn =
                  if (line == startPos.getLine) startPos.getColumn
                  else 0
                val endColumn0 =
                  if (line == endPos.getLine) endPos.getColumn
                  else selectedLineLengths(offset)
                val endColumn = Math.max(endColumn0, startColumn)
                val sourcePos = ir.Position(new URI(sourceName),
                    sourceStartPos.getLine, sourceStartPos.getColumn)

                entriesByLine(offset) +=
                  ((startColumn, endColumn, sourcePos, symbolName))
              }
            }
          })

          for (lineNumber <- 0 until selectedCount) {
            val entries = entriesByLine(lineNumber).result().sortBy(_._1)
            var lastEndColumn = 0
            for ((startColumn, endColumn, sourcePos, symbolName) <- entries) {
              val startColumn1 = Math.max(startColumn, lastEndColumn)
              sourceMapWriter.startNode(startColumn, sourcePos,
                  Option(symbolName))
              sourceMapWriter.endNode(endColumn)
              lastEndColumn = endColumn
            }
            sourceMapWriter.nextLine()
          }

        case None =>
          /* The source map does not exist.
           * This happens typically for corejslib.js and other helper files
           * written directly in JS.
           * We generate a fake line-by-line source map for these on the fly
           */
          val sourceFile = getFileSourceFile(file)

          for (lineNumber <- 0 until offsets.size) {
            val offset = offsets(lineNumber)
            if (offset != NotSelected) {
              val originalPos = ir.Position(sourceFile, lineNumber, 0)
              sourceMapWriter.startNode(0, originalPos, None)
              sourceMapWriter.endNode(selectedLineLengths(offset))
              sourceMapWriter.nextLine()
            }
          }
      }
    } finally {
      br.close()
    }
  }

  override def addJSTree(tree: ir.Trees.Tree): Unit = {
    val printer = new ir.Printers.IRTreePrinterWithSourceMap(
        outputWriter, sourceMapWriter)
    printer.printTopLevelTree(tree)
    // Do not close the printer: we do not have ownership of the writers
  }

  override def complete(): Unit = {
    super.complete()
    sourceMapWriter.complete()
  }

  private def getFileSourceFile(
      file: VirtualJSFile): ir.Position.SourceFile = file match {
    case file: FileVirtualJSFile =>
      file.file.toURI
    case _ =>
      try new URI(file.path)
      catch {
        case e: URISyntaxException =>
          new URI(
               "virtualfile", // Pseudo-Scheme
               file.path,     // Scheme specific part
               null           // Fragment
          )
      }
  }
}

class JSFileBuilderWithSourceMap(n: String, ow: Writer,
    sourceMapOutputWriter: Writer,
    relativizeSourceMapBasePath: Option[URI] = None)
    extends JSFileBuilderWithSourceMapWriter(
        n, ow,
        new ir.SourceMapWriter(sourceMapOutputWriter, n,
            relativizeSourceMapBasePath)) {

  override def complete(): Unit = {
    addLine("//# sourceMappingURL=" + name + ".map")
    super.complete()
  }

  override def closeWriters(): Unit = {
    super.closeWriters()
    sourceMapOutputWriter.close()
  }
}
