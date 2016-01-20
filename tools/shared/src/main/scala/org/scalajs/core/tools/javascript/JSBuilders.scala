/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.javascript

import scala.annotation.tailrec

import scala.collection.mutable

import java.io._
import java.util.regex.Pattern
import java.net.{ URI, URISyntaxException }

import org.scalajs.core.ir.Position
import org.scalajs.core.tools.io._

/** An abstract builder taking IR or JSTrees */
trait JSTreeBuilder {
  /** Add a JavaScript tree representing a statement.
   *  The tree must be a valid JavaScript tree (typically obtained by
   *  desugaring a full-fledged IR tree).
   */
  def addJSTree(tree: Trees.Tree): Unit

  /** Completes the builder. */
  def complete(): Unit = ()
}

class JSFileBuilder(val name: String,
    protected val outputWriter: Writer) extends JSTreeBuilder {
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

  /** Add a JavaScript tree representing a statement.
   *  The tree must be a valid JavaScript tree (typically obtained by
   *  desugaring a full-fledged IR tree).
   */
  def addJSTree(tree: Trees.Tree): Unit = {
    val printer = new Printers.JSTreePrinter(outputWriter)
    printer.printTopLevelTree(tree)
    // Do not close the printer: we do not have ownership of the writers
  }

  /** Closes the underlying writer(s).
   */
  def closeWriters(): Unit = {
    outputWriter.close()
  }
}

class JSFileBuilderWithSourceMapWriter(n: String, ow: Writer,
    protected val sourceMapWriter: SourceMapWriter)
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

      /* We ignore a potential source map.
       * This happens typically for corejslib.js and other helper files
       * written directly in JS.
       * We generate a fake line-by-line source map for these on the fly
       */
      val sourceFile = file.toURI

      for (lineNumber <- 0 until offsets.size) {
        val offset = offsets(lineNumber)
        if (offset != NotSelected) {
          val originalPos = Position(sourceFile, lineNumber, 0)
          sourceMapWriter.startNode(0, originalPos, None)
          sourceMapWriter.endNode(selectedLineLengths(offset))
          sourceMapWriter.nextLine()
        }
      }
    } finally {
      br.close()
    }
  }

  override def addJSTree(tree: Trees.Tree): Unit = {
    val printer = new Printers.JSTreePrinterWithSourceMap(
        outputWriter, sourceMapWriter)
    printer.printTopLevelTree(tree)
    // Do not close the printer: we do not have ownership of the writers
  }

  override def complete(): Unit = {
    super.complete()
    sourceMapWriter.complete()
  }

}

class JSFileBuilderWithSourceMap(n: String, ow: Writer,
    sourceMapOutputWriter: Writer,
    relativizeSourceMapBasePath: Option[URI] = None)
    extends JSFileBuilderWithSourceMapWriter(
        n, ow,
        new SourceMapWriter(sourceMapOutputWriter, n,
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
