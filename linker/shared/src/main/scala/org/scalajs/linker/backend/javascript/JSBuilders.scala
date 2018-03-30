/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.linker.backend.javascript

import org.scalajs.ir.Position

import java.io._
import java.net.URI

/** An abstract builder taking IR or JSTrees */
trait JSBuilder {
  /** Add a JavaScript tree representing a statement.
   *  The tree must be a valid JavaScript tree (typically obtained by
   *  desugaring a full-fledged IR tree).
   */
  def addJSTree(tree: Trees.Tree): Unit

  /** Add a chunk of JavaScript code. */
  def addStatement(originalLocation: URI, code: String): Unit

  /** Completes the builder. */
  def complete(): Unit
}

trait JSLineBuilder extends JSBuilder {
  def addLine(line: String): Unit
}

final class JSFileBuilder(name: String, outputWriter: Writer) extends JSLineBuilder {
  def addLine(line: String): Unit = {
    outputWriter.write(line)
    outputWriter.write('\n')
  }

  def addStatement(originalLocation: URI, code: String): Unit = {
    outputWriter.write(code)
    if (code.nonEmpty && !code.endsWith("\n"))
      outputWriter.write('\n')
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

  def complete(): Unit =
    outputWriter.close()
}

class JSFileBuilderWithSourceMap(name: String, outputWriter: Writer,
    sourceMapOutputWriter: Writer,
    relativizeSourceMapBasePath: Option[URI]) extends JSLineBuilder {
  private val sourceMapWriter = new SourceMapWriter(
      sourceMapOutputWriter, name, relativizeSourceMapBasePath)

  def addLine(line: String): Unit = {
    outputWriter.write(line)
    outputWriter.write('\n')
    sourceMapWriter.nextLine()
  }

  def addStatement(originalLocation: URI, code: String): Unit = {
    if (code.nonEmpty) {
      outputWriter.write(code)

      if (!code.endsWith("\n"))
        outputWriter.write("\n")

      for ((line, i) <- code.stripSuffix("\n").split("\n", -1).zipWithIndex) {
        val originalPos = Position(originalLocation, i, 0)
        sourceMapWriter.startNode(0, originalPos, None)
        sourceMapWriter.endNode(line.length)
        sourceMapWriter.nextLine()
      }
    }
  }

  def addJSTree(tree: Trees.Tree): Unit = {
    val printer = new Printers.JSTreePrinterWithSourceMap(
        outputWriter, sourceMapWriter)
    printer.printTopLevelTree(tree)
    // Do not close the printer: we do not have ownership of the writers
  }

  def complete(): Unit = {
    addLine("//# sourceMappingURL=" + name + ".map")
    outputWriter.close()
    sourceMapWriter.complete()
    sourceMapOutputWriter.close()
  }
}
