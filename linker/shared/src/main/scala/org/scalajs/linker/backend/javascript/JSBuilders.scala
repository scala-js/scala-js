/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.linker.backend.javascript

import org.scalajs.ir.Position

import org.scalajs.linker.LinkerOutput

import java.io._
import java.net.URI
import java.nio.charset.StandardCharsets

/** An abstract builder taking IR or JSTrees */
trait JSBuilder {
  /** Add a JavaScript tree representing a statement.
   *  The tree must be a valid JavaScript tree (typically obtained by
   *  desugaring a full-fledged IR tree).
   */
  def addJSTree(tree: Trees.Tree): Unit

  /** Completes the builder. */
  def complete(): Unit
}

trait JSLineBuilder extends JSBuilder {
  def addLine(line: String): Unit
}

final class JSFileBuilder(output: LinkerOutput) extends JSLineBuilder {
  private val outputWriter =
    new OutputStreamWriter(output.jsFile.outputStream, StandardCharsets.UTF_8)

  def addLine(line: String): Unit = {
    outputWriter.write(line)
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

  def complete(): Unit = outputWriter.close()
}

class JSFileBuilderWithSourceMap(output: LinkerOutput,
    relativizeSourceMapBasePath: Option[URI]) extends JSLineBuilder {
  require(output.sourceMap.isDefined)

  private def writer(out: OutputStream) =
    new OutputStreamWriter(out, StandardCharsets.UTF_8)

  private val outputWriter = writer(output.jsFile.outputStream)

  private val sourceMapWriter = new SourceMapWriter(
      writer(output.sourceMap.get.outputStream), output.jsFileURI,
      relativizeSourceMapBasePath)

  def addLine(line: String): Unit = {
    outputWriter.write(line)
    outputWriter.write('\n')
    sourceMapWriter.nextLine()
  }

  def addJSTree(tree: Trees.Tree): Unit = {
    val printer = new Printers.JSTreePrinterWithSourceMap(
        outputWriter, sourceMapWriter)
    printer.printTopLevelTree(tree)
    // Do not close the printer: we do not have ownership of the writers
  }

  def complete(): Unit = {
    output.sourceMapURI.foreach(uri =>
        addLine("//# sourceMappingURL=" + uri.toASCIIString()))
    outputWriter.close()
    sourceMapWriter.complete()
  }
}
