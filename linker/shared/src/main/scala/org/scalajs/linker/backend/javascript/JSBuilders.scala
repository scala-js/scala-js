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

import java.io._
import java.net.URI
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

/** An abstract builder taking IR or JSTrees */
trait JSBuilder {
  /** Add a JavaScript tree representing a statement.
   *  The tree must be a valid JavaScript tree (typically obtained by
   *  desugaring a full-fledged IR tree).
   */
  def addJSTree(tree: Trees.Tree): Unit
}

trait JSLineBuilder extends JSBuilder {
  def addLine(line: String): Unit
}

final class JSFileBuilder extends JSLineBuilder {
  private val out = new ByteArrayOutputStream
  private val outputWriter =
    new OutputStreamWriter(out, StandardCharsets.UTF_8)

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

  def complete(): ByteBuffer = {
    outputWriter.close()
    ByteBuffer.wrap(out.toByteArray())
  }
}

final class JSFileBuilderWithSourceMap(
    jsFileURI: Option[URI],
    sourceMapURI: Option[URI],
    relativizeSourceMapBasePath: Option[URI]
) extends JSLineBuilder {

  private def writer(out: OutputStream) =
    new OutputStreamWriter(out, StandardCharsets.UTF_8)

  private val jsOut = new ByteArrayOutputStream()
  private val smOut = new ByteArrayOutputStream()

  private val jsWriter = writer(jsOut)

  private val smWriter =
    new SourceMapWriter(writer(smOut), jsFileURI, relativizeSourceMapBasePath)

  def addLine(line: String): Unit = {
    jsWriter.write(line)
    jsWriter.write('\n')
    smWriter.nextLine()
  }

  def addJSTree(tree: Trees.Tree): Unit = {
    val printer = new Printers.JSTreePrinterWithSourceMap(jsWriter, smWriter)
    printer.printTopLevelTree(tree)
    // Do not close the printer: we do not have ownership of the writers
  }

  def complete(): (ByteBuffer, ByteBuffer) = {
    sourceMapURI.foreach(uri =>
        addLine("//# sourceMappingURL=" + uri.toASCIIString()))
    jsWriter.close()
    smWriter.complete()

    (ByteBuffer.wrap(jsOut.toByteArray()), ByteBuffer.wrap(smOut.toByteArray()))
  }
}
