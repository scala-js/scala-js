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

package org.scalajs.linker

import org.scalajs.linker.irio.WritableVirtualBinaryFile

import java.net.URI

/** Output specification for a linker run.
 *
 *  @param jsFile The JavaScript file a [[Linker]] writes to.
 *
 *  @param sourceMap The sourceMap file the linker writes to. A [[Linker]] may
 *      ignore this file. N.b. the [[StandardLinker]] will ignore it if
 *      [[StandardLinker.Config.sourceMap]] is false. Further, a [[Linker]] must
 *      not fail if this is not set, but rather not write a source map (even if
 *      it is configured to write a source map).
 *
 *  @param sourceMapURI URI to reach the source map from the JavaScript file.
 *      This is typically a relative URI but is not required. A [[Linker]]
 *      should ignore this, if [[sourceMap]] is not set or source map production
 *      is disabled.
 *
 *  @param jsFileURI URI to reach the JavaScript file from the source map. This
 *      is typically a relative URI but is not required. A [[Linker]] may use
 *      this even if [[sourceMap]] is not set, but it is typically meaningless.
 */
final class LinkerOutput private (
    val jsFile: WritableVirtualBinaryFile,
    val sourceMap: Option[WritableVirtualBinaryFile],
    val sourceMapURI: Option[URI],
    val jsFileURI: Option[URI]
) {
  private def this(jsFile: WritableVirtualBinaryFile) =
    this(jsFile, None, None, None)

  def withSourceMap(sourceMap: WritableVirtualBinaryFile): LinkerOutput =
    copy(sourceMap = Some(sourceMap))

  def withSourceMapURI(sourceMapURI: URI): LinkerOutput =
    copy(sourceMapURI = Some(sourceMapURI))

  def withJSFileURI(jsFileURI: URI): LinkerOutput =
    copy(jsFileURI = Some(jsFileURI))

  private def copy(jsFile: WritableVirtualBinaryFile = jsFile,
      sourceMap: Option[WritableVirtualBinaryFile] = sourceMap,
      sourceMapURI: Option[URI] = sourceMapURI,
      jsFileURI: Option[URI] = jsFileURI): LinkerOutput = {
    new LinkerOutput(jsFile, sourceMap, sourceMapURI, jsFileURI)
  }
}

object LinkerOutput {
  def apply(jsFile: WritableVirtualBinaryFile): LinkerOutput = new LinkerOutput(jsFile)
}
