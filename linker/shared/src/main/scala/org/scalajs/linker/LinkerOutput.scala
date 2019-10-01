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

import scala.concurrent._

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer

import java.net.URI

import org.scalajs.linker.standard.OutputFileImpl

/** Output specification for a linker run.
 *
 *  @param jsFile The JavaScript file a [[Linker]] writes to.
 *
 *  @param sourceMap The sourceMap file the linker writes to. A [[Linker]] may
 *      ignore this file. N.b. the [[StandardLinker]] will ignore it if
 *      [[StandardConfig.sourceMap]] is false. Further, a [[Linker]] must not
 *      fail if this is not set, but rather not write a source map (even if it
 *      is configured to write a source map).
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
    val jsFile: LinkerOutput.File,
    val sourceMap: Option[LinkerOutput.File],
    val sourceMapURI: Option[URI],
    val jsFileURI: Option[URI]
) {
  private def this(jsFile: LinkerOutput.File) =
    this(jsFile, None, None, None)

  def withSourceMap(sourceMap: LinkerOutput.File): LinkerOutput =
    copy(sourceMap = Some(sourceMap))

  def withSourceMapURI(sourceMapURI: URI): LinkerOutput =
    copy(sourceMapURI = Some(sourceMapURI))

  def withJSFileURI(jsFileURI: URI): LinkerOutput =
    copy(jsFileURI = Some(jsFileURI))

  private def copy(
      jsFile: LinkerOutput.File = jsFile,
      sourceMap: Option[LinkerOutput.File] = sourceMap,
      sourceMapURI: Option[URI] = sourceMapURI,
      jsFileURI: Option[URI] = jsFileURI): LinkerOutput = {
    new LinkerOutput(jsFile, sourceMap, sourceMapURI, jsFileURI)
  }
}

object LinkerOutput extends LinkerOutputPlatformExtensions {
  def apply(jsFile: LinkerOutput.File): LinkerOutput = new LinkerOutput(jsFile)

  abstract class File private[linker] () {
    private[linker] def impl: OutputFileImpl
  }
}
