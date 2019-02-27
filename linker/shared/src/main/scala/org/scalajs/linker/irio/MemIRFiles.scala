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

package org.scalajs.linker.irio

import java.io._

import org.scalajs.ir

/** A simple in-memory virtual serialized Scala.js IR file. */
final class MemVirtualSerializedScalaJSIRFile(
    val path: String,
    val relativePath: String,
    val version: Option[String],
    content: Array[Byte]
) extends VirtualScalaJSIRFile {
  def entryPointsInfo: ir.EntryPointsInfo =
    withInputStream(ir.Serializers.deserializeEntryPointsInfo)

  def tree: ir.Trees.ClassDef =
    withInputStream(ir.Serializers.deserialize)

  @inline
  private def withInputStream[A](f: InputStream => A): A = {
    val stream = new ByteArrayInputStream(content)
    try VirtualScalaJSIRFile.withPathExceptionContext(path)(f(stream))
    finally stream.close()
  }
}
