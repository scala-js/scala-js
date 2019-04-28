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

import scala.concurrent._

import java.nio.ByteBuffer

import org.scalajs.ir

/** A simple in-memory virtual serialized Scala.js IR file. */
final class MemVirtualSerializedScalaJSIRFile(
    val path: String,
    val relativePath: String,
    val version: Option[String],
    content: Array[Byte]
) extends VirtualScalaJSIRFile {
  def entryPointsInfo(implicit ec: ExecutionContext): Future[ir.EntryPointsInfo] =
    withBuffer(ir.Serializers.deserializeEntryPointsInfo)

  def tree(implicit ec: ExecutionContext): Future[ir.Trees.ClassDef] =
    withBuffer(ir.Serializers.deserialize)

  @inline
  private def withBuffer[A](f: ByteBuffer => A)(
      implicit ec: ExecutionContext): Future[A] = {
    val result = Future(f(ByteBuffer.wrap(content)))
    VirtualScalaJSIRFile.withPathExceptionContext(path, result)
  }
}
