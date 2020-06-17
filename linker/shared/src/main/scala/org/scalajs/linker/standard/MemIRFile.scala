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

package org.scalajs.linker.standard

import scala.concurrent._

import java.nio.ByteBuffer

import org.scalajs.ir

import org.scalajs.linker.interface.IRFile
import org.scalajs.linker.interface.unstable.IRFileImpl

import ConcreteIRFileImpl._

/** A simple in-memory virtual serialized Scala.js IR file. */
final class MemIRFileImpl(
    path: String,
    version: Option[String],
    content: Array[Byte]
) extends IRFileImpl(path, version) {
  def entryPointsInfo(implicit ec: ExecutionContext): Future[IRFileImpl.EntryPointsInfo] =
    withBuffer(buf => toIRFileImplEntryPointsInfo(ir.Serializers.deserializeEntryPointsInfo(buf)))

  def tree(implicit ec: ExecutionContext): Future[IRFileImpl.ClassDef] =
    withBuffer(buf => toIRFileImplClassDef(ir.Serializers.deserialize(buf)))

  @inline
  private def withBuffer[A](f: ByteBuffer => A)(
      implicit ec: ExecutionContext): Future[A] = {
    val result = Future(f(ByteBuffer.wrap(content)))
    withPathExceptionContext(path, result)
  }
}
