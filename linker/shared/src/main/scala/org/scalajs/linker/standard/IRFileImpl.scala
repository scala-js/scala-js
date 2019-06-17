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

import java.io.IOException
import java.nio.ByteBuffer

import org.scalajs.ir

import org.scalajs.linker.IRFile

/** A virtual Scala.js IR file.
 *  It contains the class info and the IR tree.
 */
abstract class IRFileImpl(
  /** Abstract path of the file.
   *
   *  The path of the file is used for lookup and caching (together with the
   *  version).
   */
  val path: String,

  /** An optional implementation-dependent "version" token.
   *
   *  If non-empty, a different version must be returned when the content
   *  changes. It should be equal if the content has not changed, but it is
   *  not mandatory.
   *  Such a token can be used by caches: the file need not be read and
   *  processed again if its version has not changed.
   */
  val version: Option[String]
) extends IRFile {
  private[linker] final def impl: IRFileImpl = this

  /** Entry points information for this file. */
  def entryPointsInfo(implicit ec: ExecutionContext): Future[ir.EntryPointsInfo]

  /** IR Tree of this file. */
  def tree(implicit ec: ExecutionContext): Future[ir.Trees.ClassDef]
}

object IRFileImpl {
  def fromIRFile(irFile: IRFile): IRFileImpl = irFile.impl

  def fromMem(path: String, version: Option[String], content: Array[Byte]): IRFileImpl =
    new MemIRFileImpl(path, version, content)

  def withPathExceptionContext[A](path: String, future: Future[A])(
      implicit ec: ExecutionContext): Future[A] = {
    future.recover {
      case e: ir.IRVersionNotSupportedException =>
        throw new ir.IRVersionNotSupportedException(e.version, e.supported,
            s"Failed to deserialize a file compiled with Scala.js ${e.version}" +
            s" (supported: ${e.supported.mkString(", ")}): $path", e)

      case e: IOException =>
        throw new IOException(s"Failed to deserialize $path", e)
    }
  }

  /** A simple in-memory virtual serialized Scala.js IR file. */
  private final class MemIRFileImpl(
      path: String,
      version: Option[String],
      content: Array[Byte]
  ) extends IRFileImpl(path, version) {
    def entryPointsInfo(implicit ec: ExecutionContext): Future[ir.EntryPointsInfo] =
      withBuffer(ir.Serializers.deserializeEntryPointsInfo)

    def tree(implicit ec: ExecutionContext): Future[ir.Trees.ClassDef] =
      withBuffer(ir.Serializers.deserialize)

    @inline
    private def withBuffer[A](f: ByteBuffer => A)(
        implicit ec: ExecutionContext): Future[A] = {
      val result = Future(f(ByteBuffer.wrap(content)))
      IRFileImpl.withPathExceptionContext(path, result)
    }
  }
}
