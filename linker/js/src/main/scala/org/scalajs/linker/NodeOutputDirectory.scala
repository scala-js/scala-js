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

import scala.scalajs.js
import scala.scalajs.js.typedarray.TypedArrayBufferOps._

import java.nio.ByteBuffer

import org.scalajs.linker.interface.OutputDirectory
import org.scalajs.linker.interface.unstable.OutputDirectoryImpl

object NodeOutputDirectory {
  import NodeFS.cbFuture

  def apply(directory: String): OutputDirectory = new Impl(directory)

  private final class Impl(directory: String) extends OutputDirectoryImpl {
    def writeFull(name: String, buf: ByteBuffer)(implicit ec: ExecutionContext): Future[Unit] = {
      val path = getPath(name)

      val data =
        if (buf.hasTypedArray()) buf.typedArray().subarray(buf.position(), buf.limit())
        else ByteBuffer.allocateDirect(buf.remaining()).put(buf).typedArray()

      cbFuture[Unit](NodeFS.writeFile(path, data, _))
        .map(_ => buf.position(buf.limit()))
    }

    def listFiles()(implicit ec: ExecutionContext): Future[Iterable[String]] =
      cbFuture[js.Array[String]](NodeFS.readdir(directory, _)).map(x => x)

    def delete(name: String)(implicit ec: ExecutionContext): Future[Unit] =
      cbFuture[Unit](NodeFS.unlink(getPath(name), _))

    private def getPath(name: String) = NodeFS.join(directory, name)
  }
}

