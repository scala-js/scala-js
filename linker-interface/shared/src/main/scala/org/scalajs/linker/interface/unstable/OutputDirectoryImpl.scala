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

package org.scalajs.linker.interface.unstable

import scala.concurrent._

import java.nio.ByteBuffer

import org.scalajs.linker.interface.OutputDirectory

abstract class OutputDirectoryImpl extends OutputDirectory {
  final private[interface] def impl: OutputDirectoryImpl = this

  /** Writes to the given file.
   *
   *  Writing should only result in a file write if the contents of the file
   *  actually changed. Further, if the underlying filesystem allows it, the
   *  file should be written atomically.
   */
  def writeFull(name: String, buf: ByteBuffer)(
      implicit ec: ExecutionContext): Future[Unit]

  /** Lists all the files in the directory. */
  def listFiles()(implicit ec: ExecutionContext): Future[List[String]]

  /** Deletes the given file. Fails if it does not exist. */
  def delete(name: String)(implicit ec: ExecutionContext): Future[Unit]
}

object OutputDirectoryImpl {
  def fromOutputDirectory(f: OutputDirectory): OutputDirectoryImpl = f.impl
}
