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
   *
   *  Calling this method is equivalent to calling
   *  `writeFull(name, buf, skipContentCheck = false)`.
   */
  def writeFull(name: String, buf: ByteBuffer)(
      implicit ec: ExecutionContext): Future[Unit]

  /** Writes to the given file.
   *
   *  - If `skipContentCheck` is `false`, writing should only result in a file
   *    write if the contents of the file actually changed.
   *  - If it is `true`, the implementation is encouraged not to check for the
   *    file contents, and always write it; however, this not mandatory for
   *    backward compatibility reasons.
   *
   *  If the underlying filesystem allows it, the file should be written
   *  atomically.
   *
   *  The default implementation of this method calls `writeFull` without the
   *  `skipContentCheck`, which is suboptimal. Therefore, it is encouraged to
   *  override it.
   */
  def writeFull(name: String, buf: ByteBuffer, skipContentCheck: Boolean)(
      implicit ec: ExecutionContext): Future[Unit] = {
    writeFull(name, buf)
  }

  /** Fully read the given file into a new ByteBuffer. */
  def readFull(name: String)(
      implicit ec: ExecutionContext): Future[ByteBuffer]

  /** Lists all the files in the directory. */
  def listFiles()(implicit ec: ExecutionContext): Future[List[String]]

  /** Deletes the given file. Fails if it does not exist. */
  def delete(name: String)(implicit ec: ExecutionContext): Future[Unit]
}

object OutputDirectoryImpl {
  def fromOutputDirectory(f: OutputDirectory): OutputDirectoryImpl = f.impl
}
