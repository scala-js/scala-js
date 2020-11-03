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

import org.scalajs.linker.interface.{LinkerOutput, OutputDirectory}

@deprecated("Part of old Linker interface", "1.3.0")
class OutputFileImpl(
    val name: String, val directory: OutputDirectory
) extends LinkerOutput.File {
  final private[interface] def impl: OutputFileImpl = this

  /** Convenience method to write this file in its output directory. */
  final def writeFull(buf: ByteBuffer)(implicit ec: ExecutionContext): Future[Unit] = {
    OutputDirectoryImpl.fromOutputDirectory(directory).writeFull(name, buf)
  }
}

@deprecated("Part of old Linker interface", "1.3.0")
object OutputFileImpl {
  def fromOutputFile(f: LinkerOutput.File): OutputFileImpl = f.impl
}
