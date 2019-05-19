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

import scala.util.control.NonFatal

import java.io._
import java.nio.file._

/** Provides best-effort atomic writing to a file
 *
 *  This FileOutputStream writes to a temporary file and moves it onto
 *  the original file, once this FileOutputStream is closed. However,
 *  it is only able to do so, if the underlying filesystem supports
 *  it. If it doesn't it falls back to non-atomic moving or copying.
 */
private[irio] class AtomicFileOutputStream private (
    private val baseFile: File,
    private val tmpFile: File
) extends FileOutputStream(tmpFile) {

  private[this] var _closed = false

  def this(baseFile: File) = {
    this(baseFile, {
      // Create a temporary file we actually write to
      val tmpFile = File.createTempFile(
          ".tmp-" + baseFile.getName, ".tmp", baseFile.getParentFile)
      tmpFile.deleteOnExit()
      tmpFile
    })
  }

  override def close(): Unit = {
    super.close()

    synchronized {
      if (!_closed) {
        _closed = true
        atomicReplace()
      }
    }
  }

  /** Try to atomically replace the baseFile with the tmpFile */
  private[this] def atomicReplace(): Unit = {
    try {
      // Try atomic move.
      Files.move(tmpFile.toPath, baseFile.toPath, StandardCopyOption.ATOMIC_MOVE)
    } catch {
      case NonFatal(_) =>
        /* We need to catch all exceptions, because it is platform dependent:
         * - whether ATOMIC_MOVE overrides an existing file or not,
         * - it throws a FileAlreadyExistsException in this case.
         *
         * If the atomic move fails, we fall back to a normal copy & delete.
         */
        try {
          Files.copy(tmpFile.toPath, baseFile.toPath,
              StandardCopyOption.REPLACE_EXISTING)
        } finally {
          tmpFile.delete()
        }
    }
  }
}
