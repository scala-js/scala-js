package org.scalajs.core.tools.io

import java.io._
import java.net.URI

/** Provides best-effort atomic writing to a file
 *
 *  This FileOutputStream writes to a temporary file and moves it onto
 *  the original file, once this FileOutputStream is closed. However,
 *  it is only able to do so, if the underlying filesystem supports
 *  it. If it doesn't it falls back to non-atomic moving or copying.
 */
private[io] class AtomicFileOutputStream private (
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
    if (!tmpFile.renameTo(baseFile)) {
      // Renaming failed. Fallback to copy
      try {
        IO.copyTo(FileVirtualBinaryFile(tmpFile),
            WritableFileVirtualBinaryFile(baseFile))
      } finally {
        tmpFile.delete()
      }
    }
  }
}
