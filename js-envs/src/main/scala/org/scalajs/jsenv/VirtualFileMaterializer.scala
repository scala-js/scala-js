package org.scalajs.jsenv

import scala.annotation.tailrec

import org.scalajs.core.tools.io._

import java.io.File

/** A helper class to temporarily store virtual files to the filesystem.
 *
 *  Can be used with tools that require real files.
 *  @param singleDir if true, forces files to be copied into
 *      [[cacheDir]]. Useful to setup include directories for
 *      example.
 */
final class VirtualFileMaterializer(singleDir: Boolean = false) {
  import VirtualFileMaterializer._

  val cacheDir = {
    val dir = createTempDir()
    dir.deleteOnExit()
    dir
  }

  /** Create a target file to write/copy to. Will also call
   *  deleteOnExit on the file.
   */
  private def trgFile(name: String): File = {
    val f = new File(cacheDir, name)
    f.deleteOnExit()
    f
  }

  def materialize(vf: VirtualTextFile): File = vf match {
    case vf: FileVirtualFile if !singleDir =>
      vf.file
    case _ =>
      val trg = trgFile(vf.name)
      IO.copyTo(vf, WritableFileVirtualTextFile(trg))
      trg
  }

  def materialize(vf: VirtualBinaryFile): File = vf match {
    case vf: FileVirtualFile if !singleDir =>
      vf.file
    case _ =>
      val trg = trgFile(vf.name)
      IO.copyTo(vf, WritableFileVirtualBinaryFile(trg))
      trg
  }

  /** Removes the cache directory. Any operation on this
   *  VirtualFileMaterializer is invalid after [[close]] has been
   *  called.
   */
  def close(): Unit = {
    cacheDir.listFiles().foreach(_.delete)
    cacheDir.delete()
  }

  // scalastyle:off line.size.limit
  /* Taken from Guava:
   * https://github.com/google/guava/blob/1c285fc8d289c43b46aa55e7f90ec0359be5b69a/guava/src/com/google/common/io/Files.java#L413-L426
   */
  // scalastyle:on line.size.limit
  private def createTempDir(): File = {
    val baseDir = new File(System.getProperty("java.io.tmpdir"))
    val baseName = System.currentTimeMillis() + "-"

    @tailrec
    def loop(tries: Int): File = {
      val tempDir = new File(baseDir, baseName + tries)
      if (tempDir.mkdir())
        tempDir
      else if (tries < TempDirAttempts)
        loop(tries + 1)
      else {
        throw new IllegalStateException("Failed to create directory within " +
            s"$TempDirAttempts attempts (tried ${baseName}0 to " +
            s"${baseName}${TempDirAttempts - 1})")
      }
    }

    loop(0)
  }

}

private object VirtualFileMaterializer {
  private final val TempDirAttempts = 10000
}
