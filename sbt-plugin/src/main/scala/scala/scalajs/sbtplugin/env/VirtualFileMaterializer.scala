package scala.scalajs.sbtplugin.env

import scala.scalajs.tools.io.{IO => _, _}

import sbt.IO

import java.io.File

/** A helper class to temporarily store virtual files to the filesystem.
 *  
 *  Can be used with tools that require real files.
 *  @param singleDir if true, forces files to be copied into
 *      [[cacheDir]]. Useful to setup include directories for
 *      example.
 */
final class VirtualFileMaterializer(singleDir: Boolean = false) {

  val cacheDir = {
    val dir = IO.createTemporaryDirectory
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

  private def materializeFileVF(vf: FileVirtualFile): File = {
    if (!singleDir) vf.file
    else {
      val trg = trgFile(vf.name)
      IO.copyFile(vf.file, trg)
      trg
    }
  }

  def materialize(vf: VirtualTextFile): File = vf match {
    case vf: FileVirtualFile => materializeFileVF(vf)
    case _ =>
      val trg = trgFile(vf.name)
      IO.write(trg, vf.content)
      trg
  }

  def materialize(vf: VirtualBinaryFile): File = vf match {
    case vf: FileVirtualFile => materializeFileVF(vf)
    case _ =>
      val trg = trgFile(vf.name)
      IO.write(trg, vf.content)
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

}
