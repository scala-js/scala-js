package scala.scalajs.sbtplugin.env

import scala.scalajs.tools.io._
import sbt.IO
import scala.collection.mutable
import java.io.File

/** Helper class to write files that should be available on the classpath to a
 *  temporary directory. Also performs caching of this directory.
 *
 *  This class is NOT thread safe
 */
final class AvailableFilesCache {

  // Map from extracted cached files to version
  var cached = mutable.Map.empty[String, Option[Any]]

  var curTmpDir = newTmpDir()

  def cacheFiles(vfiles: Seq[VirtualJSFile]): File = {
    if (!curTmpDir.exists) {
      // Someone removed our tmp dir. Invalidate cache, create new one
      cached.clear()
      curTmpDir = newTmpDir()
    }

    // Check if we need to remove any files
    if (!cached.isEmpty) {
      val deleted = cached.keySet -- vfiles.map(_.name).toSet
      deleted foreach { deleteFile _ }
    }

    // Add new files
    for (vfile <- vfiles if !isUpToDate(vfile)) {
      copyFile(vfile)
      cached += vfile.name -> vfile.version
    }

    curTmpDir
  }

  private def isUpToDate(vfile: VirtualJSFile) = {
    val oldVersion = cached.getOrElse(vfile.name, None)
    oldVersion.isDefined && oldVersion == vfile.version
  }

  private def deleteFile(fname: String) = cacheFile(fname).delete()
  private def copyFile(vfile: VirtualJSFile) = vfile match {
    case vfile: FileVirtualJSFile =>
      IO.copyFile(vfile.file, cacheFile(vfile), preserveLastModified = true)
    case _ =>
      IO.write(cacheFile(vfile), vfile.content, append = false)
  }

  private def cacheFile(vfile: VirtualJSFile): File = cacheFile(vfile.name)
  private def cacheFile(fname: String): File = new File(curTmpDir, fname)

  /** create a new temporary directory that is deleted on exit */
  private def newTmpDir() = {
    val dir = IO.createTemporaryDirectory
    dir.deleteOnExit()
    dir
  }

}
