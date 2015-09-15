/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.optimizer

import scala.collection.mutable

import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.io._

import org.scalajs.core.ir
import ir.Infos

final class IRFileCache {

  private[this] val irCache = mutable.Map.empty[String, PersistentIRFile]

  private[this] var readyToUse: Boolean = false

  private[this] var statsReused: Int = 0
  private[this] var statsInvalidated: Int = 0
  private[this] var statsTreesRead: Int = 0

  /** Updates this cache with new IR files.
   *
   *  Successive calls of this method must be interlieved with calls to
   *  [[cleanAfterUse]].
   *  @returns true if a file or the composition of files changed
   */
  def update(irFiles: Traversable[VirtualScalaJSIRFile]): Boolean = {
    requireReadyState(false, "Call cleanAfterUse before calling update again")
    readyToUse = true

    // Are we sure we'll change something based on file count?
    val baseUpdated = irFiles.size != irCache.size

    // Update every file
    val updated = irFiles.foldLeft(baseUpdated) { (updated, irFile) =>
      val file = irCache.getOrElseUpdate(irFile.path,
          new PersistentIRFile(irFile.path))
      val thisUpdated = file.updateFile(irFile)
      updated || thisUpdated
    }

    // Remove files that don't exist anymore
    irCache.retain((_, f) => f.exists)

    updated
  }

  /** Cleans this cache after usage.
   *
   *  You must call this method before calling [[update]] again.
   *  @returns Caching statistics about this use
   */
  def cleanAfterUse(): IRFileCache.Stats = {
    requireReadyState(true, "Cache already clean or uninitialized. Call update")
    readyToUse = false

    // Release resources in IR files
    irCache.values.foreach(_.cleanAfterUse())

    val resultStats =
      new IRFileCache.Stats(statsReused, statsInvalidated, statsTreesRead)

    // Reset stats
    statsReused = 0
    statsInvalidated = 0
    statsTreesRead = 0

    resultStats
  }

  /** Returns the current set of files for use
   *
   *  Can only be invoked after an invocation of [[update]] and before
   *  [[cleanAfterUse]] (but an arbitrary number of times).
   */
  def files: Traversable[VirtualScalaJSIRFile] = {
    requireReadyState(true, "Cache not ready. Call update")
    irCache.values
  }

  private def requireReadyState(state: Boolean, msg: => String) = {
    if (state != readyToUse)
      throw new IllegalStateException(msg)
  }

  private final class PersistentIRFile(
      val path: String) extends VirtualScalaJSIRFile {
    import ir.Trees._

    private[this] var _exists: Boolean = false

    private[this] var _irFile: VirtualScalaJSIRFile = null
    private[this] var _version: Option[String] = None
    private[this] var _info: Infos.ClassInfo = null
    private[this] var _tree: ClassDef = null

    def updateFile(irFile: VirtualScalaJSIRFile): Boolean = {
      assert(irFile.path == path, s"Path mismatch: $path, ${irFile.path}")

      _exists = true
      _irFile = irFile
      if (_version.isDefined && _version == _irFile.version) {
        // yeepeeh, nothing to do
        statsReused += 1
        false
      } else {
        _version = irFile.version
        _info = irFile.info
        _tree = null
        statsInvalidated += 1
        true
      }
    }

    override def version: Option[String] = _version

    override def info: Infos.ClassInfo = _info

    override def tree: ClassDef = {
      if (_tree == null) {
        statsTreesRead += 1
        _tree = _irFile.tree
      }
      _tree
    }

    def infoAndTree: (Infos.ClassInfo, ClassDef) = (info, tree)

    def exists: Boolean = _exists

    /** Releases unneeded resources of this PersistentIRFile.
     *
     *  Also marks this file to be non-existent.
     */
    def cleanAfterUse(): Unit = {
      _irFile = null
      _exists = false
    }
  }

}

object IRFileCache {
  final class Stats(val reused: Int, val invalidated: Int, val treesRead: Int) {
    /** Descriptive line to display in logs */
    def logLine: String = {
      s"IR Cache stats: reused: $reused -- " +
      s"invalidated: $invalidated -- " +
      s"trees read: $treesRead"
    }
  }
}
