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

package org.scalajs.linker.standard

import scala.annotation.tailrec
import scala.concurrent._
import scala.util.Failure

import java.net.URI

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

import org.scalajs.ir.EntryPointsInfo
import org.scalajs.ir.Trees.ClassDef

import org.scalajs.linker._

final class StandardIRFileCache extends IRFileCache {
  /* General implementation comment: We always synchronize before doing I/O
   * (instead of using a calculate and CAS pattern). This is since we assume
   * that paying the cost for synchronization is lower than I/O.
   */

  import StandardIRFileCache.Stats

  /** Holds the cached IR */
  private[this] val globalCache = new ConcurrentHashMap[String, PersistedFiles]

  // Statistics
  private[this] val statsReused = new AtomicInteger(0)
  private[this] val statsInvalidated = new AtomicInteger(0)
  private[this] val statsTreesRead = new AtomicInteger(0)

  def newCache: IRFileCache.Cache = new CacheImpl

  def stats: Stats =
    new Stats(statsReused.get, statsInvalidated.get, statsTreesRead.get)

  def clearStats(): Unit = {
    statsReused.set(0)
    statsInvalidated.set(0)
    statsTreesRead.set(0)
  }

  private final class CacheImpl extends IRFileCache.Cache {
    private[this] var localCache: Seq[PersistedFiles] = _

    def cached(files: Seq[IRContainer])(
        implicit ec: ExecutionContext): Future[Seq[IRFile]] = {
      update(files)
      Future.traverse(localCache)(_.files).map(_.flatten)
    }

    private def update(files: Seq[IRContainer])(
        implicit ec: ExecutionContext): Unit = clearOnThrow {
      val result = Seq.newBuilder[PersistedFiles]

      for (stableFile <- files) {
        val file = IRContainerImpl.fromIRContainer(stableFile)

        @tailrec
        def putContents(): PersistedFiles = {
          val newValue = new PersistedFiles(file.path)
          val oldValue = globalCache.putIfAbsent(file.path, newValue)

          val contents = if (oldValue != null) oldValue else newValue

          if (contents.reference()) contents
          else putContents()
        }

        val contents = putContents()
        contents.update(file)
        result += contents
      }

      free()
      localCache = result.result()
    }

    def free(): Unit = {
      if (localCache != null) {
        localCache.foreach(_.unreference())
        localCache = null
      }
    }

    protected override def finalize(): Unit = {
      free()
    }
  }

  /** Stores the extracted [[IRFile]]s from the file at path.
   *
   *  This also tracks references to itself by reference counting.
   *  Further, a [[PersistedFiles]] has a tombstone state. It is necessary to
   *  avoid a race between referencing a file just retrieved from
   *  [[globalCache]] and removing a file from [[globalCache]] that has just
   *  been unreferenced.
   */
  private final class PersistedFiles(path: String) {

    /** Number of references we have. -1 means we are a tombstone */
    private[this] val _references = new AtomicInteger(0)

    /** Last version we have been updated with.
     *  May only be written under synchronization, except if this is a tombstone
     */
    @volatile
    private[this] var _version: Option[String] = None

    /** Files in this [[PersistedFiles]] being calculated.
     *  May only be written under synchronization, except if this is a tombstone
     */
    @volatile
    private[this] var _files: Future[Seq[IRFile]] = null

    def files: Future[Seq[IRFile]] = _files

    /** Try to reference this block of files.
     *  @return true if referencing succeeded, false if this is a tombstone
     */
    @tailrec
    final def reference(): Boolean = {
      val refs = _references.get

      if (refs == -1) {
        // we are a tombstone, help cleaning up and bail out
        cleanup()
        false
      } else {
        // try to increase ref count
        if (_references.compareAndSet(refs, refs + 1)) true // done
        else reference() // something changed, try again
      }
    }

    /** Unreference this file.
     *
     *  If there are no references any more, turn this [[PersistedFiles]] into
     *  a tombstone and remove it from the cache.
     */
    final def unreference(): Unit = {
      val refs = _references.decrementAndGet()
      assert(refs >= 0, "Unreferencing an not referenced file")

      /* If we have 0 references, try to become a tombstone. We could be
       * referenced again in a race. In this case, don't do anything.
       */
      if (refs == 0 && _references.compareAndSet(0, -1)) {
        cleanup()
      }
    }

    /** Clean up, after becoming a tombstone */
    private def cleanup(): Unit = {
      /* We need to verify our own identity. Otherwise we might mess with a new,
       * clean state after an exception (if we are pre-exception).
       */
      globalCache.remove(path, this)
      // aggressively free stuff for GC
      _version = null
      _files = null
    }

    /** Updates this file with the given [[ScalaJSIRContainer]].
     *
     *  May only be called by a thread, if it holds a reference to this file.
     */
    def update(file: IRContainerImpl)(implicit ec: ExecutionContext): Unit = {
      assert(_references.get > 0, "Updating an unreferenced file")
      assert(file.path == path, s"Path mismatch: $path, ${file.path}")

      // Helper to ensure v is stable during check
      @inline
      def upToDate(v: Option[String]) = v.isDefined && v == file.version

      if (upToDate(_version)) {
        // yeepeeh, nothing to do
        statsReused.incrementAndGet()
      } else {
        // We need to update this. We synchronize
        synchronized {
          if (upToDate(_version)) {
            // someone else had the same idea and did our work
            statsReused.incrementAndGet()
          } else {
            statsInvalidated.incrementAndGet()
            _files = clearOnFail {
              file.sjsirFiles.map { files =>
                files.map { file =>
                  new PersistentIRFile(IRFileImpl.fromIRFile(file))
                }
              }
            }
            _version = file.version
          }
        }
      }
    }
  }

  private final class PersistentIRFile(private[this] var _irFile: IRFileImpl)(
      implicit ec: ExecutionContext) extends IRFileImpl(_irFile.path, _irFile.version) {

    @volatile
    private[this] var _tree: Future[ClassDef] = null

    // Force reading of entry points since we'll definitely need them.
    private[this] val _entryPointsInfo: Future[EntryPointsInfo] = _irFile.entryPointsInfo

    override def entryPointsInfo(implicit ec: ExecutionContext): Future[EntryPointsInfo] =
      _entryPointsInfo

    override def tree(implicit ec: ExecutionContext): Future[ClassDef] = {
      if (_tree == null) {
        synchronized {
          if (_tree == null) { // check again, race!
            loadTree()
          }
        }
      }

      _tree
    }

    /** Must be called under synchronization only */
    private def loadTree()(implicit ec: ExecutionContext): Unit = {
      statsTreesRead.incrementAndGet()
      _tree = clearOnFail(_irFile.tree) // This can fail due to I/O
      _irFile = null // Free for GC
    }
  }

  /** If something fails, we clear the `globalCache` to avoid leaks. The already
   *  existing [[PersistedFiles]]s may continue to exist. This is OK, since in
   *  the worst case they will try to remove themselves form a map in which they
   *  are not anymore.
   */
  @inline
  private def clearOnFail[T](v: => Future[T])(implicit ec: ExecutionContext): Future[T] =
    clearOnThrow(v).andThen { case Failure(_) => globalCache.clear() }

  @inline
  private def clearOnThrow[T](body: => T): T = {
    try {
      body
    } catch {
      case t: Throwable =>
        globalCache.clear()
        throw t
    }
  }
}

object StandardIRFileCache {
  final class Stats private[StandardIRFileCache] (
      val reused: Int,
      val invalidated: Int,
      val treesRead: Int
  ) extends IRFileCache.Stats {
    def logLine: String = {
      s"reused: $reused -- " +
      s"invalidated: $invalidated -- " +
      s"trees read: $treesRead"
    }
  }
}
