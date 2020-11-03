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

package org.scalajs.linker.interface

import scala.concurrent._

/** Centralized Scala.js IR cache.
 *
 *  Caches all Scala.js IR used in a given JVM. It supports creating of multiple
 *  sub-caches ([[IRFileCache.Cache]]) that track individual file sets.
 *  The global cache is fully thread-safe. However, the sub-caches are not.
 */
abstract class IRFileCache private[interface] () {

  /** Create a new sub-cache.
   *
   *  Users should call [[IRFileCache.Cache.free]] once they are done to allow
   *  for more aggressive GC.
   */
  def newCache: IRFileCache.Cache

  /** Approximate statistics about the cache usage */
  def stats: IRFileCache.Stats

  /** Reset statistics */
  def clearStats(): Unit
}

object IRFileCache {

  /** A cache to use for individual runs. Not threadsafe */
  abstract class Cache private[interface] () {

    /** Extract and cache IR.
     *
     *  The returned value is valid until the next invocation of [[cached]] or
     *  [[free]].
     *
     *  @note Updating any of the underlying files in the container during the
     *      lifetime of a returned [[IRFile]] yields unspecified behavior.
     */
    def cached(files: Seq[IRContainer])(implicit ec: ExecutionContext): Future[Seq[IRFile]]

    /** Should be called if this cache is not used anymore.
     *
     *  Frees resources in the global cache, if they are not used anymore.
     *  The cache may be reused after calling [[free]] (but this is not any
     *  faster than calling [[IRFileCache.newCache]], modulo the object
     *  allocation).
     */
    def free(): Unit
  }

  /** Statistics about an individual run. */
  abstract class Stats private[interface] {

    /** Descriptive line to display in logs */
    def logLine: String
  }
}
