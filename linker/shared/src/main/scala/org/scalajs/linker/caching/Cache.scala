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

package org.scalajs.linker.caching

/** Base class of all caches.
 *
 *  A cache can be invalidated to clear everything it cached.
 *
 *  Each cache keeps track of whether it was *used* in any given run.
 *  `cleanAfterRun()` invalidates the cache if it was not used. Then it resets
 *  the tracker to prepare for the next run.
 */
abstract class Cache {
  private var _cacheUsed: Boolean = false

  protected[caching] def markUsed(): Unit =
    _cacheUsed = true

  def invalidate(): Unit = ()

  def cleanAfterRun(): Boolean = {
    val wasUsed = _cacheUsed
    if (!wasUsed)
      invalidate()
    _cacheUsed = false
    wasUsed
  }
}
