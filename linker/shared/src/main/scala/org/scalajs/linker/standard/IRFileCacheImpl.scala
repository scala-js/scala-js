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

import org.scalajs.linker._

/** IRFileCacheImpl opens [[IRFileCache]] for inheritance. */
abstract class IRFileCacheImpl extends IRFileCache

object IRFileCacheImpl {
  /** Cache opens [[IRFileCache.Cache]] for inheritance. */
  abstract class Cache extends IRFileCache.Cache

  /** Stats opens [[IRFileCache.Stats]] for inheritance. */
  abstract class Stats extends IRFileCache.Stats
}
