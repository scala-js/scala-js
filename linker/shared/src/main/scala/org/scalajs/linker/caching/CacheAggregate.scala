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

/** Marker trait for caches that aggregate subcaches.
 *
 *  This trait is for documentation purposes only. Cache aggregates *own* their
 *  subcaches. The aggregate's `cleanAfterRun()` method calls the same method
 *  its subcaches. It may discard subcaches that were not used in the run.
 */
trait CacheAggregate extends Cache
