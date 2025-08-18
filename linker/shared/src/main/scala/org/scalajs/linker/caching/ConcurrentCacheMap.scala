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

/** A concurrent map of subcaches.
 *
 *  A concurrent cache map is a [[CacheMap]] on which concurrent calls to `get`
 *  are allowed (even for the same key).
 *
 *  `cleanAfterRun()` is not thread-safe. There must exist a happens-before
 *  relationship between any call to `cleanAfterRun()` and other methods.
 *
 *  `ConcurrentCacheMap` has a single abstract method `initialValue`. It is
 *  designed to be constructible as a SAM lambda.
 */
abstract class ConcurrentCacheMap[Key, Value <: Cache] extends CacheMap[Key, Value] {
  override protected def createUnderlyingHashMap(): java.util.Map[Key, Value] =
    new java.util.concurrent.ConcurrentHashMap()
}
