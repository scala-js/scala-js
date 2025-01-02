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

/** A map of subcaches.
 *
 *  A cache map is like a `HashMap` with auto-computed values. Values must be
 *  caches themselves.
 *
 *  `CacheMap` itself is not thread-safe. Use [[ConcurrentCacheMap]] if several
 *  threads must concurrently call `get()`.
 *
 *  `CacheMap` has a single abstract method `createValue`. It is designed to
 *  be constructible as a SAM lambda.
 */
abstract class CacheMap[Key, Value <: Cache] extends Cache with CacheAggregate {
  private val _caches: java.util.Map[Key, Value] = createUnderlyingHashMap()

  protected def createUnderlyingHashMap(): java.util.Map[Key, Value] =
    new java.util.HashMap()

  protected def createValue(key: Key): Value

  /** Unique instance of the lambda that we pass to `computeIfAbsent`. */
  private val createValueFunction: java.util.function.Function[Key, Value] =
    (key: Key) => createValue(key)

  override def invalidate(): Unit = {
    super.invalidate()
    _caches.clear() // TODO do we need to invalidate all subcaches?
  }

  def get(key: Key): Value = {
    markUsed()
    val result = _caches.computeIfAbsent(key, createValueFunction)
    result.markUsed()
    result
  }

  override def cleanAfterRun(): Boolean = {
    val result = super.cleanAfterRun()
    if (result)
      _caches.entrySet().removeIf(!_.getValue().cleanAfterRun())
    result
  }
}
