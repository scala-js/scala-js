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

/** An optional subcache.
 *
 *  Shallow shell around another cache. It discards the instance of the
 *  underlying cache when the latter was not used in a run.
 *
 *  `CacheOption` has a single abstract method `createValue`. It is designed
 *  to be constructible as a SAM lambda.
 */
abstract class CacheOption[Value <: Cache] extends Cache with CacheAggregate {
  private var initialized: Boolean = false
  private var underlying: Value = null.asInstanceOf[Value]

  protected def createValue(): Value

  override def invalidate(): Unit = {
    super.invalidate()
    initialized = false
    underlying = null.asInstanceOf[Value] // TODO do we need to invalidate the subcache?
  }

  def get(): Value = {
    markUsed()
    if (!initialized) {
      underlying = createValue()
      initialized = true
    }
    underlying.markUsed()
    underlying
  }

  override def cleanAfterRun(): Boolean = {
    val result = super.cleanAfterRun()
    if (result && !underlying.cleanAfterRun())
      invalidate()
    result
  }
}
