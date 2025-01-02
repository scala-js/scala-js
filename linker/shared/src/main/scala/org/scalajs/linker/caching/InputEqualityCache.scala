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

/** A cache that depends on an `input: I`, testing with `==`.
 *
 *  On first request, or when the input changes, the value is recomputed.
 *
 *  @tparam I
 *    the type of input, for which `==` must be meaningful
 */
trait InputEqualityCache[I, A] extends Cache {
  private var initialized: Boolean = false
  private var lastInput: I = null.asInstanceOf[I]
  private var value: A = null.asInstanceOf[A]

  override def invalidate(): Unit = {
    super.invalidate()
    initialized = false
    lastInput = null.asInstanceOf[I]
    value = null.asInstanceOf[A]
  }

  protected final def getOrCompute(input: I, v: => A): A = {
    markUsed()
    if (!initialized || input != lastInput) {
      value = v
      lastInput = input
      initialized = true
    }
    value
  }
}
