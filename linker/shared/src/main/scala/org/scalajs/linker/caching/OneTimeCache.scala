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

/** Cache that holds a single value, computed the first time it is requested. */
trait OneTimeCache[A] extends Cache {
  private var initialized: Boolean = false
  private var value: A = null.asInstanceOf[A]

  override def invalidate(): Unit = {
    super.invalidate()
    initialized = false
    value = null.asInstanceOf[A]
  }

  protected final def getOrCompute(v: => A): A = {
    markUsed()
    if (!initialized) {
      value = v
      initialized = true
    }
    value
  }
}
