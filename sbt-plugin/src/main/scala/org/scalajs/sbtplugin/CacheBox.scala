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

package org.scalajs.sbtplugin

/** A CacheBox is a support class to abuse an sbt setting as a cache.
 *
 *  A CacheBox is needed, once the cached result needs to depend on a task,
 *  since then it cannot simply be made a setting anymore.
 *
 *  @note
 *    **Unstable API**: this API is subject to backward incompatible changes in
 *    future minor versions of Scala.js.
 */
final class CacheBox[T] {
  private[this] var initialized: Boolean = _
  private[this] var value: T = _

  def ensure(f: => T): T = synchronized {
    if (!initialized) {
      value = f
      initialized = true
    }
    value
  }

  def foreach(f: T => Unit): Unit = synchronized {
    if (initialized) {
      f(value)
    }
  }
}
