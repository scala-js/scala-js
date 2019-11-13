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
 */
private[sbtplugin] final class CacheBox[T] {
  private[this] var value: T = _

  def ensure(f: => T): T = synchronized {
    if (value == null) {
      value = f
    }
    value
  }

  def foreach(f: T => Unit): Unit = synchronized {
    if (value != null) {
      f(value)
    }
  }
}
