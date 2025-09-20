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

import org.scalajs.ir.Version

/** A cache for a single value that gets invalidated based on a `Version`. */
trait VersionedCache[T] extends Cache {
  private var _lastVersion: Version = Version.Unversioned
  private var _value: T = null.asInstanceOf[T]

  override def invalidate(): Unit = {
    super.invalidate()
    _lastVersion = Version.Unversioned
    _value = null.asInstanceOf[T]
  }

  private def updateVersion(version: Version): Boolean = {
    markUsed()
    if (_lastVersion.sameVersion(version)) {
      false
    } else {
      invalidate()
      _lastVersion = version
      true
    }
  }

  protected final def getOrCompute(version: Version, computeValue: => T): T = {
    if (updateVersion(version))
      _value = computeValue
    _value
  }

  protected final def getOrComputeWithChanged(version: Version, computeValue: => T): (T, Boolean) = {
    if (updateVersion(version)) {
      _value = computeValue
      (_value, true)
    } else {
      (_value, false)
    }
  }
}
