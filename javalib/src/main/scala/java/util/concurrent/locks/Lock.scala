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

package java.util.concurrent.locks

import java.util.concurrent.TimeUnit

trait Lock {
  def lock(): Unit
  def lockInterruptibly(): Unit
  def tryLock(): Boolean
  def tryLock(time: Long, unit: TimeUnit): Boolean
  def unlock(): Unit

  // Not implemented:
  // def newCondition(): Condition
}
