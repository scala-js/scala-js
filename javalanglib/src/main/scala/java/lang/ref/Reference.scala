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

package java.lang.ref

abstract class Reference[T >: Null <: AnyRef](private[this] var referent: T) {
  def get(): T = referent
  def clear(): Unit = referent = null
  def isEnqueued(): Boolean = false
  def enqueue(): Boolean = false
}
