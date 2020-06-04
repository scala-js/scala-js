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

package java.lang

class ThreadLocal[T] {
  private var hasValue: scala.Boolean = false
  private var v: T = _

  protected def initialValue(): T = null.asInstanceOf[T]

  def get(): T = {
    if (!hasValue)
      set(initialValue())
    v
  }

  def set(o: T): Unit = {
    v = o
    hasValue = true
  }

  def remove(): Unit = {
    hasValue = false
    v = null.asInstanceOf[T] // for gc
  }
}
