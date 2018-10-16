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

package org.scalajs.testsuite.javalib.util.concurrent

import java.{util => ju}

import org.scalajs.testsuite.javalib.util.MapFactory

import scala.reflect.ClassTag

object ConcurrentMapFactory {
  def allFactories: Iterator[ConcurrentMapFactory] =
    ConcurrentHashMapFactory.allFactories
}

trait ConcurrentMapFactory extends MapFactory {
  def empty[K: ClassTag, V: ClassTag]: ju.concurrent.ConcurrentMap[K, V]

  override def allowsNullValuesQueries: Boolean = false

  override def allowsNullKeysQueries: Boolean = false
}
