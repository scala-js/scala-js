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

package org.scalajs.testsuite.javalib.util

import java.{util => ju}

import scala.reflect.ClassTag

trait DequeTest extends CollectionTest {
  def factory: DequeFactory
}

object DequeFactory {
  def allFactories: Iterator[DequeFactory] =
    ArrayDequeFactory.allFactories
}

trait DequeFactory extends CollectionFactory {
  def empty[E: ClassTag]: ju.Deque[E]
}
