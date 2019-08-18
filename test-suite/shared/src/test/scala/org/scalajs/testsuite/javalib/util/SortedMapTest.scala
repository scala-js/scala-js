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

import org.junit.Test
import org.junit.Assert._

import scala.reflect.ClassTag

trait SortedMapTest extends MapTest {

  def factory: SortedMapFactory

  // TODO: implement tests (when we port the first SortedMap)

  @Test def should_always_be_sorted(): Unit = {

  }

  @Test def should_return_the_firstKey(): Unit = {

  }

  @Test def should_return_the_lastKey(): Unit = {

  }

  @Test def should_return_a_proper_headMap(): Unit = {

  }

  @Test def should_return_a_proper_tailMap(): Unit = {

  }

  @Test def should_return_a_proper_subMap(): Unit = {

  }
}

object SortedMapFactory {
  def allFactories: Iterator[SortedMapFactory] = Iterator.empty
}

trait SortedMapFactory extends MapFactory {
  def empty[K: ClassTag, V: ClassTag]: ju.SortedMap[K, V]
}
