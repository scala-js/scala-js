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

  @Test def sort(): Unit = {

  }

  @Test def firstKey(): Unit = {

  }

  @Test def lastKey(): Unit = {

  }

  @Test def headMap(): Unit = {

  }

  @Test def tailMap(): Unit = {

  }

  @Test def subMap(): Unit = {

  }
}

trait SortedMapFactory extends MapFactory {
  def empty[K: ClassTag, V: ClassTag]: ju.SortedMap[K, V]
}
