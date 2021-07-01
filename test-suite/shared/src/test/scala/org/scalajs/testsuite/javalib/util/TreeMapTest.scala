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
import java.util.function.{BiConsumer, BiFunction, Function}

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.javalib.util.concurrent.ConcurrentMapFactory
import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

import scala.reflect.ClassTag

import Utils._

abstract class TreeMapTest(val factory: TreeMapFactory)
    extends AbstractMapTest
    with NavigableMapTest {

  @Test
  def comparator(): Unit = {
    assertNull(new ju.TreeMap[String, String]().comparator())

    val cmp = ju.Comparator.naturalOrder[String]()

    assertSame(cmp, new ju.TreeMap[String, String](cmp).comparator())
  }
}

class TreeMapWithoutNullTest extends TreeMapTest(new TreeMapFactory)

class TreeMapWithNullTest extends TreeMapTest(new TreeMapWithNullFactory)

class TreeMapFactory extends AbstractMapFactory with NavigableMapFactory {
  def implementationName: String = "java.util.TreeMap"

  def empty[K: ClassTag, V: ClassTag]: ju.TreeMap[K, V] =
    new ju.TreeMap[K, V]

  def allowsNullKeys: Boolean = false

  def allowsNullValues: Boolean = true

  override def allowsNullKeysQueries: Boolean = false

  override def allowsSupertypeKeyQueries: Boolean = false
}

class TreeMapWithNullFactory extends TreeMapFactory {
  override def implementationName: String =
    super.implementationName + " (allows nulls)"

  override def empty[K: ClassTag, V: ClassTag]: ju.TreeMap[K, V] = {
    val natural = ju.Comparator.comparing[K, Comparable[Any]](
        ((_: K).asInstanceOf[Comparable[Any]]): Function[K, Comparable[Any]])
    new ju.TreeMap[K, V](ju.Comparator.nullsFirst(natural))
  }

  override def allowsNullKeys: Boolean = true

  override def allowsNullKeysQueries: Boolean = true
}
