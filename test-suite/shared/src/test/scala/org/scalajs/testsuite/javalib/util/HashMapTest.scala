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

class HashMapTest extends MapTest {
  def factory(): HashMapFactory = new HashMapFactory
}

class HashMapFactory extends AbstractMapFactory {
  override def implementationName: String =
    "java.util.HashMap"

  override def empty[K: ClassTag, V: ClassTag]: ju.HashMap[K, V] =
    new ju.HashMap[K, V]

  def allowsNullKeys: Boolean = true
  def allowsNullValues: Boolean = true
}
