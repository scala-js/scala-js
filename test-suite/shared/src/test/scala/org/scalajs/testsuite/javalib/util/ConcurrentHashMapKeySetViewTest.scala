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

class ConcurrentHashMapKeySetViewTest extends SetTest {
  override def factory: ConcurrentHashMapKeySetViewFactory = new ConcurrentHashMapKeySetViewFactory
}

class ConcurrentHashMapKeySetViewFactory extends SetFactory {
  override def implementationName: String =
    "java.util.ConcurrentHashMap.KeySetView"

  override def allowsNullElementQuery: Boolean = false
  override def allowsNullElement: Boolean = false

  override def empty[E: ClassTag]: ju.Set[E] =
    ju.concurrent.ConcurrentHashMap.newKeySet[E]()
}
