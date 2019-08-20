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

abstract class AbstractSetTest extends SetTest {
  def factory: AbstractSetFactory
}

trait AbstractSetFactory extends SetFactory {
  def empty[E: ClassTag]: ju.AbstractSet[E]
}
