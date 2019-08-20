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

import scala.language.implicitConversions

import java.{util => ju}

import scala.reflect.ClassTag

class HashSetTest extends AbstractSetTest {
  def factory: HashSetFactory = new HashSetFactory
}

class HashSetFactory extends AbstractSetFactory {
  def implementationName: String =
    "java.util.HashSet"

  def empty[E: ClassTag]: ju.HashSet[E] =
    new ju.HashSet[E]()

  def allowsNullElement: Boolean = true
}
