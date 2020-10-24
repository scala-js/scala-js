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

import org.junit.Test

import java.{util => ju}

import scala.reflect.ClassTag

class ArrayListTest extends AbstractListTest {

  override def factory: AbstractListFactory = new ArrayListFactory

  @Test def ensureCapacity(): Unit = {
    // note that these methods become no ops in js
    val al = new ju.ArrayList[String]
    al.ensureCapacity(0)
    al.ensureCapacity(34)
    al.trimToSize()
  }
}

class ArrayListFactory extends AbstractListFactory {
  override def implementationName: String =
    "java.util.ArrayList"

  override def empty[E: ClassTag]: ju.ArrayList[E] =
    new ju.ArrayList[E]
}
