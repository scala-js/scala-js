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

package org.scalajs.testsuite.javalib.lang

import org.junit.Test
import org.junit.Assert._

class ThrowablesTestOnJDK17 {
  @Test def ioobeConstructorsWithIndex(): Unit = {
    /* Technically the `Int` overload was already added in JDK 9, so it should
     * be part of `require-jdk11/`. We test them together here, though.
     */

    val e1 = new IndexOutOfBoundsException(542)
    assertTrue(e1.getMessage(), e1.getMessage().contains("542"))

    val e2 = new IndexOutOfBoundsException(1234L)
    assertTrue(e2.getMessage(), e2.getMessage().contains("1234"))
  }
}
