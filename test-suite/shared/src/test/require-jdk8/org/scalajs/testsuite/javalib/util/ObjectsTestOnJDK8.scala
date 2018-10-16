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
import org.junit.Assert._

import java.util.Objects

class ObjectsTestOnJDK8 {

  @Test def isNull(): Unit = {
    assertTrue(Objects.isNull(null))
    assertFalse(Objects.isNull(new Object))
  }

  @Test def nonNull(): Unit = {
    assertFalse(Objects.nonNull(null))
    assertTrue(Objects.nonNull(new Object))
  }
}
