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

package org.scalajs.junit

import org.junit._
import org.scalajs.junit.utils._

object ExceptionAfterClass {
  @AfterClass def afterClass(): Unit =
    throw new IllegalArgumentException("foo")
}

class ExceptionAfterClass {
  @Test def test1(): Unit = ()
  @Test def test2(): Unit = ()
}

class ExceptionAfterClassAssertions extends JUnitTest
