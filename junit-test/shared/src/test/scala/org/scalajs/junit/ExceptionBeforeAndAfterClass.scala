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

object ExceptionBeforeAndAfterClass {
  @BeforeClass def beforeClass(): Unit =
    throw new AssertionError("before")

  @AfterClass def afterClass(): Unit =
    throw new IllegalArgumentException("after")
}

class ExceptionBeforeAndAfterClass {
  @Test def test1(): Unit = ()
  @Test def test2(): Unit = ()
}

class ExceptionBeforeAndAfterClassAssertions extends JUnitTest
