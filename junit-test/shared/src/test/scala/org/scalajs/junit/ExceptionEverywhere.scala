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

object ExceptionEverywhere {
  @BeforeClass def beforeClass(): Unit =
    throw new AssertionError("before class")

  @AfterClass def afterClass(): Unit =
    throw new AssertionError("after class")
}

class ExceptionEverywhere {
  @Before def before(): Unit = throw new AssertionError("before")
  @After def after(): Unit = throw new AssertionError("after")

  @Test def test1(): Unit = throw new AssertionError("test 1")
  @Test def test2(): Unit = throw new AssertionError("test 2")
}

class ExceptionEverywhereAssertions extends JUnitTest
