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
import org.junit.Assert._

import org.scalajs.junit.utils._

import java.io.IOException

class ExpectTest {
  @Test(expected = classOf[IOException])
  def expectNormal(): Unit = throw new IOException

  @Test(expected = classOf[IOException])
  def failExpectDifferent(): Unit = throw new IllegalArgumentException

  @Test(expected = classOf[IOException])
  def failExpectNoThrow(): Unit = ()

  @Test(expected = classOf[AssertionError])
  def expectAssert(): Unit = throw new AssertionError

  @Test(expected = classOf[AssertionError])
  def failExpectAssert(): Unit = ()
}

class ExpectTestAssertions extends JUnitTest {
  protected def expectedOutput(builder: OutputBuilder): OutputBuilder = {
    builder
      .success("expectNormal")
      .wrongException("failExpectDifferent",
          "Unexpected exception, expected<java.io.IOException> but was<java.lang.IllegalArgumentException>",
          classOf[IllegalArgumentException])
      .assertion("failExpectNoThrow", "Expected exception: java.io.IOException")
      .success("expectAssert")
      .assertion("failExpectAssert", "Expected exception: java.lang.AssertionError")
  }
}
