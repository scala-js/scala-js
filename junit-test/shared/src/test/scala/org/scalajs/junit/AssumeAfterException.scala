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

import org.junit.Assume._
import org.junit._

import org.scalajs.junit.utils._

class AssumeAfterException {
  @After def after(): Unit =
    assumeTrue("This assume should not pass", false)

  @Test def test(): Unit =
    throw new IllegalArgumentException("test throws")
}

class AssumeAfterExceptionAssertions extends JUnitTest {
  protected def expectedOutput(builder: OutputBuilder): OutputBuilder = {
    builder.exceptionAndAnotherExceptionInAfter("test",
        "test throws",
        classOf[IllegalArgumentException],
        "This assume should not pass",
        classOf[org.junit.internal.AssumptionViolatedException])
  }
}
