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

class ExceptionInAfterTest {
  @After def after(): Unit =
    throw new UnsupportedOperationException("Exception in after()")

  /* Even if the test method declares expecting the exception thrown by the
   * after() method, it must result in an error, not a success.
   */
  @Test(expected = classOf[UnsupportedOperationException])
  def test(): Unit =
    throw new UnsupportedOperationException("Exception in test()")
}

class ExceptionInAfterTestAssertions extends JUnitTest {
  protected def expectedOutput(builder: OutputBuilder): OutputBuilder = {
    builder.exception("test",
        "Exception in after()",
        classOf[UnsupportedOperationException])
  }
}
