package org.scalajs.junit

import org.junit._
import org.junit.Assert._

import org.scalajs.junit.utils._

class ExceptionInBeforeTest {
  @Before def before(): Unit =
    throw new UnsupportedOperationException("Exception in before()")

  @After def after(): Unit =
    throw new IllegalArgumentException("after() must actually be called")

  /* Even if the test method declares expecting the exception thrown by the
   * before() method, it must result in an error, not a success.
   */
  @Test(expected = classOf[UnsupportedOperationException])
  def test(): Unit =
    throw new IllegalStateException("test() must not be called")
}

class ExceptionInBeforeTestAssertions extends JUnitTest {
  protected def expectedOutput(builder: OutputBuilder): OutputBuilder = {
    builder.exceptionAndAnotherExceptionInAfter("test",
        "Exception in before()",
        classOf[UnsupportedOperationException],
        "after() must actually be called",
        classOf[IllegalArgumentException])
  }
}
