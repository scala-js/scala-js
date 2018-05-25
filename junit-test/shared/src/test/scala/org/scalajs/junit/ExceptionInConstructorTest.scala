package org.scalajs.junit

import org.junit._
import org.junit.Assert._

import org.scalajs.junit.utils._

class ExceptionInConstructorTest {
  throw new UnsupportedOperationException(
      "Exception while constructing the test class")

  @Before def before(): Unit =
    throw new IllegalStateException("before() must not be called")

  @After def after(): Unit =
    throw new IllegalStateException("after() must not be called")

  /* Even if the test method declares expecting the exception thrown by the
   * constructor, it must result in an error, not a success.
   */
  @Test(expected = classOf[UnsupportedOperationException])
  def test(): Unit =
    throw new IllegalStateException("test() must not be called")
}

class ExceptionInConstructorTestAssertions extends JUnitTest {
  protected def expectedOutput(builder: OutputBuilder): OutputBuilder = {
    builder.exception("test",
        "Exception while constructing the test class",
        classOf[UnsupportedOperationException])
  }
}
