/*
 * Ported from https://github.com/junit-team/junit
 */
package org.junit

import org.junit.internal.InexactComparisonCriteria
import org.junit.internal.ExactComparisonCriteria
import org.hamcrest.Matcher
import org.hamcrest.MatcherAssert

object Assert {
  @noinline
  def assertTrue(message: String, condition: Boolean): Unit = {
    if (!condition)
      fail(message)
  }

  @noinline
  def assertTrue(condition: Boolean): Unit =
    assertTrue(null, condition)

  @noinline
  def assertFalse(message: String, condition: Boolean): Unit =
    assertTrue(message, !condition)

  @noinline
  def assertFalse(condition: Boolean): Unit =
    assertFalse(null, condition)

  @noinline
  def fail(message: String): Unit =
    if (message eq null) throw new AssertionError()
    else throw new AssertionError(message)

  @noinline
  def fail(): Unit =
    fail(null)

  @noinline
  def assertEquals(message: String, expected: Any, actual: Any): Unit = {
    if (!equalsRegardingNull(expected, actual)) {
      (expected, actual) match {
        case (expectedString: String, actualString: String) =>
          val cleanMsg: String = if (message == null) "" else message
          throw new ComparisonFailure(cleanMsg, expectedString, actualString)

        case _ =>
          failNotEquals(message, expected, actual)
      }
    }
  }

  @inline
  private def equalsRegardingNull(expected: Any, actual: Any): Boolean =
    if (expected == null) actual == null
    else isEquals(expected, actual)

  @inline
  private def isEquals(expected: Any, actual: Any): Boolean =
    expected.equals(actual)

  @noinline
  def assertEquals(expected: Any, actual: Any): Unit =
    assertEquals(null, expected, actual)

  @noinline
  def assertNotEquals(message: String, unexpected: Any, actual: Any): Unit = {
    if (equalsRegardingNull(unexpected, actual))
      failEquals(message, actual)
  }

  @noinline
  def assertNotEquals(unexpected: Any, actual: Any): Unit =
    assertNotEquals(null, unexpected, actual)

  private def failEquals(message: String, actual: Any): Unit = {
    val checkedMessage = {
      if (message != null) message
      else "Values should be different"
    }
    fail(s"$checkedMessage. Actual: $actual")
  }

  @noinline
  def assertNotEquals(message: String, unexpected: Long, actual: Long): Unit = {
    if (unexpected == actual)
      failEquals(message, actual)
  }

  @noinline
  def assertNotEquals(unexpected: Long, actual: Long): Unit =
    assertNotEquals(null, unexpected, actual)

  @noinline
  def assertNotEquals(message: String, unexpected: Double, actual: Double,
      delta: Double): Unit = {
    if (!doubleIsDifferent(unexpected, actual, delta))
      failEquals(message, actual)
  }

  @noinline
  def assertNotEquals(unexpected: Double, actual: Double, delta: Double): Unit =
    assertNotEquals(null, unexpected, actual, delta)

  @noinline
  def assertNotEquals(unexpected: Float, actual: Float, delta: Float): Unit =
    assertNotEquals(null, unexpected, actual, delta)

  @deprecated("Use assertEquals(double expected, double actual, double " +
      "epsilon) instead", "")
  @noinline
  def assertEquals(expected: Double, actual: Double): Unit = {
    fail("Use assertEquals(expected, actual, delta) to compare " +
        "floating-point numbers")
  }

  @deprecated("Use assertEquals(String message, double expected, double " +
      "actual, double epsilon) instead", "")
  @noinline
  def assertEquals(message: String, expected: Double, actual: Double): Unit = {
    fail("Use assertEquals(expected, actual, delta) to compare " +
        "floating-point numbers")
  }

  @noinline
  def assertEquals(expected: Long, actual: Long): Unit =
    assertEquals(null, expected, actual)

  @noinline
  def assertEquals(message: String, expected: Long, actual: Long): Unit =
    assertEquals(message, expected: Any, actual: Any)

  @noinline
  def assertArrayEquals(message: String, expecteds: Array[AnyRef],
      actuals: Array[AnyRef]): Unit = {
    internalArrayEquals(message, expecteds, actuals)
  }

  @noinline
  def assertArrayEquals(expecteds: Array[AnyRef],
      actuals: Array[AnyRef]): Unit = {
    assertArrayEquals(null, expecteds, actuals)
  }

  @noinline
  def assertArrayEquals(message: String, expecteds: Array[Boolean],
      actuals: Array[Boolean]): Unit = {
    internalArrayEquals(message, expecteds, actuals)
  }

  @noinline
  def assertArrayEquals(expecteds: Array[Boolean],
      actuals: Array[Boolean]): Unit = {
    assertArrayEquals(null, expecteds, actuals)
  }

  @noinline
  def assertArrayEquals(message: String, expecteds: Array[Byte],
      actuals: Array[Byte]): Unit = {
    internalArrayEquals(message, expecteds, actuals)
  }

  @noinline
  def assertArrayEquals(expecteds: Array[Byte], actuals: Array[Byte]): Unit =
    assertArrayEquals(null, expecteds, actuals)

  @noinline
  def assertArrayEquals(message: String, expecteds: Array[Char],
      actuals: Array[Char]): Unit = {
    internalArrayEquals(message, expecteds, actuals)
  }

  @noinline
  def assertArrayEquals(expecteds: Array[Char], actuals: Array[Char]): Unit =
    assertArrayEquals(null, expecteds, actuals)

  @noinline
  def assertArrayEquals(message: String, expecteds: Array[Short],
      actuals: Array[Short]): Unit = {
    internalArrayEquals(message, expecteds, actuals)
  }

  @noinline
  def assertArrayEquals(expecteds: Array[Short],
      actuals: Array[Short]): Unit = {
    assertArrayEquals(null, expecteds, actuals)
  }

  @noinline
  def assertArrayEquals(message: String, expecteds: Array[Int],
      actuals: Array[Int]): Unit = {
    internalArrayEquals(message, expecteds, actuals)
  }

  @noinline
  def assertArrayEquals(expecteds: Array[Int], actuals: Array[Int]): Unit =
    assertArrayEquals(null, expecteds, actuals)

  @noinline
  def assertArrayEquals(message: String, expecteds: Array[Long],
      actuals: Array[Long]): Unit = {
    internalArrayEquals(message, expecteds, actuals)
  }

  @noinline
  def assertArrayEquals(expecteds: Array[Long], actuals: Array[Long]): Unit =
    assertArrayEquals(null, expecteds, actuals)

  @noinline
  def assertArrayEquals(message: String, expecteds: Array[Double],
      actuals: Array[Double], delta: Double): Unit = {
    new InexactComparisonCriteria(delta).arrayEquals(message, expecteds, actuals)
  }

  @noinline
  def assertArrayEquals(expecteds: Array[Double], actuals: Array[Double],
      delta: Double): Unit = {
    assertArrayEquals(null, expecteds, actuals, delta)
  }

  @noinline
  def assertArrayEquals(message: String, expecteds: Array[Float],
      actuals: Array[Float], delta: Float): Unit = {
    new InexactComparisonCriteria(delta).arrayEquals(message, expecteds, actuals)
  }

  @noinline
  def assertArrayEquals(expecteds: Array[Float], actuals: Array[Float],
      delta: Float): Unit = {
    assertArrayEquals(null, expecteds, actuals, delta)
  }

  private def internalArrayEquals(message: String, expecteds: AnyRef,
      actuals: AnyRef): Unit = {
    new ExactComparisonCriteria().arrayEquals(message, expecteds, actuals)
  }

  @noinline
  def assertEquals(message: String, expected: Double, actual: Double,
      delta: Double): Unit = {
    if (doubleIsDifferent(expected, actual, delta)) {
      failNotEquals(message, expected, actual)
    }
  }

  @noinline
  def assertEquals(message: String, expected: Float, actual: Float,
      delta: Float): Unit = {
    if (floatIsDifferent(expected, actual, delta)) {
      failNotEquals(message, expected, actual)
    }
  }

  @noinline
  def assertNotEquals(message: String, unexpected: Float, actual: Float,
      delta: Float): Unit = {
    if (!floatIsDifferent(unexpected, actual, delta))
      failEquals(message, actual)
  }

  private def doubleIsDifferent(d1: Double, d2: Double,
      delta: Double): Boolean = {
    java.lang.Double.compare(d1, d2) != 0 && Math.abs(d1 - d2) > delta
  }

  private def floatIsDifferent(f1: Float, f2: Float, delta: Float): Boolean =
    java.lang.Float.compare(f1, f2) != 0 && Math.abs(f1 - f2) > delta

  @noinline
  def assertEquals(expected: Double, actual: Double, delta: Double): Unit =
    assertEquals(null, expected, actual, delta)

  @noinline
  def assertEquals(expected: Float, actual: Float, delta: Float): Unit =
    assertEquals(null, expected, actual, delta)

  @noinline
  def assertNotNull(message: String, obj: Any): Unit =
    assertTrue(message, obj != null)

  @noinline
  def assertNotNull(obj: Any): Unit =
    assertNotNull(null, obj)

  @noinline
  def assertNull(message: String, obj: Any): Unit = {
    if (obj != null)
      failNotNull(message, obj)
  }

  @noinline
  def assertNull(obj: Any): Unit =
    assertNull(null, obj)

  private def failNotNull(message: String, actual: Any): Unit = {
    val formatted = if (message != null) message + " " else ""
    fail(s"${formatted}expected null, but was:<$actual}>")
  }

  @noinline
  def assertSame(message: String, expected: Any, actual: Any): Unit = {
    if (expected.asInstanceOf[AnyRef] ne actual.asInstanceOf[AnyRef])
      failNotSame(message, expected, actual)
  }

  @noinline
  def assertSame(expected: Any, actual: Any): Unit =
    assertSame(null, expected, actual)

  @noinline
  def assertNotSame(message: String, unexpected: Any, actual: Any): Unit = {
    if (unexpected.asInstanceOf[AnyRef] eq actual.asInstanceOf[AnyRef])
      failSame(message)
  }

  @noinline
  def assertNotSame(unexpected: Any, actual: Any): Unit =
    assertNotSame(null, unexpected, actual)

  private def failSame(message: String): Unit = {
    if (message == null)
      fail("expected not same")
    else
      fail(s"$message expected not same")
  }

  private def failNotSame(message: String, expected: Any, actual: Any): Unit = {
    if (message == null)
      fail(s"expected same:<$expected> was not:<$actual>")
    else
      fail(s"$message expected same:<$expected> was not:<$actual>")
  }

  @inline
  private def failNotEquals(message: String, expected: Any, actual: Any): Unit =
    fail(format(message, expected, actual))

  private[junit] def format(message: String, expected: Any, actual: Any): String = {
    val formatted = if (message != null && message != "") message + " " else ""
    val expectedString = String.valueOf(expected)
    val actualString = String.valueOf(actual)
    if (expectedString == actualString) {
      val expectedFormatted = formatClassAndValue(expected, expectedString)
      val actualFormatted = formatClassAndValue(actual, actualString)
      s"${formatted}expected: $expectedFormatted but was: $actualFormatted"
    } else {
      s"${formatted}expected:<$expectedString> but was:<$actualString>"
    }
  }

  private def formatClassAndValue(value: Any, valueString: String): String = {
    val className = if (value == null) "null" else value.getClass.getName
    s"$className<$valueString>"
  }

  @noinline
  def assertThat[T](actual: T, matcher: Matcher[T]): Unit =
    assertThat("", actual, matcher)

  @noinline
  def assertThat[T](reason: String, actual: T, matcher: Matcher[T]): Unit =
    MatcherAssert.assertThat(reason, actual, matcher)

  // The following methods will be available on JUnit 4.13, a backport implementation
  // is being tested in JUnitAssertionTest until 4.13 is released.

  /*
  @noinline
  def assertThrows(expectedThrowable: Class[_ <: Throwable],
      runnable: ThrowingRunnable): Unit = {
    expectThrows(expectedThrowable, runnable)
  }

  @noinline
  def expectThrows[T <: Throwable](expectedThrowable: Class[T], runnable: ThrowingRunnable): T = {
    try {
      runnable.run()
      val message =
        s"expected ${expectedThrowable.getSimpleName} to be thrown," +
        " but nothing was thrown"
      throw new AssertionError(message)
    } catch {
      case actualThrown: Throwable =>
        if (expectedThrowable.isInstance(actualThrown)) {
          actualThrown.asInstanceOf[T]
        } else {
          val mismatchMessage = format("unexpected exception type thrown;",
            expectedThrowable.getSimpleName, actualThrown.getClass.getSimpleName)

          val assertionError = new AssertionError(mismatchMessage)
          assertionError.initCause(actualThrown)
          throw assertionError
        }
    }
  }

  trait ThrowingRunnable {
    def run(): Unit
  }
  */
}
