/*
 * Ported from https://github.com/junit-team/junit
 */
package org.junit.internal

import org.junit.Assert

abstract class ComparisonCriteria {

  def arrayEquals(message: String, expecteds: AnyRef, actuals: AnyRef): Unit =
    arrayEquals(message, expecteds, actuals, true)

  private def arrayEquals(message: String, expecteds: AnyRef, actuals: AnyRef,
      outer: Boolean): Unit = {
    if (expecteds != actuals &&
        !java.util.Arrays.deepEquals(Array(expecteds), Array(actuals))) {

      val header = if (message == null) "" else s"$message: "

      val exceptionMessage = if (outer) header else ""
      val expectedsLength =
        assertArraysAreSameLength(expecteds, actuals, exceptionMessage)

      for (i <- 0 until expectedsLength) {
        val expected = get(expecteds, i)
        val actual = get(actuals, i)

        if (isArray(expected) && isArray(actual)) {
          try {
            arrayEquals(message, expected, actual, false)
          } catch {
            case e: ArrayComparisonFailure =>
              e.addDimension(i)
              throw e
            case e: AssertionError =>
              throw new ArrayComparisonFailure(header, e, i)
          }
        } else {
          try {
            assertElementsEqual(expected, actual)
          } catch {
            case e: AssertionError =>
              throw new ArrayComparisonFailure(header, e, i)
          }
        }
      }
    }
  }

  private def isArray(expected: AnyRef): Boolean =
    expected.isInstanceOf[Array[_]]

  private def assertArraysAreSameLength(expecteds: AnyRef, actuals: AnyRef,
        header: String): Int = {
    if (expecteds == null)
      Assert.fail(header + "expected array was null")
    if (actuals == null)
      Assert.fail(header + "actual array was null")
    val actualsLength = actuals.asInstanceOf[Array[_]].length
    val expectedsLength = expecteds.asInstanceOf[Array[_]].length
    if (actualsLength != expectedsLength) {
      Assert.fail(header +
          "array lengths differed, expected.length=" + expectedsLength +
          " actual.length=" + actualsLength)
    }
    expectedsLength
  }

  @inline
  private def get(arr: AnyRef, i: Int): AnyRef =
    arr.asInstanceOf[Array[_]](i).asInstanceOf[AnyRef]

  private def length(arr: AnyRef): Int =
    arr.asInstanceOf[Array[_]].length

  protected def assertElementsEqual(expected: AnyRef, actual: AnyRef): Unit
}
