/*
 * Ported from https://github.com/junit-team/junit
 */
package org.junit.internal

import org.junit.Assert

class InexactComparisonCriteria private (val fDelta: AnyRef)
    extends ComparisonCriteria {

  def this(delta: Double) =
    this(delta: java.lang.Double)

  def this(delta: Float) =
    this(delta: java.lang.Float)

  override protected def assertElementsEqual(expected: AnyRef,
      actual: AnyRef): Unit = {
    Assert.assertEquals(expected.asInstanceOf[Double],
      actual.asInstanceOf[Double], fDelta.asInstanceOf[Double])
  }
}
