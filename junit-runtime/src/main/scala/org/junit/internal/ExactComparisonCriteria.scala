/*
 * Ported from https://github.com/junit-team/junit
 */
package org.junit.internal

import org.junit.Assert

class ExactComparisonCriteria extends ComparisonCriteria {
  override protected def assertElementsEqual(expected: AnyRef,
      actual: AnyRef): Unit = {
    Assert.assertEquals(expected, actual)
  }
}
