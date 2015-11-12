package org.scalajs.testsuite.junit

import org.hamcrest.CoreMatchers._
import org.junit.Assert._
import org.junit.Assume._
import org.junit._
import org.junit.internal.AssumptionViolatedException

class JUnitAssumptionsTest {

  private val ShallNotPass = false

  def testIfAssumePass(assumption: => Unit, shouldPass: Boolean = true): Unit = {
    try {
      assumption
      if (!shouldPass)
        fail("Assumption should have failed")
    } catch {
      case assVio: AssumptionViolatedException =>
        if (shouldPass)
          throw assVio
    }
  }

  @Test
  def testAssumeTrue(): Unit = {
    testIfAssumePass(assumeTrue("true be assumed to be true", true))
    testIfAssumePass(assumeTrue(true))
    testIfAssumePass(assumeTrue("false be assumed to be true", false), ShallNotPass)
    testIfAssumePass(assumeTrue(false), ShallNotPass)

    testIfAssumePass(assumeFalse("false be assumed to be false", false))
    testIfAssumePass(assumeFalse(false))
    testIfAssumePass(assumeFalse("true be assumed to be false", true), ShallNotPass)
    testIfAssumePass(assumeFalse(true), ShallNotPass)
  }

  @Test
  def testAssumeNotNull(): Unit = {
    testIfAssumePass(assumeNotNull())
    testIfAssumePass(assumeNotNull(new Object))
    testIfAssumePass(assumeNotNull("", new Object, " "))

    testIfAssumePass(assumeNotNull(null), ShallNotPass)
    testIfAssumePass(assumeNotNull(new Object, null), ShallNotPass)
    testIfAssumePass(assumeNotNull(null, new Object), ShallNotPass)
  }

  @Test
  def testAssumeThat(): Unit = {
    testIfAssumePass(assumeThat(null, nullValue()))
    testIfAssumePass(assumeThat(null, notNullValue()), ShallNotPass)

    testIfAssumePass(assumeThat(new Object, notNullValue()))
    testIfAssumePass(assumeThat(new Object, nullValue()), ShallNotPass)

    testIfAssumePass(assumeThat(new Object, notNullValue(classOf[AnyRef])))

    testIfAssumePass(assumeThat(1, is(1)))
    testIfAssumePass(assumeThat(1, is(2)), ShallNotPass)

    testIfAssumePass(assumeThat(1, not(is(2))))
    testIfAssumePass(assumeThat(1, not(is(1))), ShallNotPass)

    testIfAssumePass(assumeThat(1, is(not(2))))
    testIfAssumePass(assumeThat(1, is(not(1))), ShallNotPass)

    testIfAssumePass(assumeThat(1, not(2)))
    testIfAssumePass(assumeThat(1, not(1)), ShallNotPass)
  }

  @Test
  def testAssumesNoException(): Unit = {
    testIfAssumePass(assumeNoException("assumeNoException(null) should succeed", null))
    testIfAssumePass(assumeNoException(null))

    testIfAssumePass(assumeNoException("assumeNoException(new Throwable) should succeed",
        new Throwable), ShallNotPass)
    testIfAssumePass(assumeNoException(new Throwable), ShallNotPass)
  }
}
