package org.scalajs.testsuite.util

object AssertThrows {
  /** Backport implementation of Assert.assertThrows to be used until JUnit 4.13 is
   *  released. See org.junit.Assert.scala in jUnitRuntime.
   */
  def assertThrows(expectedThrowable: Class[_ <: Throwable],
      runnable: ThrowingRunnable): Unit = {
    expectThrows(expectedThrowable, runnable)
  }

  /** Backport implementation of Assert.expectThrows to be used until JUnit 4.13 is
   *  released. See org.junit.Assert.scala in jUnitRuntime.
   */
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
          val mismatchMessage = "unexpected exception type thrown;" +
            expectedThrowable.getSimpleName + " " + actualThrown.getClass.getSimpleName

          val assertionError = new AssertionError(mismatchMessage)
          assertionError.initCause(actualThrown)
          throw assertionError
        }
    }
  }

  /** Backport implementation of Assert.ThrowingRunnable to be used until
   *  JUnit 4.13 is released. See org.junit.Assert.scala in jUnitRuntime.
   */
  trait ThrowingRunnable {
    def run(): Unit
  }

  def throwingRunnable(code: => Unit): ThrowingRunnable = {
    new ThrowingRunnable {
      def run(): Unit = code
    }
  }
}
