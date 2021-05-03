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

package org.scalajs.testsuite.utils

import scala.util.{Failure, Try, Success}

object AssertThrows {
  /** Backport implementation of Assert.assertThrows to be used until JUnit 4.13 is
   *  released. See org.junit.Assert.scala in jUnitRuntime.
   */
  private def assertThrowsBackport[T <: Throwable](expectedThrowable: Class[T],
      runnable: ThrowingRunnable): T = {
    val result = {
      try {
        runnable.run()
        null.asInstanceOf[T]
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
    if (result == null) {
      throw new AssertionError("expected " + expectedThrowable.getSimpleName +
          " to be thrown, but nothing was thrown")
    } else {
      result
    }
  }

  /** Backport implementation of Assert.ThrowingRunnable to be used until
   *  JUnit 4.13 is released. See org.junit.Assert.scala in jUnitRuntime.
   */
  private trait ThrowingRunnable {
    def run(): Unit
  }

  def assertThrows[T <: Throwable, U](expectedThrowable: Class[T], code: => U): T = {
    assertThrowsBackport(expectedThrowable, new ThrowingRunnable {
      def run(): Unit = code
    })
  }
}
