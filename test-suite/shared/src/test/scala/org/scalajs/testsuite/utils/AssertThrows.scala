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

import org.junit.Assert
import org.junit.function.ThrowingRunnable

object AssertThrows {
  def assertThrows[T <: Throwable, U](expectedThrowable: Class[T], code: => U): T = {
    Assert.assertThrows(expectedThrowable, new ThrowingRunnable {
      def run(): Unit = code
    })
  }

  def assertThrowsNPEIfCompliant(code: => Unit): Unit = {
    if (Platform.hasCompliantNullPointers)
      assertThrows(classOf[NullPointerException], code)
  }
}
