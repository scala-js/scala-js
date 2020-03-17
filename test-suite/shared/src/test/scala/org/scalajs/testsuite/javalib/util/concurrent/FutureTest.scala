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

package org.scalajs.testsuite.javalib.util.function

import java.util.concurrent._

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._

class FutureTest {

  @Test def allMembers(): Unit = {
    assertTrue(FutureTest.cancel(true))
    assertTrue(FutureTest.get() == 0)
    assertTrue(FutureTest.get(0L, null.asInstanceOf[TimeUnit]) == 0)
    assertTrue(FutureTest.isCancelled())
    assertTrue(FutureTest.isDone())
  }

}

object FutureTest extends Future[Int] {
  def cancel(mayInterruptIfRunning: Boolean): Boolean = true
  def get(): Int = 0
  def get(timeout: Long, unit: TimeUnit): Int = 0
  def isCancelled(): Boolean = true
  def isDone(): Boolean = true
}
