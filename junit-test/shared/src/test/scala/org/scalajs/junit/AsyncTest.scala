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

package org.scalajs.junit

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import org.junit.Assert._
import org.junit.Test

import org.scalajs.junit.utils._
import org.scalajs.junit.async._

class AsyncTest {
  @Test
  def success(): AsyncResult = {
    val res = Future(1 + 1).filter(_ == 2)
    await(res)
  }

  @Test(expected = classOf[IllegalArgumentException])
  def expectedException(): AsyncResult = {
    // Do not throw synchronously.
    val res = Future.failed(new IllegalArgumentException)
    await(res)
  }

  @Test
  def asyncFailure(): AsyncResult = {
    // Do not throw synchronously.
    val res = Future.failed(new IllegalArgumentException)
    await(res)
  }
}

class AsyncTestAssertions extends JUnitTest
