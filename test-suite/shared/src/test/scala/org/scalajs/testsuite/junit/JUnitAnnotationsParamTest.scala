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

package org.scalajs.testsuite.junit

import org.junit.Test

class JUnitAnnotationsParamTest {
  @Test
  def test0(): Unit = ()

  @Test(expected = classOf[Exception])
  def testException(): Unit =
    throw new Exception("error message")

  @Test(expected = classOf[Exception])
  def testException2(): Unit =
    throw new IndexOutOfBoundsException("error message")

  @Test(timeout = 0L)
  def testTimeOut0(): Unit = ()

  @Test(timeout = 10000L)
  def testTimeOut1(): Unit = ()

  @Test(expected = classOf[Exception], timeout = 10000L)
  def test3(): Unit = throw new Exception
}
