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
