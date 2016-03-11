package org.scalajs.testsuite.utils

import scala.scalajs.js

import org.junit.Assert._

object JSAssert {

  def assertJSArrayEquals[T](expected: js.Array[T], actual: js.Array[T]): Unit = {
    if (expected.length != actual.length)
      fail("js.Array lengths differed, expected.length=" + expected.length + " actual.length=" + actual.length)

    for (i <- expected.indices) {
      val exp = expected(i)
      val act = actual(i)
      if (exp != act)
        fail(s"js.Arrays first differed at element [$i]; expected:<$exp> but was:<$act>")
    }
  }

  def assertJSUndefined(obj: Any): Unit = {
    assertTrue(s"Expected <$obj> to be <undefined>.", js.isUndefined(obj))
  }

  def assertJSNotUndefined(obj: Any): Unit = {
    assertFalse(s"Expected <$obj> not to be <undefined>.",js.isUndefined(obj))
  }
}
