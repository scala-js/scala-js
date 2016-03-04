/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._

class FunctionTest {

  @Test def should_support_call_with_expanded_arguments(): Unit = {
    val f = js.eval("""
        var f = function() { return arguments; }; f;
    """).asInstanceOf[js.Function]

    val res = f.call(null, 42, true).asInstanceOf[js.Dictionary[Any]]
    assertEquals(42, res("0"))
    assertEquals(true, res("1"))
    assertFalse(res.contains("2"))
  }

  @Test def `should_support_call_with_the_:_*_notation_to_expand_a_Seq`(): Unit = {
    val f = js.eval("""
        var f = function() { return arguments; }; f;
    """).asInstanceOf[js.Function]

    val args = Seq[js.Any](42, true)
    val res = f.call(null, args: _*).asInstanceOf[js.Dictionary[Any]]
    assertEquals(42, res("0"))
    assertEquals(true, res("1"))
    assertFalse(res.contains("2"))
  }

}
