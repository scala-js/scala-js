/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Test

import java.util.Formatter

class FormatterJSTest {

  @Test def `should_survive_undefined`(): Unit = {
    val fmt = new Formatter()
    val res = fmt.format("%s", js.undefined).toString()
    fmt.close()
    assertEquals("undefined", res)
  }

  @Test def `should_allow_f_string_interpolation_to_survive_undefined`(): Unit = {
    assertEquals("undefined", f"${js.undefined}%s")
  }
}
