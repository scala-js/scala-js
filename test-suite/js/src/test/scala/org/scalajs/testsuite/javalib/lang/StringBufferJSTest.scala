/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.lang

import scala.scalajs.js

import org.junit.Test
import org.junit.Assert._

class StringBufferJSTest {

  def newBuf: java.lang.StringBuffer =
    new java.lang.StringBuffer

  @Test def append(): Unit =
    assertEquals("undefined", newBuf.append(js.undefined).toString)

  @Test def insert(): Unit =
    assertEquals("undefined", newBuf.insert(0, js.undefined).toString)
}

class StringBuilderJSTest {

  def newBuilder: java.lang.StringBuilder =
    new java.lang.StringBuilder

  @Test def append(): Unit = {
    assertEquals("undefined", newBuilder.append(js.undefined).toString)
  }

  @Test def insert(): Unit =
    assertEquals("undefined", newBuilder.insert(0, js.undefined).toString)

  @Test def should_allow_string_interpolation_to_survive_null_and_undefined(): Unit =
    assertEquals("undefined", s"${js.undefined}")
}
