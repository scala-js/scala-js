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

package org.scalajs.nscplugin.test.util

import java.io._

import scala.tools.nsc._
import scala.tools.nsc.reporters.ConsoleReporter

import org.junit.Assert._

trait TestHelpers extends DirectTest {

  private[this] val errBuffer = new CharArrayWriter

  override def newReporter(settings: Settings): ConsoleReporter = {
    val in = new BufferedReader(new StringReader(""))
    val out = new PrintWriter(errBuffer)
    new ConsoleReporter(settings, in, out)
  }

  /** will be prefixed to every code that is compiled. use for imports */
  def preamble: String = ""

  /** pimps a string to compile it and apply the specified test */
  implicit class CompileTests(val code: String) {
    private lazy val (success, output) = {
      errBuffer.reset()
      val success = compileString(preamble + code)
      val output = errBuffer.toString.replaceAll("\r\n?", "\n").trim
      (success, output)
    }

    def hasErrors(expected: String): Unit = {
      assertFalse("snippet shouldn't compile", success)
      assertEquals("should have right errors", expected.stripMargin.trim, output)
    }

    def hasWarns(expected: String): Unit = {
      assertTrue("snippet should compile\n" + output, success)
      assertEquals("should have right warnings", expected.stripMargin.trim, output)
    }

    def containsWarns(expected: String): Unit = {
      assertTrue("snippet should compile\n" + output, success)
      assertTrue("should contain the right warnings", output.contains(expected.stripMargin.trim))
    }

    def hasNoWarns(): Unit = {
      assertTrue("snippet should compile\n" + output, success)
      assertTrue("should not have warnings\n" + output, output.isEmpty)
    }

    def fails(): Unit =
      assertFalse("snippet shouldn't compile", success)

    def warns(): Unit = {
      assertTrue("snippet should compile\n" + output, success)
      assertFalse("should have warnings", output.isEmpty)
    }

    def succeeds(): Unit =
      assertTrue("snippet should compile\n" + output, success)
  }

  implicit class CodeWrappers(sc: StringContext) {
    def expr(): CompileTests =
      new CompileTests(s"class A { ${sc.parts.mkString} }")
  }

}
