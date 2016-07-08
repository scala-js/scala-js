package org.scalajs.core.compiler.test.util

import java.io._
import scala.tools.nsc._

import reporters.{Reporter, ConsoleReporter}

import org.junit.Assert._

import scala.util.matching.Regex

trait TestHelpers extends DirectTest {

  private[this] val errBuffer = new CharArrayWriter

  override def newReporter(settings: Settings): Reporter = {
    val in = new BufferedReader(new StringReader(""))
    val out = new PrintWriter(errBuffer)
    new ConsoleReporter(settings, in, out)
  }

  /** will be prefixed to every code that is compiled. use for imports */
  def preamble: String = ""

  /** pimps a string to compile it and apply the specified test */
  implicit class CompileTests(val code: String) {

    def hasErrors(expected: String): Unit = {
      val reps = repResult {
        assertFalse("snippet shouldn't compile", compileString(preamble + code))
      }
      assertEquals("should have right errors",
          expected.stripMargin.trim, reps.trim)
    }

    def hasWarns(expected: String): Unit = {
      val reps = repResult {
        assertTrue("snippet should compile", compileString(preamble + code))
      }
      assertEquals("should have right warnings",
          expected.stripMargin.trim, reps.trim)
    }

    def containsWarns(expected: String): Unit = {
      val reps = repResult {
        assertTrue("snippet should compile", compileString(preamble + code))
      }
      assertTrue("should contain the right warnings",
          reps.trim.contains(expected.stripMargin.trim))
    }

    def hasNoWarns(): Unit = {
      val reps = repResult {
        assertTrue("snippet should compile", compileString(preamble + code))
      }
      assertTrue("should not have warnings", reps.isEmpty)
    }

    def fails(): Unit =
      assertFalse("snippet shouldn't compile", compileString(preamble + code))

    def warns(): Unit = {
      val reps = repResult {
        assertTrue("snippet should compile", compileString(preamble + code))
      }
      assertFalse("should have warnings", reps.isEmpty)
    }

    def succeeds(): Unit =
      assertTrue("snippet should compile", compileString(preamble + code))

    private def repResult(body: => Unit) = {
      errBuffer.reset()
      body
      errBuffer.toString.replaceAll("\r\n?", "\n")
    }
  }

  implicit class CodeWrappers(sc: StringContext) {
    def expr(): CompileTests =
      new CompileTests(s"class A { ${sc.parts.mkString} }")
  }

}
