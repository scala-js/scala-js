package scala.scalajs.compiler.test.util

import java.io._
import scala.tools.nsc._

import reporters.ConsoleReporter

import org.junit.Assert._

import scala.util.matching.Regex

trait TestHelpers extends DirectTest {

  private[this] val errBuffer = new CharArrayWriter

  override def newReporter(settings: Settings) = {
    val in = new BufferedReader(new StringReader(""))
    val out = new PrintWriter(errBuffer)
    new ConsoleReporter(settings, in, out)
  }

  /** will be prefixed to every code that is compiled. use for imports */
  def preamble = ""

  /** pimps a string to compile it and apply the specified test */
  implicit class CompileTests(val code: String) {

    def hasErrors(expected: String) = {
      val reps = repResult {
        assertFalse("snippet shouldn't compile", compileString(preamble + code))
      }
      assertEquals("should have right errors",
          expected.stripMargin.trim, reps.trim)
    }

    def fails() =
      assertFalse("snippet shouldn't compile", compileString(preamble + code))

    def succeeds() =
      assertTrue("snippet should compile", compileString(preamble + code))

    private def repResult(body: => Unit) = {
      errBuffer.reset()
      body
      errBuffer.toString
    }
  }

}
