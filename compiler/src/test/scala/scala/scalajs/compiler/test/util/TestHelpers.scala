package scala.scalajs.compiler.test.util

import java.io._

import scala.tools.nsc._
import reporters.ConsoleReporter

import org.junit.Assert._

trait TestHelpers extends DirectTest {

  private[this] val errBuffer = new CharArrayWriter

  override def newReporter(settings: Settings) = {
    val in = new BufferedReader(new StringReader(""))
    val out = new PrintWriter(errBuffer)
    new ConsoleReporter(settings, in, out)
  }

  /** will be prefixed to every code that is compiled. use for imports */
  def preamble = ""

  implicit class CompileTests(val code: String) {

    def hasErrors(expected: String) = checkRepResult(expected) {
      assertFalse("snippet shouldn't compile", compileString(preamble + code))
    }

    def succeeds() = checkRepResult("") {
      assertTrue("snippet should compile", compileString(preamble + code))
    }

    private def checkRepResult(expected: String)(body: => Unit) = {
      errBuffer.reset()
      body
      val reports = errBuffer.toString
      assertEquals("should have right errors", expected.stripMargin.trim, reports.trim)
    }
  }


}