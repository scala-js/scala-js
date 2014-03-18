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

  implicit class CompileTests(val code: String) {

    def hasErrors(expected: String) = {
      val reps = repResult {
        assertFalse("snippet shouldn't compile", compileString(preamble + code))
      }
      assertEquals("should have right errors", expected.stripMargin.trim, reps.trim)
    }

    def hasErrors(expected: CompatMatcher) = {
      val reps = repResult {
        assertFalse("snippet shouldn't compile", compileString(preamble + code))
      }

      def mLit(
          trg: String,
          lits: Seq[String],
          res: Seq[Regex]): Boolean = lits match {
        case l :: ls =>
          val stripped = l.stripMargin
          println(stripped)
          if (trg.startsWith(stripped))
            mRe(trg.stripPrefix(stripped), ls, res)
          else false
        case Nil => true
      }

      def mRe(
          trg: String,
          lits: Seq[String],
          res: Seq[Regex]): Boolean = res.head.findPrefixOf(trg) map { p =>
        mLit(trg.substring(p.length), lits, res.tail)
      } getOrElse false

      println(reps)

      println(mLit(reps, expected.lits, expected.res))

      assertTrue(s"should have right errors. Should: $expected. Is: $reps",
          mLit(reps, expected.lits, expected.res))

    }

    def succeeds() =
      assertTrue("snippet should compile", compileString(preamble + code))

    private def repResult(body: => Unit) = {
      errBuffer.reset()
      body
      errBuffer.toString
    }
  }

  implicit class ErrCompat(val sc: StringContext) {
    def compat(res: Regex*) = {
      assert(res.size == sc.parts.size - 1)
      CompatMatcher(sc.parts, res)
    }
  }

  case class CompatMatcher(lits: Seq[String], res: Seq[Regex])

}