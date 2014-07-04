package scala.scalajs.sbtplugin.test.env

import scala.scalajs.tools.env.JSEnv
import scala.scalajs.tools.io.MemVirtualJSFile
import scala.scalajs.tools.classpath.PartialClasspath
import scala.scalajs.tools.logging.NullLogger
import scala.scalajs.tools.env.NullJSConsole

import org.junit.Assert._

abstract class JSEnvTest {

  protected def newJSEnv: JSEnv

  implicit class RunMatcher(codeStr: String) {

    val emptyCP = PartialClasspath.empty.resolve()
    val code    = new MemVirtualJSFile("testScript.js").withContent(codeStr)

    def hasOutput(expectedOut: String): Unit = {

      val console = new StoreJSConsole()
      val logger  = new StoreLogger()

      newJSEnv.runJS(emptyCP, code, logger, console)

      val log = logger.getLog

      assertTrue("VM shouldn't produce log. Log:\n" +
          log.mkString("\n"), log.isEmpty)
      assertEquals("Output should match", expectedOut, console.getLog)
    }

    def fails(): Unit = {
      try {
        newJSEnv.runJS(emptyCP, code, NullLogger, NullJSConsole)
        assertTrue("Code snipped should fail", false)
      } catch {
        case e: Exception =>
      }
    }
  }

}
