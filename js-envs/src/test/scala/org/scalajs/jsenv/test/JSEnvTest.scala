package org.scalajs.jsenv.test

import org.scalajs.jsenv._

import org.scalajs.core.tools.io.MemVirtualJSFile
import org.scalajs.core.tools.classpath.PartialClasspath
import org.scalajs.core.tools.logging.NullLogger

import org.junit.Assert._

abstract class JSEnvTest {

  protected def newJSEnv: JSEnv

  implicit class RunMatcher(codeStr: String) {

    val emptyCP = PartialClasspath.empty.resolve()
    val code    = new MemVirtualJSFile("testScript.js").withContent(codeStr)

    def hasOutput(expectedOut: String): Unit = {

      val console = new StoreJSConsole()
      val logger  = new StoreLogger()

      newJSEnv.jsRunner(emptyCP, code, logger, console).run()

      val log = logger.getLog

      assertTrue("VM shouldn't produce log. Log:\n" +
          log.mkString("\n"), log.isEmpty)
      assertEquals("Output should match", expectedOut, console.getLog)
    }

    def fails(): Unit = {
      try {
        newJSEnv.jsRunner(emptyCP, code, NullLogger, NullJSConsole).run()
        assertTrue("Code snipped should fail", false)
      } catch {
        case e: Exception =>
      }
    }
  }

}
