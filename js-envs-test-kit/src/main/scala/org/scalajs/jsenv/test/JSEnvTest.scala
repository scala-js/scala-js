package org.scalajs.jsenv.test

import org.scalajs.jsenv._

import org.scalajs.core.tools.io.MemVirtualJSFile
import org.scalajs.core.tools.logging._

import org.junit.Assert._

import StoreLogger._

abstract class JSEnvTest {

  protected def newJSEnv: JSEnv

  implicit class RunMatcher(codeStr: String) {

    val code = new MemVirtualJSFile("testScript.js").withContent(codeStr)

    def hasOutput(expectedOut: String): Unit = {

      val console = new StoreJSConsole()
      val logger  = new StoreLogger()

      newJSEnv.jsRunner(code).run(logger, console)

      val log = logger.getLog
      val hasBadLog = log exists {
        case Log(level, _) if level >= Level.Warn => true
        case Trace(_) => true
        case _ => false
      }

      assertFalse("VM shouldn't log errors, warnings or traces. Log:\n" +
          log.mkString("\n"), hasBadLog)
      assertEquals("Output should match", expectedOut, console.getLog)
    }

    def fails(): Unit = {
      try {
        newJSEnv.jsRunner(code).run(NullLogger, NullJSConsole)
        assertTrue("Code snipped should fail", false)
      } catch {
        case e: Exception =>
      }
    }
  }

}
