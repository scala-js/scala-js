package org.scalajs.jsenv.test

import org.scalajs.jsenv._

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.classpath._
import org.scalajs.core.tools.logging._

import org.junit.Test
import org.junit.Assert._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/** A couple of tests that test communication for mix-in into a test suite */
trait AsyncTests {

  protected def newJSEnv: AsyncJSEnv

  protected def emptyCP: CompleteClasspath = PartialClasspath.empty.resolve()

  protected def asyncRunner(code: String): AsyncJSRunner = {
    val codeVF = new MemVirtualJSFile("testScript.js").withContent(code)
    newJSEnv.asyncRunner(emptyCP, codeVF,
        new ScalaConsoleLogger(Level.Warn), ConsoleJSConsole)
  }

  @Test
  def futureTest = {
    val runner = asyncRunner("")
    val fut = runner.start()

    Await.result(fut, Duration.Inf)

    assertFalse("VM should be terminated", runner.isRunning)
  }

}
