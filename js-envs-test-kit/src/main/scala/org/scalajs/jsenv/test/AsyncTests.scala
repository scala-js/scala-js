package org.scalajs.jsenv.test

import org.scalajs.jsenv._

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.logging._

import org.junit.Test
import org.junit.Assert._

import scala.concurrent.{Future, Await}
import scala.concurrent.duration._

/** A couple of tests that test async runners for mix-in into a test suite */
trait AsyncTests extends BasicJSEnvTests {

  protected final val DefaultTimeout: Duration = 10.seconds

  protected def newJSEnv: AsyncJSEnv

  protected def asyncRunner(code: String): AsyncJSRunner = {
    val codeVF = new MemVirtualJSFile("testScript.js").withContent(code)
    newJSEnv.asyncRunner(codeVF)
  }

  protected def start(runner: AsyncJSRunner): Future[Unit] = {
    runner.start(new ScalaConsoleLogger(Level.Warn), ConsoleJSConsole)
  }

  @Test
  def futureTest: Unit = {
    val runner = asyncRunner("")
    val fut = start(runner)

    Await.result(fut, DefaultTimeout)

    assertFalse("VM should be terminated", runner.isRunning)
  }

  @Test
  def stopAfterTerminatedTest: Unit = {
    val runner = asyncRunner("")
    val fut = start(runner)

    Await.result(fut, DefaultTimeout)

    runner.stop() // should do nothing, and not fail
  }

}
