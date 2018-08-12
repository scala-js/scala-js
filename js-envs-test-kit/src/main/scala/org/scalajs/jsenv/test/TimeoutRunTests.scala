package org.scalajs.jsenv.test

import scala.concurrent.duration._

import org.junit.{Before, Test}
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.jsenv._
import org.scalajs.jsenv.test.kit.{TestKit, Run}

private[test] class TimeoutRunTests(config: JSEnvSuiteConfig, withCom: Boolean) {
  private val kit = new TestKit(config.jsEnv, config.awaitTimeout)

  private def withRun(input: String)(body: Run => Unit) = {
    if (withCom) kit.withComRun(input)(body)
    else kit.withRun(input)(body)
  }

  @Before
  def before: Unit = {
    assumeTrue("JSEnv needs timeout support", config.supportsTimeout)
  }

  /** Slack for timeout tests (see #3457)
   *
   *  Empirically we can observe that timing can be off by ~0.1ms. By cutting
   *  10ms slack, we definitely account for this without compromising the tests.
   */
  private val slack = 10.millis

  @Test
  def basicTimeoutTest: Unit = {

    val deadline = (300.millis - slack).fromNow

    withRun("""
      setTimeout(function() { console.log("1"); }, 200);
      setTimeout(function() { console.log("2"); }, 100);
      setTimeout(function() { console.log("3"); }, 300);
      setTimeout(function() { console.log("4"); },   0);
    """) {
      _.expectOut("4\n")
        .expectOut("2\n")
        .expectOut("1\n")
        .expectOut("3\n")
        .closeRun()
    }

    assertTrue("Execution took too little time", deadline.isOverdue())
  }

  @Test
  def intervalTest: Unit = {
    val deadline = (100.millis - slack).fromNow

    withRun("""
      setInterval(function() { console.log("tick"); }, 20);
    """) {
      _.expectOut("tick\n")
        .expectOut("tick\n")
        .expectOut("tick\n")
        .expectOut("tick\n")
        .expectOut("tick\n")
        .closeRun() // Terminate after 5 iterations
    }

     assertTrue("Execution took too little time", deadline.isOverdue())
  }
}
