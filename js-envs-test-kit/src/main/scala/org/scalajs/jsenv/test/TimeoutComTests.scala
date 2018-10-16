/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.jsenv.test

import scala.concurrent.duration._

import org.junit.{Before, Test}
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.jsenv._
import org.scalajs.jsenv.test.kit.TestKit

private[test] class TimeoutComTests(config: JSEnvSuiteConfig) {
  private val kit = new TestKit(config.jsEnv, config.awaitTimeout)

  @Before
  def before: Unit = {
    assumeTrue("JSEnv needs timeout support", config.supportsTimeout)
    assumeTrue("JSEnv needs com support", config.supportsCom)
  }

  /** Slack for timeout tests (see #3457)
   *
   *  Empirically we can observe that timing can be off by ~0.1ms. By cutting
   *  10ms slack, we definitely account for this without compromising the tests.
   */
  private val slack = 10.millis

  @Test
  def delayedInitTest: Unit = {
    val deadline = (100.millis - slack).fromNow
    kit.withComRun("""
      setTimeout(function() {
        scalajsCom.init(function(msg) {
          scalajsCom.send("Got: " + msg);
        });
      }, 100);
    """) { run =>
      run.send("Hello World")
        .expectMsg("Got: Hello World")

      assertTrue("Execution took too little time", deadline.isOverdue())

      run
        .expectNoMsgs()
        .closeRun()
    }
  }

  @Test
  def delayedReplyTest: Unit = {
    kit.withComRun("""
      scalajsCom.init(function(msg) {
        setTimeout(scalajsCom.send, 200, "Got: " + msg);
      });
    """) { run =>
      for (i <- 1 to 10) {
        val deadline = (200.millis - slack).fromNow
        run
          .send(s"Hello World: $i")
          .expectMsg(s"Got: Hello World: $i")

        assertTrue("Execution took too little time", deadline.isOverdue())
      }

      run
        .expectNoMsgs()
        .closeRun()
    }
  }

  @Test
  def intervalSendTest: Unit = {
    val deadline = (250.millis - slack).fromNow

    kit.withComRun("""
      scalajsCom.init(function(msg) {});
      var sent = 0
      var interval = setInterval(function () {
        scalajsCom.send("Hello");
        sent++;
        if (sent >= 5) clearInterval(interval);
      }, 50);
    """) { run =>
      for (i <- 1 to 5)
        run.expectMsg("Hello")

      assertTrue("Execution took too little time", deadline.isOverdue())

      run
        .expectNoMsgs()
        .closeRun()
    }
  }

  @Test
  def noMessageTest: Unit = {
    kit.withComRun(s"""
      // Make sure JVM has already closed when we init
      setTimeout(scalajsCom.init, 1000, function(msg) {});
    """) {
      _.closeRun()
    }
  }

  @Test // #3411
  def noImmediateCallbackTest: Unit = {
    kit.withComRun(s"""
      setTimeout(function() {
        var gotCalled = false;
        scalajsCom.init(function(msg) { gotCalled = true; });
        if (gotCalled) throw "Buffered messages did not get deferred to the event loop";
      }, 100);
    """) {
      _.send("Hello World")
        .closeRun()
    }
  }
}
