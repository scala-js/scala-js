package org.scalajs.jsenv.test

import org.scalajs.jsenv._

import org.junit.{Before, Test}
import org.junit.Assert._
import org.junit.Assume._

import scala.concurrent.duration._

private[test] class TimeoutComTests(config: JSEnvSuiteConfig) {
  private val kit = new TestComKit(config)

  @Before
  def before: Unit = {
    assumeTrue("JSEnv needs timeout support", config.supportsTimeout)
    assumeTrue("JSEnv needs com support", config.supportsCom)
  }

  @Test
  def delayedInitTest: Unit = {
    val deadline = 100.millis.fromNow
    val run = kit.start(s"""
      setTimeout(function() {
        scalajsCom.init(function(msg) {
          scalajsCom.send("Got: " + msg);
        });
      }, 100);
    """, RunConfig())

    try {
      run.run.send("Hello World")
      assertEquals("Got: Hello World", run.waitNextMessage())
      assertTrue("Execution took too little time", deadline.isOverdue())
    } finally {
      run.closeAndWait()
    }
  }

  @Test
  def delayedReplyTest: Unit = {
    val run = kit.start(s"""
      scalajsCom.init(function(msg) {
        setTimeout(scalajsCom.send, 200, "Got: " + msg);
      });
    """, RunConfig())

    try {
      for (i <- 1 to 10) {
        val deadline = 200.millis.fromNow
        run.run.send(s"Hello World: $i")
        assertEquals(s"Got: Hello World: $i", run.waitNextMessage())
        assertTrue("Execution took too little time", deadline.isOverdue())
      }
    } finally {
      run.closeAndWait()
    }
  }

  @Test
  def intervalSendTest: Unit = {
    val deadline = 250.millis.fromNow

    val run = kit.start(s"""
      scalajsCom.init(function(msg) {});
      var sent = 0
      var interval = setInterval(function () {
        scalajsCom.send("Hello");
        sent++;
        if (sent >= 5) clearInterval(interval);
      }, 50);
    """, RunConfig())

    try {
      for (i <- 1 to 5)
        assertEquals("Hello", run.waitNextMessage())

      assertTrue("Execution took too little time", deadline.isOverdue())
    } finally {
      run.closeAndWait()
    }
  }

  @Test
  def noMessageTest: Unit = {
    val run = kit.start(s"""
      // Make sure JVM has already closed when we init
      setTimeout(scalajsCom.init, 1000, function(msg) {});
    """, RunConfig())
    run.closeAndWait()
  }

  @Test // #3411
  def noImmediateCallbackTest: Unit = {
    val run = kit.start(s"""
      setTimeout(function() {
        var gotCalled = false;
        scalajsCom.init(function(msg) { gotCalled = true; });
        if (gotCalled) throw "Buffered messages did not get deferred to the event loop";
      }, 100);
    """, RunConfig())

    try {
      run.run.send("Hello World")
    } finally {
      run.closeAndWait()
    }
  }
}
