package org.scalajs.jsenv.test

import org.scalajs.jsenv._

import org.junit.{Before, Test}
import org.junit.Assert._
import org.junit.Assume._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global

private[test] class ComTests(config: JSEnvSuiteConfig) {
  private val kit = new TestComKit(config)

  @Before
  def before: Unit = {
    assumeTrue("JSEnv needs com support", config.supportsCom)
  }

  @Test
  def basicTest: Unit = {
    val run = kit.start("""
      scalajsCom.init(function(msg) { scalajsCom.send("received: " + msg); });
      scalajsCom.send("Hello World");
    """, RunConfig())

    try {
      assertEquals("Hello World", run.waitNextMessage())

      for (i <- 0 to 10) {
        run.run.send(i.toString)
        assertEquals(s"received: $i", run.waitNextMessage())
      }
    } finally {
      run.closeAndWait()
    }
  }

  @Test
  def jsExitsOnMessageTest: Unit = {
    assumeTrue(config.supportsExit)

    val run = kit.start("""
      scalajsCom.init(function(msg) { __ScalaJSEnv.exitFunction(0); });
      for (var i = 0; i < 10; ++i)
        scalajsCom.send("msg: " + i);
      """, RunConfig())

    try {
      for (i <- 0 until 10)
        assertEquals(s"msg: $i", run.waitNextMessage())

      run.run.send("quit")

      Await.result(run.run.future, config.awaitTimeout)
    } finally {
      run.run.close()
    }
  }

  @Test
  def multiEnvTest: Unit = {
    val n = 10
    val runs = List.fill(5) {
      kit.start("""
      scalajsCom.init(function(msg) {
        scalajsCom.send("pong");
      });
      """, RunConfig())
    }

    try {
      for (_ <- 0 until n) {
        runs.foreach(_.run.send("ping"))
        runs.foreach(r => assertEquals("pong", r.waitNextMessage()))
      }
    } finally {
      runs.foreach(_.closeAndWait())
    }
  }

  private def replyTest(msg: String) = {
    val run = kit.start("scalajsCom.init(scalajsCom.send);", RunConfig())

    try {
      run.run.send(msg)
      assertEquals(msg, run.waitNextMessage())
    } finally {
      run.closeAndWait()
    }
  }

  @Test
  def largeMessageTest: Unit = {
    // 1MB data
    replyTest(new String(Array.tabulate(1024 * 1024)(_.toChar)))
  }

  @Test
  def highCharTest: Unit = { // #1536
    replyTest("\uC421\u8F10\u0112\uFF32")
  }

  @Test
  def noInitTest: Unit = {
    val run = kit.start("", RunConfig())
    try {
      run.run.send("Dummy")
    } finally {
      run.closeAndWait()
    }
  }
}
