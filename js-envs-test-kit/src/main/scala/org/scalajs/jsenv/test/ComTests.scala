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
    assumeTrue(config.terminateVMJSCode.isDefined)

    val run = kit.start(s"""
      scalajsCom.init(function(msg) { ${config.terminateVMJSCode.get}; });
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

  @Test
  def largeMessageTest: Unit = {
    // 1KB data
    val baseMsg = new String(Array.tabulate(512)(_.toChar))
    val baseLen = baseMsg.length

    // Max message size: 1KB * 2^(2*iters+1) = 1MB
    val iters = 4

    val run = kit.start("""
      scalajsCom.init(function(msg) {
        scalajsCom.send(msg + msg);
      });
    """, RunConfig())

    try {
      run.run.send(baseMsg)

      def resultFactor(iters: Int) = Math.pow(2, 2 * iters + 1).toInt

      for (i <- 0 until iters) {
        val reply = run.waitNextMessage()

        val factor = resultFactor(i)

        assertEquals(baseLen * factor, reply.length)

        for (j <- 0 until factor)
          assertEquals(baseMsg, reply.substring(j * baseLen, (j + 1) * baseLen))

        run.run.send(reply + reply)
      }

      val lastLen = run.waitNextMessage().length
      assertEquals(baseLen * resultFactor(iters), lastLen)
    } finally {
      run.closeAndWait()
    }
  }

  @Test
  def highCharTest: Unit = { // #1536
    val run = kit.start("scalajsCom.init(scalajsCom.send);", RunConfig())

    try {
      val msg = "\uC421\u8F10\u0112\uFF32"
      run.run.send(msg)
      assertEquals(msg, run.waitNextMessage())
    } finally {
      run.closeAndWait()
    }
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
