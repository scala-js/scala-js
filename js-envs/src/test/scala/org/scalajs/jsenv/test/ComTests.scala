package org.scalajs.jsenv.test

import org.scalajs.jsenv._

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.logging._

import org.junit.Test
import org.junit.Assert._

/** A couple of tests that test communication for mix-in into a test suite */
trait ComTests extends AsyncTests {

  protected def newJSEnv: ComJSEnv

  protected def comRunner(code: String): ComJSRunner = {
    val codeVF = new MemVirtualJSFile("testScript.js").withContent(code)
    newJSEnv.comRunner(emptyCP, codeVF,
        new ScalaConsoleLogger(Level.Warn), ConsoleJSConsole)
  }

  private def assertThrowClosed(msg: String, body: => Unit): Unit = {
    val thrown = try {
      body
      false
    } catch {
      case _: ComJSEnv.ComClosedException =>
        true
    }

    assertTrue(msg, thrown)
  }

  @Test
  def comCloseJVMTest = {
    val com = comRunner(s"""
      scalajsCom.init(function(msg) { scalajsCom.send("received: " + msg); });
      scalajsCom.send("Hello World");
    """)

    com.start()

    assertEquals("Hello World", com.receive())

    for (i <- 0 to 10) {
      com.send(i.toString)
      assertEquals(s"received: $i", com.receive())
    }

    com.close()
    com.await()
  }

  def comCloseJSTestCommon(timeout: Long) = {
    val com = comRunner(s"""
      scalajsCom.init(function(msg) {});
      for (var i = 0; i < 10; ++i)
        scalajsCom.send("msg: " + i);
      scalajsCom.close();
    """)

    com.start()

    Thread.sleep(timeout)

    for (i <- 0 until 10)
      assertEquals(s"msg: $i", com.receive())

    assertThrowClosed("Expect receive to throw after closing of channel",
        com.receive())

    com.close()
    com.await()
  }

  @Test
  def comCloseJSTest = comCloseJSTestCommon(0)

  @Test
  def comCloseJSTestDelayed = comCloseJSTestCommon(1000)

  @Test
  def doubleCloseTest = {
    val n = 10
    val com = pingPongRunner(n)

    com.start()

    for (i <- 0 until n) {
      com.send("ping")
      assertEquals("pong", com.receive())
    }

    com.close()
    com.await()
  }

  @Test
  def multiEnvTest = {
    val n = 10
    val envs = List.fill(5)(pingPongRunner(10))

    envs.foreach(_.start())

    val ops = List[ComJSRunner => Unit](
        _.send("ping"),
        com => assertEquals("pong", com.receive())
    )

    for {
      i   <- 0 until n
      env <- envs
      op  <- ops
    } op(env)

    envs.foreach(_.close())
    envs.foreach(_.await())
  }

  private def pingPongRunner(count: Int) = {
    comRunner(s"""
      var seen = 0;
      scalajsCom.init(function(msg) {
        scalajsCom.send("pong");
        if (++seen >= $count)
          scalajsCom.close();
      });
    """)
  }

  @Test
  def largeMessageTest = {
    // 1KB data
    val baseMsg = new String(Array.tabulate(512)(_.toChar))
    val baseLen = baseMsg.length

    // Max message size: 1KB * 2^(2*iters+1) = 1MB
    val iters = 4

    val com = comRunner("""
      scalajsCom.init(function(msg) {
        scalajsCom.send(msg + msg);
      });
    """)

    com.start()

    com.send(baseMsg)

    def resultFactor(iters: Int) = Math.pow(2, 2 * iters + 1).toInt

    for (i <- 0 until iters) {
      val reply = com.receive()

      val factor = resultFactor(i)

      assertEquals(baseLen * factor, reply.length)

      for (j <- 0 until factor)
        assertEquals(baseMsg, reply.substring(j * baseLen, (j + 1) * baseLen))

      com.send(reply + reply)
    }

    val lastLen = com.receive().length
    assertEquals(baseLen * resultFactor(iters), lastLen)

    com.close()
    com.await()
  }

  @Test
  def noInitTest = {
    val com = comRunner("")

    com.start()
    com.send("Dummy")
    com.close()
    com.await()
  }

  @Test
  def stopTest = {
    val com = comRunner(s"""scalajsCom.init(function(msg) {});""")

    com.start()

    // Make sure the VM doesn't terminate.
    Thread.sleep(1000)

    assertTrue("VM should still be running", com.isRunning)

    // Stop VM instead of closing channel
    com.stop()

    try {
      com.await()
      fail("Stopped VM should be in failure state")
    } catch {
      case _: Throwable =>
    }
  }

}
