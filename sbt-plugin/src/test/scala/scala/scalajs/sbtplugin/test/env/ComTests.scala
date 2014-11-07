package scala.scalajs.sbtplugin.test.env

import scala.scalajs.tools.env._
import scala.scalajs.tools.io._
import scala.scalajs.tools.classpath.PartialClasspath
import scala.scalajs.tools.logging._

import org.junit.Test
import org.junit.Assert._

/** A couple of tests that test communication for mix-in into a test suite */
trait ComTests {

  protected def newJSEnv: ComJSEnv

  private def emptyCP = PartialClasspath.empty.resolve()

  private def comRunner(code: String) = {
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

  @Test
  def comCloseJSTest = {
    val com = comRunner(s"""
      scalajsCom.init(function(msg) {});
      for (var i = 0; i < 10; ++i)
        scalajsCom.send("msg: " + i);
      scalajsCom.close();
    """)

    com.start()

    for (i <- 0 until 10)
      assertEquals(s"msg: $i", com.receive())

    assertThrowClosed("Expect receive to throw after closing of channel",
        com.receive())

    com.await()
  }

  @Test
  def doubleCloseTest = {
    val com = comRunner(s"""
      var seen = 0;
      scalajsCom.init(function(msg) {
        scalajsCom.send("pong");
        if (++seen >= 10)
          scalajsCom.close();
      });
    """)

    com.start()

    for (i <- 0 until 10) {
      com.send("ping")
      assertEquals("pong", com.receive())
    }

    com.close()
    com.await()
  }

  @Test
  def noInitTest = {
    val com = comRunner("")

    com.start()
    com.send("Dummy")
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
