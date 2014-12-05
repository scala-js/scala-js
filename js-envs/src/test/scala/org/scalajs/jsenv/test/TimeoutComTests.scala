package org.scalajs.jsenv.test

import org.junit.Test
import org.junit.Assert._

import scala.concurrent.duration._

trait TimeoutComTests extends TimeoutTests with ComTests {

  @Test
  def delayedInitTest = {

    val com = comRunner(s"""
      setTimeout(function() {
        scalajsCom.init(function(msg) {
          scalajsCom.send("Got: " + msg);
        });
      }, 100);
    """)

    val deadline = 100.millis.fromNow

    com.start()

    com.send("Hello World")

    assertEquals("Got: Hello World", com.receive())

    assertTrue("Execution took too little time", deadline.isOverdue())

    com.close()
    com.await(DefaultTimeout)

  }

  @Test
  def delayedReplyTest = {

    val com = comRunner(s"""
      scalajsCom.init(function(msg) {
        setTimeout(scalajsCom.send, 20, "Got: " + msg);
      });
    """)

    com.start()

    for (i <- 1 to 10) {
      val deadline = 19.millis.fromNow // give some slack
      com.send(s"Hello World: $i")
      assertEquals(s"Got: Hello World: $i", com.receive())
      assertTrue("Execution took too little time", deadline.isOverdue())
    }

    com.close()
    com.await(DefaultTimeout)

  }

  @Test
  def intervalSendTest = {

    val com = comRunner(s"""
      scalajsCom.init(function(msg) {});
      var interval = setInterval(scalajsCom.send, 50, "Hello");
      setTimeout(clearInterval, 295, interval);
    """)

    val deadline = 245.millis.fromNow

    com.start()

    for (i <- 1 to 5)
      assertEquals("Hello", com.receive())

    com.close()
    com.await(DefaultTimeout)

    assertTrue("Execution took too little time", deadline.isOverdue())

  }

  @Test
  def stopTestTimeout = {

    val async = asyncRunner(s"""
      setInterval(function() {}, 0);
    """)

    async.start()
    async.stop()

    try {
      async.await(DefaultTimeout)
      fail("Expected await to fail")
    } catch {
      case t: Throwable => // all is well
    }

  }

}
