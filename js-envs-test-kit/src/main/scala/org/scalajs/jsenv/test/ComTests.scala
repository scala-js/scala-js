package org.scalajs.jsenv.test

import org.junit.{Before, Test}
import org.junit.Assume._

import org.scalajs.jsenv._
import org.scalajs.jsenv.test.kit.TestKit

private[test] class ComTests(config: JSEnvSuiteConfig) {
  private val kit = new TestKit(config.jsEnv, config.awaitTimeout)

  @Before
  def before: Unit = {
    assumeTrue("JSEnv needs com support", config.supportsCom)
  }

  @Test
  def basicTest: Unit = {
    kit.withComRun("""
      scalajsCom.init(function(msg) { scalajsCom.send("received: " + msg); });
      scalajsCom.send("Hello World");
    """) { run =>

      run.expectMsg("Hello World")

      for (i <- 0 to 10) {
        run
          .send(i.toString)
          .expectMsg(s"received: $i")
      }

      run.expectNoMsgs()
        .closeRun()
    }
  }

  @Test
  def jsExitsOnMessageTest: Unit = {
    assumeTrue(config.supportsExit)

    kit.withComRun("""
      scalajsCom.init(function(msg) { __ScalaJSEnv.exitFunction(0); });
      for (var i = 0; i < 10; ++i)
        scalajsCom.send("msg: " + i);
      """) { run =>

      for (i <- 0 until 10)
        run.expectMsg(s"msg: $i")

      run
        .send("quit")
        .expectNoMsgs()
        .succeeds()
    }
  }

  @Test
  def multiEnvTest: Unit = {
    val n = 10
    val runs = List.fill(5) {
      kit.startWithCom("""
      scalajsCom.init(function(msg) {
        scalajsCom.send("pong");
      });
      """)
    }

    try {
      for (_ <- 0 until n) {
        runs.foreach(_.send("ping"))
        runs.foreach(_.expectMsg("pong"))
      }

      runs.foreach {
        _.expectNoMsgs()
          .closeRun()
      }
    } finally {
      runs.foreach(_.close())
    }
  }

  private def replyTest(msg: String) = {
    kit.withComRun("scalajsCom.init(scalajsCom.send);") {
      _.send(msg)
        .expectMsg(msg)
        .expectNoMsgs()
        .closeRun()
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
    kit.withComRun("") {
      _.send("Dummy")
        .expectNoMsgs()
        .closeRun()
    }
  }

  @Test
  def separateComStdoutTest: Unit = {
    // Make sure that com and stdout do not interfere with each other.
    kit.withComRun("""
      scalajsCom.init(function (msg) {
        console.log("got: " + msg)
      });
      console.log("a");
      scalajsCom.send("b");
      scalajsCom.send("c");
      console.log("d");
    """) {
      _.expectOut("a\n")
        .expectMsg("b")
        .expectMsg("c")
        .expectOut("d\n")
        .send("foo")
        .expectOut("got: foo\n")
        .closeRun()
    }
  }
}
