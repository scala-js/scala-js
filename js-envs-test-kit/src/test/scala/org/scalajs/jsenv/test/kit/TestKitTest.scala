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

package org.scalajs.jsenv.test.kit

import scala.concurrent.duration._

import java.util.concurrent._

import org.junit.Assert._
import org.junit.Test

import org.scalajs.jsenv._

class TestKitTest {
  import TestKit.codeToInput
  import TestKitTest._

  private def noHangTest(env: TestEnv, msg: String)(body: TestKit => Unit) = {
    def test(e: JSEnv, cause: Throwable) = {
      val timeout = 1.minute
      val kit = new TestKit(e, timeout)
      val deadline = timeout.fromNow

      expectAssert(msg, cause)(body(kit))

      assertFalse("faster than timout", deadline.isOverdue)
    }

    test(env.withSuccess(), null)

    val t = new Throwable
    test(env.withFailure(t), t)
  }

  @Test
  def noHangExpectOutNoStream: Unit = {
    noHangTest(TestEnv(), "run completed and did not call onOutputStream") {
      _.withRun("") {
        _.expectOut("a")
          .closeRun()
      }
    }
  }
  
  @Test
  def noHangExpectErrNoStream: Unit = {
    noHangTest(TestEnv(),  "run completed and did not call onOutputStream") {
      _.withRun("") {
        _.expectErr("a")
          .closeRun()
      }
    }
  }

  @Test
  def noHangExpectMsgOnFail: Unit = {
    noHangTest(TestEnv(),  "no messages left and run has completed") {
      _.withComRun("") {
        _.expectMsg("a")
          .closeRun()
      }
    }
  }

  @Test
  def noHangExpectOutOnEOF: Unit = {
    noHangTest(TestEnv().withOutErr(""), "reached end of stream") {
      _.withRun("") {
        _.expectOut("a")
          .closeRun()
      }
    }
  }
  
  @Test
  def noHangExpectErrOnEOF: Unit = {
    noHangTest(TestEnv().withOutErr(""), "reached end of stream") {
      _.withRun("") {
        _.expectErr("a")
          .closeRun()
      }
    }
  }

  @Test
  def failOnUnexpectedSuccess: Unit = {
    val kit = new TestKit(TestEnv().withSuccess(), 1.second)
    expectAssert("run succeeded unexpectedly") {
      kit.withRun("")(_.fails())
    }
  }

  @Test
  def failOnUnexpectedFailure: Unit = {
    val t = new Throwable
    val kit = new TestKit(TestEnv().withFailure(t), 1.second)

    expectAssert("run failed unexpectedly", t) {
      kit.withRun("")(_.succeeds())
    }
  }

  @Test
  def ignoreRunFailOnClose: Unit = {
    val kit = new TestKit(TestEnv().withFailure(new Throwable("dummy for test")), 1.second)
    kit.withRun("")(_.closeRun())
  }

  @Test
  def enforceSuccessComRunOnClose: Unit = {
    val t = new Throwable
    val kit = new TestKit(TestEnv().withFailure(t), 1.second)

    expectAssert("closing a ComRun failed unexpectedly", t) {
      kit.withComRun("")(_.closeRun())
    }
  }

  @Test
  def failOnBadOut: Unit = {
    val kit = new TestKit(TestEnv().withOutErr("a"), 1.second)

    expectAssert("bad output on stdout expected:<[b]> but was:<[a]>") {
      kit.withRun("") {
        _.expectOut("b")
          .closeRun()
      }
    }
  }

  @Test
  def failOnBadErr: Unit = {
    val kit = new TestKit(TestEnv().withOutErr("a"), 1.second)

    expectAssert("bad output on stderr expected:<[b]> but was:<[a]>") {
      kit.withRun("") {
        _.expectErr("b")
          .closeRun()
      }
    }
  }

  @Test
  def ignoreExcessOut: Unit = {
    val kit = new TestKit(TestEnv().withOutErr("abcdefg"), 1.second)

    kit.withRun("") {
      _.expectOut("a")
        .expectOut("b")
        .closeRun()
    }
  }

  @Test
  def ignoreExcessErr: Unit = {
    val kit = new TestKit(TestEnv().withOutErr("abcdefg"), 1.second)

    kit.withRun("") {
      _.expectErr("a")
        .expectErr("b")
        .closeRun()
    }
  }

  @Test
  def failOnBadMsgErr: Unit = {
    val kit = new TestKit(TestEnv().withMsgs("a"), 1.second)

    expectAssert("got bad message expected:<[b]> but was:<[a]>") {
      kit.withComRun("") {
        _.expectMsg("b")
          .closeRun()
      }
    }
  }

  @Test
  def failOnExcessMsgs: Unit = {
    val kit = new TestKit(TestEnv().withMsgs("a", "b", "c"), 1.second)

    expectAssert("unhandled messages: List(b, c)") {
      kit.withComRun("") {
        _.expectMsg("a")
          .expectNoMsgs()
          .closeRun()
      }
    }
  }

  @Test
  def ignoreExcessMsgs: Unit = {
    val kit = new TestKit(TestEnv().withMsgs("a", "b", "c"), 1.second)

    kit.withComRun("") {
      _.expectMsg("a")
        .closeRun()
    }
  }

  @Test
  def timeoutOutOnNoStream: Unit = {
    val kit = new TestKit(TestEnv().withHang(), 10.millisecond)

    expectTimeout("timed out waiting on run to call onOutputStream") {
      kit.withRun("") {
        _.expectOut("b")
          .closeRun()
      }
    }
  }

  @Test
  def timeoutErrOnNoStream: Unit = {
    val kit = new TestKit(TestEnv().withHang(), 10.millisecond)

    expectTimeout("timed out waiting on run to call onOutputStream") {
      kit.withRun("") {
        _.expectErr("b")
          .closeRun()
      }
    }
  }

  @Test
  def timeoutExpectMsg: Unit = {
    val kit = new TestKit(TestEnv().withHang(), 10.millisecond)

    expectTimeout("timed out waiting for next message") {
      kit.withComRun("") {
        _.expectMsg("a")
          .closeRun()
      }
    }
  }

  @Test
  def timeoutExpectOut: Unit = {
    val kit = new TestKit(TestEnv().withOutErrHang(), 10.millisecond)

    expectTimeout("timed out reading from stream") {
      kit.withRun("") {
        _.expectOut("b")
          .closeRun()
      }
    }
  }

  @Test
  def timeoutExpectErr: Unit = {
    val kit = new TestKit(TestEnv().withOutErrHang(), 10.millisecond)

    expectTimeout("timed out reading from stream") {
      kit.withRun("") {
        _.expectErr("b")
          .closeRun()
      }
    }
  }
}

private object TestKitTest {
  def expectAssert(msg: String, cause: Throwable = null)(body: => Unit): Unit = {
    val thrown = try {
      body
      false
    } catch {
      case e: AssertionError =>
        assertEquals("bad assertion error message", msg, e.getMessage())
        assertSame("should link cause", cause, e.getCause())
        true
    }

    if (!thrown)
      throw new AssertionError("expected AssertionError to be thrown")
  }

  def expectTimeout(msg: String)(body: => Unit): Unit = {
    try {
      body
      throw new AssertionError("expected TimeoutExeception to be thrown")
    } catch {
      case e: TimeoutException =>
        assertEquals("bad timeout error message", msg, e.getMessage())
    }
  }
}
