package org.scalajs.jsenv.test.kit

import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration

import org.junit.Assert._

import org.scalajs.jsenv._

/** A [[JSComRun]] instrumented for testing.
 *
 *  Create an instance of this class through one of the overloads of
 *  `[[TestKit]].withComRun` or `[[TestKit]].startWithCom`.
 */
class ComRun private[kit] (run: JSComRun, out: IOReader, err: IOReader,
    msgs: MsgHandler, timeout: FiniteDuration)
    extends Run(run, out, err, timeout) {
  private[this] var noMessages = false

  /** Calls [[JSComRun#send]] on the underlying run. */
  final def send(msg: String): this.type = {
    run.send(msg)
    this
  }

  /** Waits until the given message is sent to the JVM.
   *
   *  @throws java.lang.AssertionError if there is another message or the run terminates.
   *  @throws java.util.concurrent.TimeoutException if there is no message for too long.
   */
  final def expectMsg(expected: String): this.type = {
    require(!noMessages, "You may not call expectMsg after calling expectNoMsgs")
    val actual = msgs.waitOnMessage(timeout.fromNow)
    assertEquals("got bad message", expected, actual)
    this
  }

  /** Marks that no further messages are expected.
   *
   *  This will make the methods [[closeRun]] / [[fails]] / [[succeeds]] fail if
   *  further messages are received.
   *
   *  @note It is illegal to call [[expectMsg]] after [[expectNoMsgs]] has been
   *      called.
   */
  final def expectNoMsgs(): this.type = {
    noMessages = true
    this
  }

  override protected def postCloseRunWait(): Unit = {
    try {
      Await.result(run.future, timeout)
    } catch {
      case t: Throwable =>
        throw new AssertionError("closing a ComRun failed unexpectedly", t)
    }
  }

  override protected def postStopChecks(): Unit = {
    super.postStopChecks()
    if (noMessages) {
      val rem = msgs.remainingMessages()
      assertTrue(s"unhandled messages: $rem", rem.isEmpty)
    }
  }
}
