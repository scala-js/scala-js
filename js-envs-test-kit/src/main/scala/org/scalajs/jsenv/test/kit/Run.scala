package org.scalajs.jsenv.test.kit

import scala.concurrent.Await
import scala.concurrent.duration.FiniteDuration

import java.nio.charset.{CodingErrorAction, StandardCharsets}

import org.junit.Assert._

import org.scalajs.jsenv._

/** A [[JSRun]] instrumented for testing.
 *
 *  Create an instance of this class through one of the overloads of
 *  `[[TestKit]].withRun` or `[[TestKit]].start`.
 */
class Run private[kit] (run: JSRun, out: IOReader, err: IOReader, timeout: FiniteDuration) extends AutoCloseable {
  private[this] val utf8decoder = {
    StandardCharsets.UTF_8.newDecoder()
      .onMalformedInput(CodingErrorAction.REPLACE)
      .onUnmappableCharacter(CodingErrorAction.REPLACE)
  }

  /** Waits until the given string is output to stdout (in UTF8).
   *
   *  @throws java.lang.AssertionError if there is some other output on stdout
   *      or the run terminates.
   *  @throws java.util.concurrent.TimeoutException if there is not enough output for too long.
   */
  final def expectOut(v: String): this.type = expectIO(out, "stdout", v)

  /** Waits until the given string is output to stderr (in UTF8).
   *
   *  @throws java.lang.AssertionError if there is some other output on stderr
   *      or the run terminates.
   *  @throws java.util.concurrent.TimeoutException if there is not enough output for too long.
   */
  final def expectErr(v: String): this.type = expectIO(err, "stderr", v)

  /** Waits until the underlying [[JSRun]] terminates and asserts it failed.
   *
   *  @throws java.lang.AssertionError if the [[JSRun]] succeeded.
   *  @throws java.util.concurrent.TimeoutException if the [[JSRun]] did not terminate in time.
   */
  final def fails(): Unit = {
    Await.ready(run.future, timeout)
    assertTrue("run succeeded unexpectedly", run.future.value.get.isFailure)
    postStopChecks()
  }

  /** Waits until the underlying [[JSRun]] terminates and asserts it succeeded.
   *
   *  @throws java.lang.AssertionError if the [[JSRun]] failed.
   *  @throws java.util.concurrent.TimeoutException if the [[JSRun]] did not terminate in time.
   */
  final def succeeds(): Unit = {
    try {
      Await.result(run.future, timeout)
    } catch {
      case t: Throwable =>
        throw new AssertionError("run failed unexpectedly", t)
    }
    postStopChecks()
  }

  /** Calls [[JSRun#close]] on the underlying [[JSRun]] and awaits termination.
   *
   *  @throws java.lang.AssertionError if the [[JSRun]] behaves unexpectedly.
   *  @throws java.util.concurrent.TimeoutException if the [[JSRun]] does not terminate in time.
   */
  final def closeRun(): Unit = {
    run.close()
    postCloseRunWait()
    postStopChecks()
  }

  /** Must be called to free all resources of this [[Run]]. Does not throw. */
  def close(): Unit = {
    out.close()
    err.close()
    run.close()
  }

  protected def postCloseRunWait(): Unit = Await.ready(run.future, timeout)

  protected def postStopChecks(): Unit = ()

  private def expectIO(reader: IOReader, name: String, v: String): this.type = {
    val len = v.getBytes(StandardCharsets.UTF_8).length
    val buf = reader.read(len, timeout.fromNow)
    val got = utf8decoder.decode(buf).toString

    assertEquals(s"bad output on $name", v, got)

    this
  }
}
