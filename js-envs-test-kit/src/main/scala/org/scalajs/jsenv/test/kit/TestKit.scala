package org.scalajs.jsenv.test.kit

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

import java.io.InputStream
import java.util.concurrent.Executors

import org.scalajs.io.MemVirtualBinaryFile
import org.scalajs.jsenv._

/** TestKit is a utility class to simplify testing of [[JSEnv]]s.
 *
 *  It is mostly used by Scala.js' provided [[JSEnv]] test suite but it may be
 *  used for additional tests specific to a particular [[JSEnv]].
 *
 *  @example
 *  {{{
 *  import scala.concurrent.duration._
 *
 *  val kit = new TestKit(new MyEnv, 1.second)
 *  kit.withRun("""console.log("Hello World");""") {
 *    _.expectOut("Hello World\n")
 *      .closeRun()
 *  }
 *  }}}
 *
 *  @note Methods in [[TestKit]] allow to take a string instead of an [[Input]].
 *      The string is converted into an input form supported by the [[JSEnv]] to
 *      execute the code therein.
 *
 *  @constructor Create a new [[TestKit]] for the given [[JSEnv]] and timeout.
 *  @param jsEnv The [[JSEnv]] to be tested.
 *  @param timeout Timeout for all `expect*` methods on [[Run]] / [[ComRun]].
 */
final class TestKit(jsEnv: JSEnv, timeout: FiniteDuration) {
  import TestKit.codeToInput

  /** Starts a [[Run]] for testing. */
  def start(code: String): Run =
    start(codeToInput(code))

  /** Starts a [[Run]] for testing. */
  def start(input: Input): Run =
    start(input, RunConfig())

  /** Starts a [[Run]] for testing. */
  def start(code: String, config: RunConfig): Run =
    start(codeToInput(code), config)

  /** Starts a [[Run]] for testing. */
  def start(input: Input, config: RunConfig): Run = {
    val (run, out, err) = io(config)(jsEnv.start(input, _))
    new Run(run, out, err, timeout)
  }

  /** Starts a [[ComRun]] for testing. */
  def startWithCom(code: String): ComRun =
    startWithCom(codeToInput(code))

  /** Starts a [[ComRun]] for testing. */
  def startWithCom(input: Input): ComRun =
    startWithCom(input, RunConfig())

  /** Starts a [[ComRun]] for testing. */
  def startWithCom(code: String, config: RunConfig): ComRun =
    startWithCom(codeToInput(code), config)

  /** Starts a [[ComRun]] for testing. */
  def startWithCom(input: Input, config: RunConfig): ComRun = {
    val msg = new MsgHandler
    val (run, out, err) = io(config)(jsEnv.startWithCom(input, _, msg.onMessage _))
    run.future.onComplete(msg.onRunComplete _)(TestKit.completer)

    new ComRun(run, out, err, msg, timeout)
  }

  /** Convenience method to start a [[Run]] and close it after usage. */
  def withRun[T](code: String)(body: Run => T): T =
    withRun(codeToInput(code))(body)

  /** Convenience method to start a [[Run]] and close it after usage. */
  def withRun[T](input: Input)(body: Run => T): T =
    withRun(input, RunConfig())(body)

  /** Convenience method to start a [[Run]] and close it after usage. */
  def withRun[T](code: String, config: RunConfig)(body: Run => T): T =
    withRun(codeToInput(code), config)(body)

  /** Convenience method to start a [[Run]] and close it after usage. */
  def withRun[T](input: Input, config: RunConfig)(body: Run => T): T = {
    val run = start(input, config)
    try body(run)
    finally run.close()
  }

  /** Convenience method to start a [[ComRun]] and close it after usage. */
  def withComRun[T](code: String)(body: ComRun => T): T = withComRun(codeToInput(code))(body)

  /** Convenience method to start a [[ComRun]] and close it after usage. */
  def withComRun[T](input: Input)(body: ComRun => T): T = withComRun(input, RunConfig())(body)

  /** Convenience method to start a [[ComRun]] and close it after usage. */
  def withComRun[T](code: String, config: RunConfig)(body: ComRun => T): T =
    withComRun(codeToInput(code), config)(body)

  /** Convenience method to start a [[ComRun]] and close it after usage. */
  def withComRun[T](input: Input, config: RunConfig)(body: ComRun => T): T = {
    val run = startWithCom(input, config)
    try body(run)
    finally run.close()
  }

  private def io[T <: JSRun](config: RunConfig)(start: RunConfig => T): (T, IOReader, IOReader) = {
    val out = new IOReader
    val err = new IOReader

    def onOutputStream(o: Option[InputStream], e: Option[InputStream]) = {
      o.foreach(out.onInputStream _)
      e.foreach(err.onInputStream _)
    }

    val newConfig = config
      .withOnOutputStream(onOutputStream)
      .withInheritOut(false)
      .withInheritErr(false)

    val run = start(newConfig)

    run.future.onComplete(out.onRunComplete _)(TestKit.completer)
    run.future.onComplete(err.onRunComplete _)(TestKit.completer)

    (run, out, err)
  }
}

private object TestKit {
  /** Execution context to run completion callbacks from runs under test. */
  private val completer =
    ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  private def codeToInput(code: String): Input = {
    val vf = MemVirtualBinaryFile.fromStringUTF8("testScript.js", code)
    Input.ScriptsToLoad(List(vf))
  }
}
