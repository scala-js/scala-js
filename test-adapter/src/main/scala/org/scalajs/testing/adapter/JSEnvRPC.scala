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

package org.scalajs.testing.adapter

import scala.concurrent.ExecutionContext

import org.scalajs.jsenv._
import org.scalajs.logging.Logger
import org.scalajs.testing.common._

/** RPC Core for use with a [[JSEnv]]. */
private[adapter] final class JSEnvRPC(
    jsenv: JSEnv, input: Seq[Input], logger: Logger, env: Map[String, String])(
    implicit ec: ExecutionContext)
    extends RPCCore {

  private val run: JSComRun = {
    /* #4560 Explicitly redirect out/err to System.out/System.err, instead of
     * relying on `inheritOut` and `inheritErr`, so that streams installed with
     * `System.setOut` and `System.setErr` are always taken into account.
     * sbt installs such alternative outputs when it runs in server mode.
     *
     * We never wait for these threads to finish. In theory, tasks that use the
     * test adapter may complete before all output has been transferred to
     * `System.out` and `System.err`.
     */
    val runConfig = RunConfig()
      .withLogger(logger)
      .withEnv(env)
      .withInheritOut(false)
      .withInheritErr(false)
      .withOnOutputStream { (out, err) =>
        out.foreach(o => PipeOutputThread.start(o, System.out))
        err.foreach(e => PipeOutputThread.start(e, System.err))
      }

    jsenv.startWithCom(input, runConfig, handleMessage)
  }

  /* Once the com closes, ensure all still pending calls are failing.
   * This can be necessary, if the JSEnv terminates unexpectedly.
   * Note: We do not need to give a grace time here, since the reply
   * dispatch happens synchronously in `handleMessage`.
   * In other words, at this point we'll only see pending calls that
   * would require another call to `handleMessage` in order to complete
   * successfully. But this is not going to happen since the com run is
   * completed (and it is an explicit guarantee that `handleMessage` is not
   * called anymore after that).
   */
  run.future.onComplete(t => close(JSEnvRPC.RunTerminatedException(t.failed.toOption)))

  override protected def send(msg: String): Unit = run.send(msg)

  override def close(cause: Throwable): Unit = {
    /* Close the RPC layer and fail all pending calls.
     * This needs to happen first so we do not race completion of the run
     * itself (to retain the cause given here).
     */
    super.close(cause)

    // Now terminate the run itself.
    run.close()
  }
}

private[adapter] object JSEnvRPC {
  final case class RunTerminatedException(c: Option[Throwable]) extends Exception(null, c.orNull)
}
