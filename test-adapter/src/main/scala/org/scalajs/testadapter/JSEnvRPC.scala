/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.testadapter

import scala.concurrent.ExecutionContext

import org.scalajs.jsenv._
import org.scalajs.testcommon._

/** RPC Core for use with a [[JSEnv]]. */
private[testadapter] final class JSEnvRPC(
    jsenv: JSEnv, input: Input, config: RunConfig)(
    implicit executionContext: ExecutionContext) extends RPCCore {

  private val run = jsenv.startWithCom(input, config, handleMessage)

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
  run.future.onComplete(
      t => close(JSEnvRPC.RunTerminatedException(t.failed.toOption)))

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

private[testadapter] object JSEnvRPC {
  final case class RunTerminatedException(c: Option[Throwable])
      extends Exception(null, c.orNull)
}
