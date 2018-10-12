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

package org.scalajs.testadapter

import scala.concurrent.{ExecutionContext, TimeoutException}

import org.scalajs.jsenv.{ComJSEnv, ComJSRunner}
import org.scalajs.testcommon._

/** RPC Core for use with a [[ComJSRunner]].
 *
 *  @note You may only create one instance of this per [[ComJSRunner]]
 */
private[testadapter] final class ComJSEnvRPC(val runner: ComJSRunner)(
    implicit executionContext: ExecutionContext) extends RPCCore {

  private val receiverThread = new Thread {
    setName("ComJSEnvRPC receiver")

    override def run(): Unit = {
      try {
        while (true) {
          try {
            handleMessage(runner.receive())
          } catch {
            case _: TimeoutException =>
          }
        }
      } catch {
        case e: Exception =>
          /* We got terminated. This might be due to an interruption from a call
           * to `close` or due to the JSEnv crashing.
           *
           * Ensure all still pending calls are failing.
           * This can be necessary, if the JSEnv terminates unexpectedly.
           * Note: We do not need to give a grace time here, since the reply
           * dispatch happens synchronously in `handleMessage`.
           * In other words, at this point we'll only see pending calls that
           * would require another call to `handleMessage` in order to complete
           * successfully. But this is not going to happen since this thread is
           * the only one that calls `handleMessage` and it's about to terminate.
           */
          ComJSEnvRPC.super.close(e)

        case t: Throwable =>
          /* Same here, but this is probably a serious VM error condition we
           * should propagate. We do not use NonFatal, since it does not catch
           * InterruptedException and ControlThrowables which we do not want to
           * propagate.
           */
          ComJSEnvRPC.super.close(t)
          throw t
      }
    }
  }

  receiverThread.start()

  override protected def send(msg: String): Unit = synchronized {
    runner.send(msg)
  }

  override def close(cause: Throwable): Unit = synchronized {
    // First close the RPC layer, so we propagate the right cause.
    super.close(cause)

    // Close the runner so the receiver thread terminates.
    runner.close()

    receiverThread.interrupt()
    receiverThread.join()
  }
}
