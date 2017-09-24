/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.testadapter

import scala.concurrent.Promise

import org.scalajs.jsenv.{ComJSEnv, ComJSRunner}
import org.scalajs.testcommon._

/** RPC Core for use with a [[ComJSRunner]].
 *
 *  You may only create one instance of this per [[ComJSRunner]].
 *
 *  Caution: [[RPCCore]] claims that it is a fully duplex, asynchronous two-way
 *  RPC interface. [[ComJSEnvRPC]] does **not** satisfy this. In fact, calls and
 *  messages to a [[ComJSEnvRPC]] are only handled while a
 *  [[ComJSEnvRPC.Future]] for a pending call is awaited. This is necessary to
 *  minimize context switches during test runs (see #3123).
 */
private[testadapter] final class ComJSEnvRPC(
    val runner: ComJSRunner) extends RPCCore[ComJSEnvRPC.Future] {

  private[this] var hasReceiver = false

  override protected def send(msg: String): Unit = runner.send(msg)

  override protected def toFuture[T](p: Promise[T]): ComJSEnvRPC.Future[T] =
    new ComJSEnvRPC.Future(p, this)

  override def close(): Unit = {
    super.close()
    runner.close()
  }

  private def await(p: Promise[_]): Unit = {
    if (tryBecomeReceiver(p)) {
      try {
        while (!p.isCompleted) {
          handleMessage(runner.receive())
          synchronized(notifyAll())
        }
      } finally {
        unbecomeReceiver()
      }
    }
  }

  private def tryBecomeReceiver(p: Promise[_]): Boolean = synchronized {
    while (!p.isCompleted && hasReceiver)
      wait()

    if (!hasReceiver) {
      hasReceiver = true
      true
    } else {
      false
    }
  }

  private def unbecomeReceiver(): Unit = synchronized {
    hasReceiver = false
    notifyAll()
  }
}

private[testadapter] object ComJSEnvRPC {
  /** A future that can only block. Used to help receiver thread. */
  class Future[T] private[ComJSEnvRPC] (p: Promise[T], com: ComJSEnvRPC) {
    def await(): T = {
      com.await(p)
      p.future.value.get.get
    }
  }
}
