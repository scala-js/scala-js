/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.testadapter

import scala.concurrent.TimeoutException

import org.scalajs.jsenv.{ComJSEnv, ComJSRunner}
import org.scalajs.testcommon._

/** RPC Core for use with a [[ComJSRunner]].
 *
 *  @note You may only create one instance of this per [[ComJSRunner]]
 */
private[testadapter] final class ComJSEnvRPC(
    val runner: ComJSRunner) extends RPCCore {

  @volatile
  private[this] var closed = false

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
        case _: ComJSEnv.ComClosedException =>
        case _: InterruptedException        =>
        case _: Exception if closed         =>
          // Some JSEnvs might throw something else if they got closed.
          // We are graceful in the 0.6.x branch.
      }
    }
  }

  receiverThread.start()

  override protected def send(msg: String): Unit = synchronized {
    if (!closed)
      runner.send(msg)
  }

  override def close(): Unit = synchronized {
    closed = true

    super.close()

    // Close the com first so the receiver thread terminates.
    runner.close()

    receiverThread.interrupt()
    receiverThread.join()
  }
}
