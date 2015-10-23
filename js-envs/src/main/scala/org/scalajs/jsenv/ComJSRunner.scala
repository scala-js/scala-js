package org.scalajs.jsenv

import scala.concurrent.duration.Duration

trait ComJSRunner extends AsyncJSRunner {

  /** Send a message to the JS VM. Throws if the message cannot be sent. */
  def send(msg: String): Unit

  /** Blocks until a message is received and returns it.
   *
   *  @throws ComJSEnv.ComClosedException if the channel is closed before a
   *    message is received
   */
  final def receive(): String = receive(Duration.Inf)

  /** Blocks until a message is received and returns it.
   *
   *  @throws ComJSEnv.ComClosedException if the channel is closed before a
   *    message is received
   *  @throws scala.concurrent.TimeoutException if the timeout expires
   *    before a message is received and the channel is still open
   */
  def receive(timeout: Duration): String

  /** Close the communication channel. Allows the VM to terminate if it is
   *  still waiting for callback. The JVM side **must** call close in
   *  order to be able to expect termination of the VM.
   *
   *  Calling [[stop]] on a [ComJSRunner]] automatically closes the
   *  channel.
   */
  def close(): Unit

  /** Abort the associated run. Also closes the communication channel. */
  abstract override def stop(): Unit = {
    close()
    super.stop()
  }

}
