package scala.scalajs.tools.env

trait ComJSRunner extends AsyncJSRunner {

  /** Send a message to the JS VM. Throws if the message cannot be sent. */
  def send(msg: String): Unit

  /** Block until a message is received. Throws a [[ComClosedExcpetion]]
   *  if the channel is closed before a message is received.
   */
  def receive(): String

  /** Close the communication channel. Allows the VM to terminate if it is
   *  still waiting for callback.
   */
  def close(): Unit

}
