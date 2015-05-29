package sbt.testing

/** A logger through which to provide feedback to the user about a run.
 *
 *  The difference between the event handler and the logger is that the event
 *  handler is for events intended to be consumed by the client software
 *  whereas the logger is for messages intended to be consumed by the client
 *  *user* (<em>i.e.</em>, a human).
 *
 *  Implementations of this interface must be thread-safe.
 */
trait Logger {

  /** True if ANSI color codes are understood by this instance. */
  def ansiCodesSupported(): Boolean

  /** Provide an error message.
   *
   *  @param msg the error message
   */
  def error(msg: String): Unit

  /** Provide an warning message.
   *
   *  @param msg the warning message
   */
  def warn(msg: String): Unit

  /** Provide an info message.
   *
   *  @param msg the info message
   */
  def info(msg: String): Unit

  /** Provide a debug message.
   *
   *  @param msg the debug message
   */
  def debug(msg: String): Unit

  /** Provide a stack trace
   *
   *  @param t the <code>Throwable</code> containing the stack trace being logged
   */
  def trace(t: Throwable): Unit
}
