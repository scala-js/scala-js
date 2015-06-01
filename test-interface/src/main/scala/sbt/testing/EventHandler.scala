package sbt.testing

/** Interface implemented by clients that handle events fired by the test
 *  framework during a run.
 *
 *  An event handler is passed to the test framework via the
 *  <code>execute</code> method of <a href="Task.html"><code>Task</code></a>s.
 */
trait EventHandler {

  /** Handle an event.
   *
   *  @param event the event to handle
   */
  def handle(event: Event): Unit
}
