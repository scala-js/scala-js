package sbt.testing

/** A task to execute.
 *
 *  The client may decide when or how to execute the task based on its tags. A
 *  task can be any job, but is primarily intended for running tests and/or
 *  supplying more tasks to the client. A framework can supply more tasks to
 *  the client in the returned an array of <em>Task</em>s (which can be empty
 *  if there's no more work to do.)
 */
trait Task {

  /** A possibly zero-length array of string tags associated with this task.
   *
   *  A task may be tagged, for example, with a string that indicates it
   *  consumes a lot of CPU time. Users can configure the client to only run,
   *  say, three such CPU-intensive tasks concurrently.
   *
   *  @return a possibly zero-length string array of this task's tags
   */
  def tags(): Array[String]

  /** Executes this task, possibly returning to the client new tasks to execute.
   *
   *  @param eventHandler an event handler to which to fire events during the run
   *  @param loggers an array of loggers to which to emit log messages during the run
   *  @return a possibly empty array of new tasks for the client to execute
   */
  def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task]

  /** Scala.js specific: Same as basic
   *  [[[sbt.testing.Task.execute(eventHandler:sbt\.testing\.EventHandler,loggers:Array[sbt\.testing\.Logger])*
   *  execute]]]
   *  but takes a continuation.
   *
   *  This is to support JavaScripts asynchronous nature.
   *
   *  When running in a JavaScript environment, only this method will be
   *  called.
   */
  def execute(eventHandler: EventHandler, loggers: Array[Logger],
      continuation: Array[Task] => Unit): Unit

  /** Returns the <code>TaskDef</code> that was used to request this
   *  <code>Task</code>.
   *
   *  @return the <code>TaskDef</code> that was used to request this
   *          <code>Task</code>.
   */
  def taskDef(): TaskDef
}
