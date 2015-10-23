package sbt.testing

/** Represents one run of a suite of tests.
 *
 *  The run represented by a <code>Runner</code> has a lifecycle. The run
 *  begins when the <code>Runner</code> is instantiated by the framework and
 *  returned to the client during a <code>Framework.runner</code> invocation.
 *  The run continues until the client invokes <code>done</code> on the
 *  <code>Runner</code>. Before invoking <code>done</code>, the client can
 *  invoke the <code>tasks</code> method as many times at it wants, but once
 *  <code>done</code> has been invoked, the <code>Runner</code> enters "spent"
 *  mode. Any subsequent invocations of <code>tasks</code> will be met with an
 *  <code>IllegalStateException</code>.
 *
 *  In Scala.js, the client may request multiple instances of
 *  <code>Runner</code>, where one of these instances is considered the master.
 *  The slaves receive a communication channel to the master. Once the master's
 *  <code>done</code> method is invoked, nothing may be invoked on the slaves
 *  or the master. Slaves can be de-comissioned before the master terminates.
 */
trait Runner {

  /** Returns an array of tasks that when executed will run tests and suites
   *  determined by the passed <code>TaskDef</code>s.
   *
   *  <p>
   *  Each returned task, when executed, will run tests and suites determined by
   *  the test class name, fingerprints, "explicitly specified" field, and
   *  selectors of one of the passed <code>TaskDef</code>s.
   *  </p>
   *
   *  <p>
   *  This <code>tasks</code> method may be called with <code>TaskDef</code>s
   *  containing the same value for <code>testClassName</code> but different
   *  fingerprints. For example, if both a class and its companion object were
   *  test classes, the <code>tasks</code> method could be passed an array
   *  containing <code>TaskDef</code>s with the same name but with a different
   *  value for <code>fingerprint.isModule</code>.
   *  </p>
   *
   *  <p>
   *  A test framework may "reject" a requested task by returning no
   *  <code>Task</code> for that <code>TaskDef</code>.
   *  </p>
   *
   *  @param taskDefs the <code>TaskDef</code>s for requested tasks
   *  @return an array of <code>Task</code>s
   *  @throws java.lang.IllegalStateException if invoked after <code>done</code>
   *      has been invoked.
   */
  def tasks(taskDefs: Array[TaskDef]): Array[Task]

  /** Indicates the client is done with this <code>Runner</code> instance.
   *
   *  After invoking the <code>done</code> method on a <code>Runner</code>
   *  instance, the client should no longer invoke the <code>task</code> methods
   *  on that instance. (If the client does invoke <code>task</code> after
   *  <code>done</code>, it will be rewarded with an
   *  <code>IllegalStateException</code>.)
   *
   *  Similarly, after returning from <code>done</code>, the test framework
   *  should no longer write any messages to the <code>Logger</code>, nor fire
   *  any more events to the <code>EventHandler</code>, passed to
   *  <code>Framework.runner</code>. If the test framework has not completed
   *  writing log messages or firing events when the client invokes
   *  <code>done</code>, the framework should not return from <code>done</code>
   *  until it is finished sending messages and events, and may block the thread
   *  that invoked <code>done</code> until it is actually done.
   *
   *  In short, by invoking <code>done</code>, the client indicates it is done
   *  invoking the <code>task</code> methods for this run. By returning from
   *  <code>done</code>, the test framework indicates it is done writing log
   *  messages and firing events for this run.
   *
   *  If the client invokes <code>done</code> more than once on the same
   *  <code>Runner</code> instance, the test framework should on subsequent
   *  invocations should throw <code>IllegalStateException</code>.
   *
   *  The test framework may send a summary (<em>i.e.</em>, a message giving
   *  total tests succeeded, failed, and so on) to the user via a log message.
   *  If so, it should return the summary from <code>done</code>. If not, it
   *  should return an empty string. The client may use the return value of
   *  <code>done</code> to decide whether to display its own summary message.
   *
   *  The test framework may return a multi-lines string (<em>i.e.</em>, a
   *  message giving total tests succeeded, failed and so on) to the client.
   *
   *  In Scala.js, the client must not call this method before all execute
   *  methods of all Tasks have called their completion continuation. Otherwise,
   *  the Framework should throw an IllegalStateException (since it cannot
   *  block).
   *
   *  Further, if this is the master, the client must not call this method
   *  before, all done methods of all slaves have returned (otherwise,
   *  IllegalStateException). If this is a slave, the returned string is ignored.
   *
   *  @return a possibly multi-line summary string, or the empty string if no
   *          summary is provided
   */
  def done(): String

  /**
   * Remote args that will be passed to <code>Runner</code> in a sub-process as
   * <em>remoteArgs</em>.
   *
   * @return an array of strings that will be passed to <code>Runner</code> in
   *         a sub-process as <code>remoteArgs</code>.
   */
  def remoteArgs(): Array[String]

  /** Returns the arguments that were used to create this <code>Runner</code>.
   *
   *  @return an array of argument that is used to create this Runner.
   */
  def args: Array[String]

  /** Scala.js specific: Invoked on the master <code>Runner</code>, if a slave
   *  sends a message (through the channel provided by the client).
   *
   *  The master may send a message back to the sending slave by returning the
   *  message in a Some.
   *
   *  Invoked on a slave <code>Runner</code>, if the master responds to a
   *  message (sent by the slave via the supplied closure in
   *  <code>slaveRunner</code>). The return value of the call is ignored in
   *  this case.
   */
  def receiveMessage(msg: String): Option[String]

  /** Scala.js specific: Serialize a task created by <code>tasks</code> or
   *  returned from <code>execute</code>.
   *
   *  The resulting string will be passed to the <code>deserializeTask</code>
   *  method of another runner. After calling this method, the passed task is
   *  invalid and should dissociate from this runner.
   */
  def serializeTask(task: Task, serializer: TaskDef => String): String

  /** Scala.js specific: Deserialize a task that has been serialized by
   *  <code>serializeTask</code> of another or this <code>Runner</code>.
   *
   *  The resulting task must associate with this runner.
   */
  def deserializeTask(task: String, deserializer: String => TaskDef): Task
}
