package sbttest.framework

import sbt.testing._

final class SlaveRunner(
    args: Array[String],
    remoteArgs: Array[String],
    testClassLoader: ClassLoader,
    send: String => Unit
) extends BaseRunner(args, remoteArgs, testClassLoader) {

  /** Number of tasks completed on this node */
  private[this] var doneCount = 0

  /** Whether we have seen a Hello message from the master yet */
  private[this] var seenHello = false

  // Notify master of our existence
  send("s")

  def tasks(taskDefs: Array[TaskDef]): Array[Task] = {
    ensureSeenHello()

    // Notify master of new tasks
    send("t" + taskDefs.length)
    taskDefs.map(newTask)
  }

  def done(): String = {
    ensureSeenHello()
    send("d" + doneCount)
    "" // <- ignored
  }

  private[framework] def taskDone(): Unit = doneCount += 1

  def receiveMessage(msg: String): Option[String] = {
    assert(msg == "Hello")
    seenHello = true
    None // <- ignored
  }

  override def serializeTask(task: Task,
      serializer: TaskDef => String): String = {
    ensureSeenHello()
    super.serializeTask(task, serializer)
  }

  override def deserializeTask(task: String,
      deserializer: String => TaskDef): Task = {
    ensureSeenHello()
    super.deserializeTask(task, deserializer)
  }

  private def ensureSeenHello(): Unit = {
    if (!seenHello)
      throw new IllegalStateException("Have not seen the master yet")
  }

}
