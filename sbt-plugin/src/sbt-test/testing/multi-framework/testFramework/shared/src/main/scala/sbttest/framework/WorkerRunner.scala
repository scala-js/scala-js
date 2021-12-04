package sbttest.framework

import sbt.testing._

import scala.concurrent._

final class WorkerRunner(
    args: Array[String],
    remoteArgs: Array[String],
    testClassLoader: ClassLoader,
    send: String => Unit
) extends BaseRunner(args, remoteArgs, testClassLoader) {

  /** Number of tasks completed on this node */
  private[this] var doneCount = 0

  /** Whether we have seen a Hello message from the controller yet */
  private[this] val seenHello = Promise[Unit]()

  private[framework] override val taskBlock = seenHello.future

  // Notify the controller of our existence
  send("s")

  def tasks(taskDefs: Array[TaskDef]): Array[Task] = {
    // Notify the controller of new tasks
    send("t" + taskDefs.length)
    taskDefs.map(newTask)
  }

  def done(): String = {
    send("d" + doneCount)
    "" // <- ignored
  }

  private[framework] def taskDone(): Unit = doneCount += 1

  def receiveMessage(msg: String): Option[String] = {
    assert(msg == "Hello")
    seenHello.success(())
    None // <- ignored
  }
}
