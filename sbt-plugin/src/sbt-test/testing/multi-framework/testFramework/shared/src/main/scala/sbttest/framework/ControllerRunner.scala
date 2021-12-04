package sbttest.framework

import sbt.testing._

import scala.concurrent._

import java.util.concurrent.atomic.AtomicInteger

final class ControllerRunner(
    args: Array[String],
    remoteArgs: Array[String],
    testClassLoader: ClassLoader
) extends BaseRunner(args, remoteArgs, testClassLoader) {

  /** Number of tasks registered in the whole system */
  private[this] val registeredCount = new AtomicInteger(0)

  /** Number of tasks completed in the whole system */
  private[this] val doneCount = new AtomicInteger(0)

  /** Number of running workers in the whole system */
  private[this] val workerCount = new AtomicInteger(0)

  /** If a task gets called in the controller, there is no point waiting for
   *  messages.
   */
  private[framework] override val taskBlock = Future.successful(())

  def tasks(taskDefs: Array[TaskDef]): Array[Task] = {
    registeredCount.addAndGet(taskDefs.length)
    taskDefs.map(newTask)
  }

  def done(): String = {
    val workers = workerCount.get
    val registered = registeredCount.get
    val done = doneCount.get

    if (workers > 0) {
      throw new IllegalStateException(
          s"There are still $workers workers running")
    }

    if (registered != done)
      throw new IllegalStateException(
          s"$registered task(s) were registered, $done were executed")
    else
      s"Dummy Test Framework processed $done task(s)"
  }

  private[framework] def taskDone(): Unit = doneCount.incrementAndGet()

  def receiveMessage(msg: String): Option[String] = msg(0) match {
    case 's' =>
      workerCount.incrementAndGet()
      // Send Hello message back
      Some("Hello")
    case 't' =>
      // A worker notifies us of the registration of tasks
      registeredCount.addAndGet(msg.tail.toInt)
      None
    case 'd' =>
      // A worker notifies us of the completion of tasks
      val count = msg.tail.toInt
      doneCount.addAndGet(count)
      workerCount.decrementAndGet()
      None
  }

}
