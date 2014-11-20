package sbttest.framework

import sbt.testing._

import java.util.concurrent.atomic.AtomicInteger

final class MasterRunner(
    args: Array[String],
    remoteArgs: Array[String],
    testClassLoader: ClassLoader
) extends BaseRunner(args, remoteArgs, testClassLoader) {

  /** Number of tasks registered in the whole system */
  private[this] val registeredCount = new AtomicInteger(0)

  /** Number of tasks completed in the whole system */
  private[this] val doneCount = new AtomicInteger(0)

  /** Number of running slaves in the whole system */
  private[this] val slaveCount = new AtomicInteger(0)

  def tasks(taskDefs: Array[TaskDef]): Array[Task] = {
    registeredCount.addAndGet(taskDefs.length)
    taskDefs.map(newTask)
  }

  def done(): String = {
    val slaves = slaveCount.get
    val registered = registeredCount.get
    val done = doneCount.get

    if (slaves > 0) {
      throw new IllegalStateException(
          s"There are still $slaves slaves running")
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
      slaveCount.incrementAndGet()
      // Send Hello message back
      Some("Hello")
    case 't' =>
      // Slave notifies us of registration of tasks
      registeredCount.addAndGet(msg.tail.toInt)
      None
    case 'd' =>
      // Slave notifies us of completion of tasks
      val count = msg.tail.toInt
      doneCount.addAndGet(count)
      slaveCount.decrementAndGet()
      None
  }

}
