package sbttest.framework

import sbt.testing._

import scala.concurrent._

abstract class BaseRunner(
    val args: Array[String],
    val remoteArgs: Array[String],
    private[framework] val testClassLoader: ClassLoader
) extends Runner {

  protected def newTask(taskDef: TaskDef): Task =
    new DummyTask(taskDef, this)

  /** Called by task when it has finished executing */
  private[framework] def taskDone(): Unit

  /** Tasks need to wait for this future to complete if they get called with a
   *  continuation. This is used to ensure that the controller/worker message
   *  channel eventually delivers messages.
   */
  private[framework] val taskBlock: Future[Unit]

  def serializeTask(task: Task, serializer: TaskDef => String): String =
    serializer(task.taskDef)

  def deserializeTask(task: String, deserializer: String => TaskDef): Task =
    newTask(deserializer(task))

}
