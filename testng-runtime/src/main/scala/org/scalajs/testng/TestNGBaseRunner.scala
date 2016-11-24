package org.scalajs.testng

import sbt.testing.{Runner, Task, TaskDef}

abstract class TestNGBaseRunner(
    val args: Array[String],
    val remoteArgs: Array[String],
    private[testng] val testClassLoader: ClassLoader,
    private[testng] val runSettings: RunSettings) extends Runner {

  protected def newTask(taskDef: TaskDef): Task = new TestNGTask(taskDef, this)

  protected var doneCount = 0
  protected var passedCount = 0
  protected var failedCount = 0
  protected var ignoredCount = 0
  protected var skippedCount = 0
  protected var totalCount = 0

  private[testng] def taskDoneCount: Int = doneCount
  private[testng] def testPassedCount: Int = passedCount
  private[testng] def testFailedCount: Int = failedCount
  private[testng] def testIgnoredCount: Int = ignoredCount
  private[testng] def testSkippedCount: Int = skippedCount
  private[testng] def testTotalCount: Int = totalCount

  private[testng] def taskDone(): Unit = doneCount += 1
  private[testng] def testPassed(): Unit = passedCount += 1
  private[testng] def testFailed(): Unit = failedCount += 1
  private[testng] def testIgnored(): Unit = ignoredCount += 1
  private[testng] def testSkipped(): Unit = skippedCount += 1
  private[testng] def testRegisterTotal(count: Int = 1): Unit = totalCount += count

  def serializeTask(task: Task, serializer: TaskDef => String): String =
    serializer(task.taskDef)

  def deserializeTask(task: String, deserializer: String => TaskDef): Task =
    newTask(deserializer(task))

  def resetTestCounts(): Unit = {
    passedCount = 0
    failedCount = 0
    ignoredCount = 0
    skippedCount = 0
    totalCount = 0
  }
}

object TestNGBaseRunner {
  object Done {
    def deserialize(str: String): Done = {
      val split = str.split(':')
      if (split.length != 6) {
        throw new IllegalArgumentException(str)
      } else {
        Done(split(0).toInt, split(1).toInt, split(2).toInt, split(3).toInt,
            split(4).toInt, split(5).toInt)
      }
    }
  }

  case class Done(done: Int, passed: Int, failed: Int, ignored: Int,
      skipped: Int, total: Int) {
    def serialize(): String =
      Seq(done, passed, failed, ignored, skipped, total).mkString(":")
  }
}
