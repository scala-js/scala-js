package org.scalajs.junit

import com.novocode.junit.RunSettings
import sbt.testing._

abstract class JUnitBaseRunner(
    val args: Array[String],
    val remoteArgs: Array[String],
    private[junit] val testClassLoader: ClassLoader,
    private[junit] val runSettings: RunSettings) extends Runner {

  protected def newTask(taskDef: TaskDef): Task =
    new JUnitTask(taskDef, this)

  protected var doneCount = 0
  protected var passedCount = 0
  protected var failedCount = 0
  protected var ignoredCount = 0
  protected var skippedCount = 0
  protected var totalCount = 0

  private[junit] def taskDoneCount: Int = doneCount
  private[junit] def testPassedCount: Int = passedCount
  private[junit] def testFailedCount: Int = failedCount
  private[junit] def testIgnoredCount: Int = ignoredCount
  private[junit] def testSkippedCount: Int = skippedCount
  private[junit] def testTotalCount: Int = totalCount

  private[junit] def taskDone(): Unit = doneCount += 1
  private[junit] def testPassed(): Unit = passedCount += 1
  private[junit] def testFailed(): Unit = failedCount += 1
  private[junit] def testIgnored(): Unit = ignoredCount += 1
  private[junit] def testSkipped(): Unit = skippedCount += 1
  private[junit] def testRegisterTotal(count: Int = 1): Unit = totalCount += count

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

object JUnitBaseRunner {
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
