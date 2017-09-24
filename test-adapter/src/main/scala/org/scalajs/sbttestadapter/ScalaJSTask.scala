/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.testadapter

import org.scalajs.jsenv._
import org.scalajs.testcommon._

import scala.concurrent.duration.Duration
import scala.collection.mutable
import scala.util.Try

import sbt.testing._

final class ScalaJSTask private[testadapter] (
    runner: ScalaJSRunner,
    taskInfo: TaskInfo
) extends Task {

  def taskDef: TaskDef = taskInfo.taskDef
  def tags: Array[String] = taskInfo.tags.toArray

  def execute(handler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    val slave = runner.getSlave()

    // Prepare result handler
    // TODO rip this out once we know it doesn't garble the logs.
    val shouldBufferLog = {
      val propName = "org.scalajs.testadapter.bufferlog"
      Try(System.getProperty(propName, "false").toBoolean).getOrElse(false)
    }

    val logBuffer = mutable.Buffer.empty[ScalaJSTask.LogElement[_]]

    def log[T](level: Logger => (T => Unit))(log: LogElement[T]): Unit = {
      if (shouldBufferLog) {
        logBuffer += new ScalaJSTask.LogElement(log.index, level, log.x)
      } else {
        level(loggers(log.index))(log.x)
      }
    }

    slave.attach(JVMSlaveEndpoints.event)(handler.handle _)
    slave.attach(JVMSlaveEndpoints.logError)(log(_.error))
    slave.attach(JVMSlaveEndpoints.logWarn)(log(_.warn))
    slave.attach(JVMSlaveEndpoints.logInfo)(log(_.info))
    slave.attach(JVMSlaveEndpoints.logDebug)(log(_.debug))
    slave.attach(JVMSlaveEndpoints.logTrace)(log(_.trace))

    try {
      // Execute task. No (!) timeout.
      val req =
        new ExecuteRequest(taskInfo, loggers.map(_.ansiCodesSupported).toList)
      val taskInfos = slave.call(JSSlaveEndpoints.execute)(req).await()

      // Flush log buffer
      if (shouldBufferLog) {
        runner.loggerLock.synchronized {
          logBuffer.foreach(_.call(loggers))
        }
      }

      taskInfos.map(new ScalaJSTask(runner, _)).toArray
    } finally {
      slave.detach(JVMSlaveEndpoints.event)
      slave.detach(JVMSlaveEndpoints.logError)
      slave.detach(JVMSlaveEndpoints.logWarn)
      slave.detach(JVMSlaveEndpoints.logInfo)
      slave.detach(JVMSlaveEndpoints.logDebug)
      slave.detach(JVMSlaveEndpoints.logTrace)
    }
  }
}

object ScalaJSTask {
  private final class LogElement[T](index: Int,
      log: Logger => (T => Unit), data: T) {
    def call(arr: Array[Logger]): Unit = log(arr(index))(data)
  }
}
