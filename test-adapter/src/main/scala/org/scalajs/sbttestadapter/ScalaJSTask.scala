/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.testadapter

import org.scalajs.core.tools.json._

import org.scalajs.jsenv._

import scala.collection.mutable

import sbt.testing._

import TaskDefSerializers._
import EventSerializers._
import ComUtils.LoopHandler
import ScalaJSTask.LogElement

final class ScalaJSTask private (
    runner: ScalaJSRunner,
    val taskDef: TaskDef,
    val tags: Array[String],
    serializedTask: String
) extends Task {

  def execute(handler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    val slave = runner.getSlave()

    // Prepare data to send to Slave VM
    val colorSupport = loggers.map(_.ansiCodesSupported).toList
    val data = {
      val bld = new JSONObjBuilder
      bld.fld("serializedTask", serializedTask)
      bld.fld("loggerColorSupport", colorSupport)
      bld.toJSON
    }

    // Send command to VM
    slave.send("execute:" + jsonToString(data))

    // Prepare result handler
    val logBuffer = mutable.Buffer.empty[LogElement[_]]

    val doneHandler: LoopHandler[List[TaskInfo]] = {
      case ("ok", msg) => Some(fromJSON[List[TaskInfo]](readJSON(msg)))
    }

    val handlerChain = (
        eventHandler(handler) orElse
        loggerHandler(logBuffer) orElse
        runner.msgHandler(slave) orElse
        doneHandler)

    // Wait for result
    val taskInfos = ComUtils.receiveLoop(slave)(handlerChain)

    // Flush log buffer
    runner.loggerLock.synchronized {
      logBuffer.foreach(_.call(loggers))
    }

    taskInfos.map(ScalaJSTask.fromInfo(runner, _)).toArray
  }

  private def eventHandler(handler: EventHandler): LoopHandler[Nothing] = {
    case ("event", data) =>
      val event = fromJSON[Event](readJSON(data))
      handler.handle(event)
      None
  }

  private def loggerHandler(
      buf: mutable.Buffer[LogElement[_]]): LoopHandler[Nothing] = {

    def processData(data: String) = {
      val pos = data.indexOf(':')
      assert(pos != -1, "Log command needs logger index")
      val index = data.substring(0, pos).toInt
      val innerData = data.substring(pos + 1)

      (index, innerData)
    }

    def log(level: Logger => (String => Unit), data: String) = {
      val (index, msg) = processData(data)
      buf += new LogElement(index, level, msg)
      None
    }

    val pf: LoopHandler[Nothing] = {
      case ("error", data) => log(_.error, data)
      case ("warn",  data) => log(_.warn,  data)
      case ("info",  data) => log(_.info,  data)
      case ("debug", data) => log(_.debug, data)
      case ("trace", data) =>
        val (index, innerData) = processData(data)
        val throwable = fromJSON[RemoteException](readJSON(innerData))
        buf += new LogElement(index, _.trace, throwable)
        None
    }

    pf
  }

}

object ScalaJSTask {
  private final class LogElement[T](index: Int,
      log: Logger => (T => Unit), data: T) {
    def call(arr: Array[Logger]): Unit = log(arr(index))(data)
  }

  private[testadapter] def fromInfo(runner: ScalaJSRunner,
      info: TaskInfo): ScalaJSTask =
    new ScalaJSTask(runner, info.taskDef, info.tags, info.serializedTask)
}
