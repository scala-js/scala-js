package org.scalajs.testinterface.internal

import scala.scalajs.js
import js.JSConverters._
import js.annotation.JSExport
import js.Dynamic.{literal => lit}

import scala.util.control.NonFatal
import scala.util.{Try, Success, Failure}

import sbt.testing._

abstract class BridgeBase(frameworkName: String) {

  protected[this] val framework = FrameworkLoader.loadFramework(frameworkName)

  @JSExport
  def init(): Unit = {
    Com.init(handleMsg _)
  }

  private def handleMsg(msg: String): Unit = {
    val pos = msg.indexOf(':')
    val cmd = if (pos == -1) msg else msg.substring(0, pos)

    def strArg = {
      if (pos == -1)
        throw new IllegalArgumentException(s"$cmd needs args")
      else
        msg.substring(pos + 1)
    }

    try {
      handleMsgImpl(cmd, strArg)
    } catch {
      case NonFatal(t) =>
        val data = js.JSON.stringify(ThrowableSerializer.serialize(t))
        Com.send("bad:" + data)
    }
  }

  protected def reply(result: Try[Any]): Unit = result match {
    case Success(()) =>
      Com.send("ok:")
    case Success(v) =>
      Com.send("ok:" + v)
    case Failure(e) =>
      val data = js.JSON.stringify(ThrowableSerializer.serialize(e))
      Com.send("fail:" + data)
  }

  protected def handleMsgImpl(cmd: String, strArg: => String): Unit

  protected def tasks2TaskInfos(tasks: Array[Task], runner: Runner): js.Any = {
    tasks.map { task =>
      val serTask = runner.serializeTask(task, taskDef =>
        js.JSON.stringify(TaskDefSerializer.serialize(taskDef)))

      lit(serializedTask = serTask,
          taskDef = TaskDefSerializer.serialize(task.taskDef),
          tags = task.tags.toJSArray)
    }.toJSArray
  }

}
