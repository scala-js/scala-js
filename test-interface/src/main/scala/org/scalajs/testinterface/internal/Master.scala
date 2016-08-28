package org.scalajs.testinterface.internal

import scala.scalajs.js
import js.Dynamic.{literal => lit}
import js.annotation.JSExport

import sbt.testing._

import scala.util.Try

import org.scalajs.testinterface.ScalaJSClassLoader

@JSExport
final class Master(frameworkName: String) extends BridgeBase(frameworkName) {

  private[this] var runner: Runner = _

  protected def handleMsgImpl(cmd: String, strArg: => String): Unit = {
    def jsonArg = js.JSON.parse(strArg)
    cmd match {
      case "newRunner" =>
        reply(newRunner(jsonArg))
      case "runnerDone" =>
        reply(runnerDone())
      case "tasks" =>
        reply(tasks(jsonArg))
      case "msg" =>
        reply(inboundMessage(strArg))
      case cmd =>
        throw new IllegalArgumentException(s"Unknown command: $cmd")
    }
  }

  // Message handler methods

  private def newRunner(data: js.Dynamic): Try[Unit] = {
    val args = data.args.asInstanceOf[js.Array[String]].toArray
    val remoteArgs = data.remoteArgs.asInstanceOf[js.Array[String]].toArray
    val loader = new ScalaJSClassLoader(
        scala.scalajs.runtime.environmentInfo.exportsNamespace)

    Try(runner = framework.runner(args, remoteArgs, loader))
  }

  private def runnerDone(): Try[String] = {
    ensureRunnerExists()

    val result = Try(runner.done())
    runner = null
    result
  }

  private def tasks(data: js.Dynamic): Try[String] = {
    ensureRunnerExists()

    val taskDefs = data.asInstanceOf[js.Array[js.Dynamic]]
      .map(TaskDefSerializer.deserialize).toArray

    Try {
      val tasks = runner.tasks(taskDefs)
      js.JSON.stringify(tasks2TaskInfos(tasks, runner))
    }
  }

  private def inboundMessage(msg: String): Try[String] = {
    ensureRunnerExists()
    Try(runner.receiveMessage(msg).fold(":n")(":s:" + _))
  }

  // Utility methods

  private def ensureRunnerExists(): Unit = {
    if (runner == null)
      throw new IllegalStateException("No runner created")
  }

}
