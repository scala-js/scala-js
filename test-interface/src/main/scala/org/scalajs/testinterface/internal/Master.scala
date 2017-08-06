package org.scalajs.testinterface.internal

import scala.scalajs.js
import js.Dynamic.{literal => lit}
import js.annotation._

import sbt.testing._

import scala.concurrent.Future
import scala.util.Try

import org.scalajs.testcommon._
import org.scalajs.testinterface.ScalaJSClassLoader

@JSExportTopLevel("org.scalajs.testinterface.internal.Master")
final class Master(frameworkName: String) {

  private[this] var runner: Runner = _

  JSRPC.attach(JSMasterEndpoints.newRunner)(newRunner _)
  JSRPC.attach(JSMasterEndpoints.runnerDone)(runnerDone _)
  JSRPC.attach(JSMasterEndpoints.tasks)(tasks _)
  JSRPC.attach(JSMasterEndpoints.msg)(inboundMessage _)

  // Message handler methods

  private def newRunner(req: RunnerArgs): Unit = {
    val framework = FrameworkLoader.loadFramework(frameworkName)
    val loader = new ScalaJSClassLoader(
        scala.scalajs.runtime.environmentInfo.exportsNamespace)
    runner = framework.runner(req.args.toArray, req.remoteArgs.toArray, loader)
  }

  private def runnerDone(req: Unit): String = {
    ensureRunnerExists()

    try runner.done()
    finally runner = null
  }

  private def tasks(taskDefs: List[TaskDef]): List[TaskInfo] = {
    ensureRunnerExists()

    val tasks = runner.tasks(taskDefs.toArray)
    tasks.map(TaskInfoBuilder.detachTask(_, runner)).toList
  }

  private def inboundMessage(msg: FrameworkMessage): Unit = {
    ensureRunnerExists()
    for (reply <- runner.receiveMessage(msg.msg)) {
      JSRPC.send(JVMMasterEndpoints.msg)(
          new FrameworkMessage(msg.slaveId, reply))
    }
  }

  // Utility methods

  private def ensureRunnerExists(): Unit = {
    if (runner == null)
      throw new IllegalStateException("No runner created")
  }

}
