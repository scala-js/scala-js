/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.testing.bridge

import scala.concurrent.{Future, Promise}

import scala.util.control.NonFatal
import scala.util.Try

import org.scalajs.testing.common._

import sbt.testing._

private[bridge] object TestAdapterBridge {

  private[this] val mux = new RunMuxRPC(JSRPC)

  def start(): Unit = {
    import JSEndpoints._

    JSRPC.attach(detectFrameworks)(detectFrameworksFun)
    JSRPC.attach(createMasterRunner)(createRunnerFun(isMaster = true))
    JSRPC.attach(createSlaveRunner)(createRunnerFun(isMaster = false))
  }

  private def detectFrameworksFun = { names: List[List[String]] =>
    FrameworkLoader.detectFrameworkNames(names).map { maybeName =>
      maybeName.map { name =>
        val framework = FrameworkLoader.loadFramework(name)
        new FrameworkInfo(name, framework.name, framework.fingerprints.toList)
      }
    }
  }

  private def createRunnerFun(isMaster: Boolean) = { args: RunnerArgs =>
    val framework = FrameworkLoader.loadFramework(args.frameworkImpl)
    val loader = new ScalaJSClassLoader()

    val runID = args.runID

    val runner = {
      if (isMaster) {
        framework.runner(args.args.toArray, args.remoteArgs.toArray, loader)
      } else {
        framework.slaveRunner(args.args.toArray, args.remoteArgs.toArray, loader,
            mux.send(JVMEndpoints.msgSlave, runID))
      }
    }

    mux.attach(JSEndpoints.tasks, runID)(tasksFun(runner))
    mux.attachAsync(JSEndpoints.execute, runID)(executeFun(runID, runner))
    mux.attach(JSEndpoints.done, runID)(doneFun(runID, runner, isMaster))

    if (isMaster) {
      mux.attach(JSEndpoints.msgMaster, runID)(msgMasterFun(runID, runner))
    } else {
      mux.attach(JSEndpoints.msgSlave, runID)(runner.receiveMessage _)
    }
  }

  private def detachRunnerCommands(runID: RunMux.RunID, isMaster: Boolean) = {
    mux.detach(JSEndpoints.tasks, runID)
    mux.detach(JSEndpoints.execute, runID)
    mux.detach(JSEndpoints.done, runID)

    if (isMaster)
      mux.detach(JSEndpoints.msgMaster, runID)
    else
      mux.detach(JSEndpoints.msgSlave, runID)
  }

  private def tasksFun(runner: Runner) = { taskDefs: List[TaskDef] =>
    val tasks = runner.tasks(taskDefs.toArray)
    tasks.map(TaskInfoBuilder.detachTask(_, runner)).toList
  }

  private def executeFun(runID: RunMux.RunID, runner: Runner) = { req: ExecuteRequest =>
    val task = TaskInfoBuilder.attachTask(req.taskInfo, runner)
    val eventHandler = new RemoteEventHandler(runID)

    val loggers = for {
      (withColor, i) <- req.loggerColorSupport.zipWithIndex
    } yield new RemoteLogger(runID, i, withColor)

    val promise = Promise[List[TaskInfo]]

    def cont(tasks: Array[Task]) = {
      val result = Try(tasks.map(TaskInfoBuilder.detachTask(_, runner)).toList)
      promise.complete(result)
    }

    try {
      task.execute(eventHandler, loggers.toArray, cont)
    } catch {
      case NonFatal(t) =>
        promise.tryFailure(t)
    }

    promise.future
  }

  private def doneFun(runID: RunMux.RunID, runner: Runner, isMaster: Boolean) = { _: Unit =>
    try runner.done()
    finally detachRunnerCommands(runID, isMaster)
  }

  private def msgMasterFun(runID: RunMux.RunID, runner: Runner) = { msg: FrameworkMessage =>
    for (reply <- runner.receiveMessage(msg.msg)) {
      val fm = new FrameworkMessage(msg.slaveId, reply)
      mux.send(JVMEndpoints.msgMaster, runID)(fm)
    }
  }

  private class RemoteEventHandler(runID: RunMux.RunID) extends EventHandler {
    def handle(event: Event): Unit = mux.send(JVMEndpoints.event, runID)(event)
  }

  private class RemoteLogger(runID: RunMux.RunID, index: Int,
      val ansiCodesSupported: Boolean) extends Logger {

    import JVMEndpoints._

    private def l[T](x: T) = new LogElement(index, x)

    def error(msg: String): Unit = mux.send(logError, runID)(l(msg))
    def warn(msg: String): Unit = mux.send(logWarn, runID)(l(msg))
    def info(msg: String): Unit = mux.send(logInfo, runID)(l(msg))
    def debug(msg: String): Unit = mux.send(logDebug, runID)(l(msg))
    def trace(t: Throwable): Unit = mux.send(logTrace, runID)(l(t))
  }
}
