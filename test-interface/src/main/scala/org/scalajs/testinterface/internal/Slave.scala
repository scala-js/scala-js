package org.scalajs.testinterface.internal

import scala.scalajs.js
import js.annotation._

import sbt.testing._

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.util.control.NonFatal
import scala.util.Try

import org.scalajs.testcommon._
import org.scalajs.testinterface.ScalaJSClassLoader

@JSExportTopLevel("org.scalajs.testinterface.internal.Slave")
final class Slave(frameworkName: String, args: js.Array[String],
    remoteArgs: js.Array[String]) {

  private[this] var runner: Runner = _

  JSRPC.attach(JSSlaveEndpoints.newRunner)(newRunner _)
  JSRPC.attachAsync(JSSlaveEndpoints.execute)(execute _)
  JSRPC.attach(JSSlaveEndpoints.stopSlave)(stopSlave _)
  JSRPC.attach(JSSlaveEndpoints.msg)(receiveMessage _)

  // Message handler methods

  private def newRunner(req: Unit): Unit = {
    val framework = FrameworkLoader.loadFramework(frameworkName)
    val loader = new ScalaJSClassLoader(
        scala.scalajs.runtime.environmentInfo.exportsNamespace)
    runner = framework.slaveRunner(args.toArray, remoteArgs.toArray,
        loader, JSRPC.send(JVMSlaveEndpoints.msg))
  }

  private def execute(req: ExecuteRequest): Future[List[TaskInfo]] = {
    ensureRunnerExists()

    val task = TaskInfoBuilder.attachTask(req.taskInfo, runner)
    val eventHandler = new RemoteEventHandler

    val loggers = for {
      (withColor, i) <- req.loggerColorSupport.zipWithIndex
    } yield new RemoteLogger(i, withColor)

    val promise = Promise[List[TaskInfo]]

    def cont(tasks: Array[Task]) = {
      val result = Try(tasks.map(TaskInfoBuilder.detachTask(_, runner)).toList)
      eventHandler.invalidate()
      loggers.foreach(_.invalidate())
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

  private def stopSlave(req: Unit): Unit = {
    ensureRunnerExists()
    try runner.done()
    finally runner = null
  }

  private def receiveMessage(msg: String): Unit =
    runner.receiveMessage(msg)

  // Private helper classes

  private abstract class Invalidatable {
    private[this] var valid = true

    private[Slave] def invalidate(): Unit = valid = false

    protected def ensureValid(): Unit =
      if (!valid) throw new IllegalStateException(s"$this has been invalidated")
  }

  private class RemoteEventHandler extends Invalidatable with EventHandler {
    def handle(event: Event): Unit = {
      ensureValid()
      JSRPC.send(JVMSlaveEndpoints.event)(event)
    }
  }

  private class RemoteLogger(index: Int,
      val ansiCodesSupported: Boolean) extends Invalidatable with Logger {

    import JVMSlaveEndpoints._

    private def l[T](x: T) = new LogElement(index, x)

    def error(msg: String): Unit = JSRPC.send(logError)(l(msg))
    def warn(msg: String): Unit = JSRPC.send(logWarn)(l(msg))
    def info(msg: String): Unit = JSRPC.send(logInfo)(l(msg))
    def debug(msg: String): Unit = JSRPC.send(logDebug)(l(msg))
    def trace(t: Throwable): Unit = JSRPC.send(logTrace)(l(t))
  }

  // Utility methods

  private def ensureRunnerExists(): Unit = {
    if (runner == null)
      throw new IllegalStateException("No runner created")
  }

}
