package org.scalajs.testinterface.internal

import scala.scalajs.js
import js.annotation.JSExport

import sbt.testing._

import scala.collection.mutable

import scala.util.control.NonFatal
import scala.util.{Try, Success, Failure}

import org.scalajs.testinterface.ScalaJSClassLoader

@JSExport
final class Slave(frameworkName: String, args: js.Array[String],
    remoteArgs: js.Array[String]) extends BridgeBase(frameworkName) {

  // State

  private[this] var canSendRunnerMessage = false

  /** Queue for messages from the slave runner to the master runner */
  private[this] val messageQueue = mutable.Queue.empty[String]

  private[this] var runner: Runner = _

  protected def handleMsgImpl(cmd: String, strArg: => String): Unit = {
    def jsonArg = js.JSON.parse(strArg)
    allowSendRunnerMessage {
      cmd match {
        case "newRunner" =>
          reply(newRunner())
        case "execute" =>
          // No reply here. execute is async
          execute(jsonArg)
        case "stopSlave" =>
          reply(stopSlave())
        case "msg" =>
          val res = incomingRunnerMessage(strArg)
          // Only reply if something failed
          if (res.isFailure)
            reply(res)
        case cmd =>
          throw new IllegalArgumentException(s"Unknown command: $cmd")
      }
    }
  }

  // Runner message handler methods

  private def outboundRunnerMessage(msg: String): Unit =
    if (canSendRunnerMessage) sendOutboundRunnerMessage(msg)
    else messageQueue.enqueue(msg)

  private def sendOutboundRunnerMessage(msg: String): Unit = {
    assert(canSendRunnerMessage)
    Com.send(s"msg:$msg")
  }

  private def allowSendRunnerMessage[T](body: => T): T = {
    try {
      canSendRunnerMessage = true

      // Flush the queue
      while (!messageQueue.isEmpty)
        sendOutboundRunnerMessage(messageQueue.dequeue)

      body
    } finally {
      canSendRunnerMessage = false
    }
  }

  // Message handler methods

  private def newRunner(): Try[Unit] = {
    val loader = new ScalaJSClassLoader(
        scala.scalajs.runtime.environmentInfo.exportsNamespace)
    Try(runner = framework.slaveRunner(args.toArray, remoteArgs.toArray,
        loader, outboundRunnerMessage))
  }

  private def execute(data: js.Dynamic): Unit = {
    ensureRunnerExists()

    val sTask = data.serializedTask.asInstanceOf[String]
    val task = runner.deserializeTask(sTask, str =>
      TaskDefSerializer.deserialize(js.JSON.parse(str)))

    val eventHandler = new RemoteEventHandler

    val colorSupport = data.loggerColorSupport.asInstanceOf[js.Array[Boolean]]
    val loggers = for {
      (withColor, i) <- colorSupport.zipWithIndex
    } yield new RemoteLogger(i, withColor)

    def cont(tasks: Array[Task]) = {
      val result = Try(js.JSON.stringify(tasks2TaskInfos(tasks, runner)))
      eventHandler.invalidate()
      loggers.foreach(_.invalidate())
      reply(result)
    }

    val launched = Try(task.execute(eventHandler, loggers.toArray, cont))

    if (launched.isFailure)
      reply(launched)
  }

  private def stopSlave(): Try[Unit] = {
    ensureRunnerExists()
    val res = Try { runner.done(); () }
    runner = null
    res
  }

  private def incomingRunnerMessage(msg: String): Try[Unit] = {
    ensureRunnerExists()
    Try { runner.receiveMessage(msg); () }
  }

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
      val serEvent = EventSerializer.serialize(event)
      Com.send("event:" + js.JSON.stringify(serEvent))
    }
  }

  private class RemoteLogger(index: Int,
      val ansiCodesSupported: Boolean) extends Invalidatable with Logger {

    def error(msg: String): Unit = send("error", msg)
    def warn(msg: String): Unit = send("warn", msg)
    def info(msg: String): Unit = send("info", msg)
    def debug(msg: String): Unit = send("debug", msg)

    def trace(t: Throwable): Unit =
      send("trace", js.JSON.stringify(ThrowableSerializer.serialize(t)))

    private def send(cmd: String, data: String): Unit = {
      ensureValid()
      Com.send(s"$cmd:$index:$data")
    }
  }

  // Utility methods

  private def ensureRunnerExists(): Unit = {
    if (runner == null)
      throw new IllegalStateException("No runner created")
  }

}
