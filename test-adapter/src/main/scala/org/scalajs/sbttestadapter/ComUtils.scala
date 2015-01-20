package org.scalajs.testadapter

import org.scalajs.core.tools.json._
import org.scalajs.jsenv._

import scala.annotation.tailrec

import scala.concurrent.duration._

private[testadapter] object ComUtils {

  type Handler[+T] = PartialFunction[(String, String), T]
  type LoopHandler[+T] = Handler[Option[T]]

  def receiveLoop[T](com: ComJSRunner)(handler: LoopHandler[T]): T =
    receiveLoop(com, Duration.Inf)(handler)

  @tailrec
  def receiveLoop[T](com: ComJSRunner, timeout: Duration)(
      handler: LoopHandler[T]): T = {
    receiveResponse(com, timeout)(handler) match {
      case Some(v) => v
      case None    => receiveLoop(com, timeout)(handler)
    }
  }

  @tailrec
  def receiveLoop[T](com: ComJSRunner, deadline: Deadline)(
      handler: LoopHandler[T]): T = {
    receiveResponse(com, deadline.timeLeft)(handler) match {
      case Some(v) => v
      case None    => receiveLoop(com, deadline)(handler)
    }
  }

  def receiveResponse[T](com: ComJSRunner)(handler: Handler[T]): T =
    receiveResponse(com, Duration.Inf)(handler)

  def receiveResponse[T](com: ComJSRunner, timeout: Duration)(
      handler: Handler[T]): T = {
    val resp = {
      try com.receive(timeout)
      catch {
        case t: ComJSEnv.ComClosedException =>
          // Check if runner failed. If it did, throw that exception instead
          if (!com.isRunning())
            com.await() // Will throw if runner failed

          throw t
      }
    }

    def badResponse(cause: Throwable = null) = {
      throw new AssertionError(
          s"JS test interface sent bad reply: $resp", cause)
    }

    val pos = resp.indexOf(':')

    if (pos == -1)
      badResponse()

    val status = resp.substring(0, pos)
    val data = resp.substring(pos + 1)

    def throwable = {
      try fromJSON[RemoteException](readJSON(data))
      catch {
        case t: Throwable => badResponse(t)
      }
    }

    def onFail = status match {
      case "fail" =>
        throw throwable
      case "bad" =>
        throw new AssertionError(
            s"JS test interface rejected command.", throwable)
      case _ =>
        badResponse()
    }

    val result = {
      try handler.lift((status, data))
      catch {
        case t: Throwable => badResponse(t)
      }
    }

    result.getOrElse(onFail)
  }

  val doneHandler: LoopHandler[Unit] = {
    case ("ok", "") => Some(())
  }

  val okHandler: Handler[Unit] = {
    case ("ok", "") =>
  }

}
