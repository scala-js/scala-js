package scala.scalajs.sbtplugin.testing

import scala.scalajs.tools.json._
import scala.scalajs.tools.env._

import scala.annotation.tailrec

private[testing] object ComUtils {

  type Handler[+T] = PartialFunction[(String, String), T]
  type LoopHandler[+T] = Handler[Option[T]]

  @tailrec
  def receiveLoop[T](com: ComJSRunner)(handler: LoopHandler[T]): T = {
    receiveResponse(com)(handler) match {
      case Some(v) => v
      case None => receiveLoop(com)(handler)
    }
  }

  def receiveResponse[T](com: ComJSRunner)(handler: Handler[T]): T = {
    val resp = {
      try com.receive()
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
