package org.scalajs.testinterface.internal

import scala.scalajs.js
import scala.scalajs.js.annotation._

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._

import org.scalajs.testcommon.RPCCore

/** JS RPC Core. Uses `scalajsCom`. */
private[internal] final object JSRPC extends RPCCore[Future] {
  Com.init(handleMessage _)

  override protected def send(msg: String): Unit = Com.send(msg)

  override protected def toFuture[T](p: Promise[T]): Future[T] = p.future

  override def close(): Unit = {
    super.close()
    Com.close()
  }
}
