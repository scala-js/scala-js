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

package org.scalajs.testcommon

import scala.language.higherKinds

import scala.collection.mutable
import scala.concurrent.Future
import scala.util.Try

import java.util.concurrent.ConcurrentHashMap

import FutureUtil.futureFromTry

/** Helper above an [[RPCCore]] that allows to multiplex between runs.
 *
 *  Instead of registering/calling a single endpoint, it supports
 *  registering/calling the same endpoint once per run.
 *
 *  This is useful for functionality that needs to be dispatched to a specific
 *  runner.
 */
private[scalajs] final class RunMuxRPC(rpc: RPCCore) {
  import RunMux.RunID
  import RunMuxRPC._

  /** Multiplexer map.
   *
   *  Access to the outer map needs to synchronized.
   *  Access to the inner map only needs to be synchronize for writing.
   */
  private[this] val mux = mutable.Map.empty[RPCCore.OpCode, ConcurrentHashMap[RunID, _]]

  def call[Req](ep: MuxRPCEndpoint[Req], runId: RunID)(req: Req): Future[ep.Resp] =
    rpc.call(ep)(new RunMux(runId, req))

  def send[Msg](ep: MuxMsgEndpoint[Msg], runId: RunID)(msg: Msg): Unit =
    rpc.send(ep)(new RunMux(runId, msg))

  def attach[Msg](ep: MuxMsgEndpoint[Msg], runId: RunID)(ex: Msg => Unit): Unit =
    attachMux(ep.opCode, runId, ex)(rpc.attach(ep))

  def attach[Req](ep: MuxRPCEndpoint[Req], runId: RunID)(ex: Req => ep.Resp): Unit =
    attachAsync(ep, runId)(x => futureFromTry(Try(ex(x))))

  def attachAsync[Req](ep: MuxRPCEndpoint[Req], runId: RunID)(
      ex: Req => Future[ep.Resp]): Unit = {
    attachMux(ep.opCode, runId, ex)(rpc.attachAsync(ep))
  }

  private def attachMux[Req, Resp](
      opCode: RPCCore.OpCode, runId: RunID, ex: Req => Resp)(
      attach: (RunMux[Req] => Resp) => Unit): Unit = synchronized {
    type DispatchMap = ConcurrentHashMap[RunID, Req => Resp]

    def newDispatchMap() = {
      val dispatch = new DispatchMap

      attach { r =>
        Option(dispatch.get(r.runId)).fold {
          throw new IllegalArgumentException(s"Unknown run ${r.runId}")
        } { f =>
          f(r.value)
        }
      }

      dispatch
    }

    val dispatch = mux.getOrElseUpdate(opCode, newDispatchMap())
    val old = dispatch.asInstanceOf[DispatchMap].put(runId, ex)
    require(old == null, s"Duplicate endpoint for opcode $opCode run $runId")
  }

  def detach(ep: Endpoint, runId: RunID): Unit = synchronized {
    val opCode = ep.opCode

    val dispatch = mux.getOrElse(opCode, throw new IllegalArgumentException(
        s"No endpoint attached for opCode $opCode"))

    val old = dispatch.remove(runId)
    require(old != null, s"No endpoint attached for opCode $opCode run $runId")

    if (dispatch.isEmpty) {
      rpc.detach(ep)
      mux -= opCode
    }
  }
}

private[scalajs] object RunMuxRPC {
  // Helper types
  type MuxRPCEndpoint[T] = RPCEndpoint { type Req = RunMux[T] }
  type MuxMsgEndpoint[T] = MsgEndpoint { type Msg = RunMux[T] }
}
