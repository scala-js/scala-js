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

package org.scalajs.testing.common

import scala.util.{Try, Failure, Success}

import scala.concurrent._
import scala.concurrent.duration._

import java.io._
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

import Serializer.{serialize, deserialize}
import FutureUtil._

/** Core RPC dispatcher.
 *
 *  Tracks and assigns call identities on top of a message passing interface.
 *
 *  Note that it does not have timeout handling for calls. Users are expected to
 *  manage call teardown by calling [[close]] in case of failure. This typically
 *  means that subclasses need to put an explicit call to [[close]] once they
 *  are sure to not call [[handleMessage]] anymore.
 *
 *  This class guarantees that dispatch handles synchronously when
 *  [[handleMessage]] is called, so closing can be performed race-free.
 */
private[testing] abstract class RPCCore()(implicit ec: ExecutionContext) {
  import RPCCore._

  /** Pending calls. */
  private[this] val pending = new ConcurrentHashMap[Long, PendingCall]

  /** Reason why we are closing this RPCCore. If non-null, we are closing. */
  @volatile
  private[this] var closeReason: Throwable = _

  /** Next call ID we'll assign. */
  private[this] val nextID = new AtomicLong(0L)

  /** Currently registered enpoints. */
  private[this] val endpoints = new ConcurrentHashMap[OpCode, BoundEndpoint]

  /** Subclass should call this whenever a new message arrives */
  final protected def handleMessage(msg: String): Unit = {
    Serializer.withInputStream(msg) { in =>
      val opCode = in.readByte()

      def getPending(): Option[PendingCall] = {
        val callID = in.readLong()
        /* Note that `callID` might not be in `pending` anymore if it got
         * removed during a close operation. In this case we're not doing
         * anything.
         */
        Option(pending.remove(callID))
      }

      opCode match {
        case RPCCore.ReplyOK =>
          getPending().foreach { p =>
            import p._
            promise.complete(Try(deserialize[Resp](in)))
          }

        case RPCCore.ReplyErr =>
          getPending().foreach { p =>
            val throwable = Try(deserialize[Throwable](in)) match {
              case Success(t) => new RPCException(t)
              case Failure(t) => t
            }

            p.promise.failure(throwable)
          }

        case _ =>
          endpoints.get(opCode) match {
            case null =>
              /* Quick and dirty way to provide more error detail for certain
               * known problems.
               * This is not ideal, but the best we can do, since we do not know
               * all possible opCodes we could receive (we'd need something like
               * an opCode "domain").
               * For now this is good enough; if collisions happen in the
               * future, we can improve this.
               */
              val detail = opCode match {
                case JSEndpoints.msgSlave.opCode =>
                  "; " +
                  "The test adapter could not send a message to a slave, " +
                  "which probably happens because the slave terminated early, " +
                  "without waiting for the reply to a call to send(). " +
                  "This is probably a bug in the testing framework you are " +
                  "using. See also #3201."

                case _ =>
                  ""
              }

              throw new IllegalStateException(s"Unknown opcode: $opCode$detail")

            case bep: BoundMsgEndpoint =>
              val ep: bep.endpoint.type = bep.endpoint
              import ep._

              bep.exec(deserialize[Msg](in))

            case bep: BoundRPCEndpoint =>
              val callID = in.readLong()

              val ep: bep.endpoint.type = bep.endpoint
              import ep._

              futureFromTry(Try(deserialize[Req](in)))
                .flatMap(bep.exec)
                .onComplete(repl => send(makeReply(callID, repl)))
          }
      }
    }
  }

  /** Subclass needs to implement message sending. */
  protected def send(msg: String): Unit

  /** Used to send a message to the other end. */
  final def send(ep: MsgEndpoint)(msg: ep.Msg): Unit = {
    import ep._
    send(makeMsgMsg(opCode, msg))
  }

  /** Used to make an actual call to the other end. */
  final def call(ep: RPCEndpoint)(req: ep.Req): Future[ep.Resp] = {
    import ep._

    // Reserve an id for this call.
    val id = nextID.incrementAndGet()

    // Prepare message. We do this early in case it throws.
    val msg = makeRPCMsg(opCode, id, req)

    // Register pending call.
    val promise = Promise[Resp]
    val oldCall = pending.put(id, PendingCall(promise))

    if (oldCall != null) {
      val error = new AssertionError("Ran out of call ids!")
      close(error)
      throw error
    }

    if (closeReason != null) {
      /* In the meantime, someone closed the channel. Help closing.
       * We need this check to guard against a race between `call` and `close`.
       */
      helpClose()
    } else {
      // Actually send message.
      send(msg)
    }

    promise.future
  }

  final def attach(ep: MsgEndpoint)(ex: ep.Msg => Unit): Unit = {
    attach(new BoundMsgEndpoint {
      val endpoint: ep.type = ep
      val exec = ex
    })
  }

  /** Attaches the given method to the given (local) endpoint. */
  final def attach(ep: RPCEndpoint)(ex: ep.Req => ep.Resp): Unit = {
    attachAsync(ep)(x => futureFromTry(Try(ex(x))))
  }

  /** Attaches the given method to the given (local) endpoint. */
  final def attachAsync(ep: RPCEndpoint)(ex: ep.Req => Future[ep.Resp]): Unit = {
    attach(new BoundRPCEndpoint {
      val endpoint: ep.type = ep
      val exec = ex
    })
  }

  private final def attach(bep: BoundEndpoint): Unit = {
    val opCode = bep.endpoint.opCode
    val old = endpoints.put(opCode, bep)
    require(old == null, s"Duplicate endpoint for opcode $opCode.")
  }

  final def detach(ep: Endpoint): Unit = {
    val old = endpoints.remove(ep.opCode)
    require(old != null, "Endpoint was not attached.")
  }

  /** Close the communication channel.
   *
   *  This only affects the current calls (i.e. the client part of the
   *  interface). Endpoint attachment is unaffected.
   *
   *  It is permitted to call `close` multiple times. However, if the calls are
   *  concurrent and have different reasons, which pending calls get cancelled
   *  with which reasons is unspecified (but all of them will get cancelled).
   */
  def close(reason: Throwable): Unit = {
    closeReason = reason
    helpClose()
  }

  private def helpClose(): Unit = {
    /* Fix for #3128: explicitly upcast to java.util.Map so that the keySet()
     * method is binary compatible on JDK7.
     */
    val pendingCallIDs = (pending: java.util.Map[Long, _]).keySet()
    val exception = new ClosedException(closeReason)

    /* Directly use the Java Iterator because Scala's JavaConverters are
     * tricky to use across 2.12- and 2.13+.
     */
    val pendingCallIDsIter = pendingCallIDs.iterator()
    while (pendingCallIDsIter.hasNext()) {
      val callID = pendingCallIDsIter.next()
      for (failing <- Option(pending.remove(callID)))
        failing.promise.failure(exception)
    }
  }

  private def makeReply[T: Serializer](id: Long, result: Try[T]): String = {
    result.map(makeRPCMsg(ReplyOK, id, _)) match {
      case Success(m) => m
      case Failure(t) => makeRPCMsg(ReplyErr, id, t)
    }
  }

  private def makeRPCMsg[T: Serializer](opCode: OpCode, id: Long,
      payload: T): String = {
    Serializer.withOutputStream { out =>
      out.writeByte(opCode)
      out.writeLong(id)
      serialize(payload, out)
    }
  }

  private def makeMsgMsg[T: Serializer](opCode: OpCode, payload: T): String = {
    Serializer.withOutputStream { out =>
      out.writeByte(opCode)
      serialize(payload, out)
    }
  }
}

private[testing] object RPCCore {
  type OpCode = Byte

  /** Exception thrown if a remote invocation fails. */
  final case class RPCException(c: Throwable) extends Exception(null, c)

  /** Exception thrown if the channel got closed. */
  final case class ClosedException(c: Throwable) extends Exception(null, c)

  private val ReplyOK: Byte = 0.toByte
  private val ReplyErr: Byte = 1.toByte

  def isReservedOpCode(opc: OpCode): Boolean =
    opc == ReplyOK || opc == ReplyErr

  private sealed trait BoundEndpoint {
    val endpoint: Endpoint
  }

  private sealed trait BoundMsgEndpoint extends BoundEndpoint {
    val endpoint: MsgEndpoint
    val exec: endpoint.Msg => Unit
  }

  private sealed trait BoundRPCEndpoint extends BoundEndpoint {
    val endpoint: RPCEndpoint
    val exec: endpoint.Req => Future[endpoint.Resp]
  }

  private trait PendingCall {
    type Resp
    val promise: Promise[Resp]
    implicit val serializer: Serializer[Resp]
  }

  private object PendingCall {
    def apply[R](p: Promise[R])(implicit s: Serializer[R]): PendingCall = {
      new PendingCall {
        type Resp = R
        val promise: Promise[Resp] = p

        implicit val serializer: Serializer[R] = s
      }
    }
  }
}
