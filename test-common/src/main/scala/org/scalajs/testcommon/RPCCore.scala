package org.scalajs.testcommon

import scala.util.{Try, Failure, Success}

import scala.collection.JavaConverters._
import scala.concurrent._
import scala.concurrent.duration._

import java.io._
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

import Serializer.{serialize, deserialize}

private[scalajs] abstract class RPCCore {
  import RPCCore._

  /** Pending calls.
   *
   *  @note We do deliberately not timeout calls in here since there will be
   *      timeouts on a higher level and a test run is relatively short lived
   *      (< 10h) and we expect failure ratio to be extremely low.
   */
  private[this] val pending = new ConcurrentHashMap[Long, PendingCall]

  private[this] val nextID = new AtomicLong(0L)

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
              throw new IllegalStateException(s"Unknown opcode: $opCode")

            case bep: BoundMsgEndpoint =>
              val ep: bep.endpoint.type = bep.endpoint
              import ep._

              bep.exec(deserialize[Msg](in))

            case bep: BoundRPCEndpoint =>
              val callID = in.readLong()

              val ep: bep.endpoint.type = bep.endpoint
              import ep._

              import scala.concurrent.ExecutionContext.Implicits.global

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

    // Prepare message.
    val msg = makeRPCMsg(opCode, id, req)

    // Register pending call.
    val promise = Promise[Resp]
    val oldCall = pending.put(id, PendingCall(promise))

    if (oldCall != null) {
      throw new AssertionError("Ran out of call ids!")
    }

    // Actually send message.
    send(msg)

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

  /** Close the communication channel. */
  def close(): Unit = {
    /* Fix for #3128: explicitly upcast to java.util.Map so that the keySet()
     * method is binary compatible on JDK7.
     */
    val pendingCallIDs = (pending: java.util.Map[Long, _]).keySet()

    for {
      callID <- pendingCallIDs.asScala
      failing <- Option(pending.remove(callID))
    } {
      failing.promise.failure(new IOException("Channel got closed"))
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

  /** Same as Future.fromTry(x) but works in 2.10 */
  private def futureFromTry[T](x: Try[T]): Future[T] = {
    val promise = Promise[T]
    promise.complete(x)
    promise.future
  }
}

private[scalajs] object RPCCore {
  type OpCode = Byte

  /** Exception thrown if a remote invocation fails. */
  class RPCException(c: Throwable) extends Exception(null, c)

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
