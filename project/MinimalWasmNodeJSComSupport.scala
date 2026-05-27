/*
 * Scala.js JS Envs (https://github.com/scala-js/scala-js-js-envs)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package build

import scala.concurrent._
import scala.util.{Failure, Success}
import scala.util.control.NonFatal

// TODO Replace this by a better execution context on the RunConfig.
import scala.concurrent.ExecutionContext.Implicits.global

import java.io._
import java.net._
import org.scalajs.jsenv._

private[build] final class MinimalWasmNodeJSComRun(run: JSRun, handleMessage: String => Unit,
    serverSocket: ServerSocket) extends JSComRun {
  import MinimalWasmNodeJSComRun._

  /** Promise that completes once the receiver thread is completed. */
  private[this] val promise = Promise[Unit]()

  @volatile
  private[this] var state: State = AwaitingConnection(Nil)

  // If the run completes, make sure we also complete.
  run.future.onComplete {
    case Failure(t) => forceClose(t)
    case Success(_) => onJSTerminated()
  }

  private[this] val receiver = new Thread {
    setName("MinimalWasmNodeJSComRun receiver")

    override def run(): Unit = {
      try {
        try {
          /* We need to await the connection unconditionally. Otherwise the JS end
           * might try to connect indefinitely.
           */
          awaitConnection()

          while (state != Closing) {
            state match {
              case s: AwaitingConnection =>
                throw new IllegalStateException(s"Unexpected state: $s")

              case Closing =>
                /* We can end up here if there is a race between the two read to
                 * state. Do nothing, loop will terminate.
                 */

              case Connected(_, _, js2jvm) =>
                try {
                  val len = js2jvm.readInt()
                  val carr = Array.fill(len)(js2jvm.readChar())
                  handleMessage(String.valueOf(carr))
                } catch {
                  case _: EOFException =>
                    // JS end terminated gracefully. Close.
                    close()
                }
            }
          }
        } catch {
          case _: IOException if state == Closing =>
            // We got interrupted by a graceful close.
            // This is OK.
        }

        /* Everything got closed. We wait for the run to terminate.
         * We need to wait in order to make sure that closing the
         * underlying run does not fail it.
         */
        MinimalWasmNodeJSComRun.this.run.future.foreach { _ =>
          MinimalWasmNodeJSComRun.this.run.close()
          promise.trySuccess(())
        }
      } catch {
        case t: Throwable => handleThrowable(t)
      }
    }
  }

  receiver.start()

  def future: Future[Unit] = promise.future

  def send(msg: String): Unit = synchronized {
    state match {
      case AwaitingConnection(msgs) =>
        state = AwaitingConnection(msg :: msgs)

      case Connected(_, jvm2js, _) =>
        try {
          writeMsg(jvm2js, msg)
          jvm2js.flush()
        } catch {
          case t: Throwable => handleThrowable(t)
        }

      case Closing => // ignore msg.
    }
  }

  def close(): Unit = synchronized {
    val oldState = state

    // Signal receiver thread that it is OK if socket read fails.
    state = Closing

    oldState match {
      case c: Connected =>
        // Interrupts the receiver thread and signals the VM to terminate.
        closeAll(c)

      case Closing | _:AwaitingConnection =>
    }
  }

  private def onJSTerminated() = {
    close()

    /* Interrupt receiver if we are still waiting for connection.
     * Should only be relevant if we are still awaiting the connection.
     * Note: We cannot do this in close(), otherwise if the JVM side closes
     * before the JS side connected, the JS VM will fail instead of terminate
     * normally.
     */
    serverSocket.close()
  }

  private def forceClose(cause: Throwable) = {
    promise.tryFailure(cause)
    close()
    run.close()
    serverSocket.close()
  }

  private def handleThrowable(cause: Throwable) = {
    forceClose(cause)
    if (!NonFatal(cause))
      throw cause
  }

  private def awaitConnection(): Unit = {
    var comSocket: Socket = null
    var jvm2js: DataOutputStream = null
    var js2jvm: DataInputStream = null

    try {
      comSocket = serverSocket.accept()
      serverSocket.close()  // we don't need it anymore.
      jvm2js = new DataOutputStream(
          new BufferedOutputStream(comSocket.getOutputStream()))
      js2jvm = new DataInputStream(
          new BufferedInputStream(comSocket.getInputStream()))

      onConnected(Connected(comSocket, jvm2js, js2jvm))
    } catch {
      case t: Throwable =>
        closeAll(comSocket, jvm2js, js2jvm)
        throw t
    }
  }

  private def onConnected(c: Connected): Unit = synchronized {
    state match {
      case AwaitingConnection(msgs) =>
        msgs.reverse.foreach(writeMsg(c.jvm2js, _))
        c.jvm2js.flush()
        state = c

      case _: Connected =>
        throw new IllegalStateException(s"Unexpected state: $state")

      case Closing =>
        closeAll(c)
    }
  }
}

private[build] object MinimalWasmNodeJSComRun {
  /** Starts a [[JSComRun]] using the provided [[JSRun]] launcher.
   *
   *  @param config Configuration for the run.
   *  @param onMessage callback upon message reception.
   *  @param startRun [[JSRun]] launcher. Gets passed the TCP port that the
   *      Wasm runner must connect to.
   */
  def start(config: RunConfig, onMessage: String => Unit)(startRun: Int => JSRun): JSComRun = {
    try {
      val serverSocket =
        new ServerSocket(0, 0, InetAddress.getByName("127.0.0.1")) // IPv4 loopback address

      val run = startRun(serverSocket.getLocalPort)

      new MinimalWasmNodeJSComRun(run, onMessage, serverSocket)
    } catch {
      case NonFatal(t) => JSComRun.failed(t)
    }
  }

  private def closeAll(c: Closeable*): Unit =
    c.withFilter(_ != null).foreach(_.close())

  private def closeAll(c: Connected): Unit =
    closeAll(c.comSocket, c.jvm2js, c.js2jvm)

  private sealed trait State

  private final case class AwaitingConnection(
      sendQueue: List[String]) extends State

  private final case class Connected(
      comSocket: Socket,
      jvm2js: DataOutputStream,
      js2jvm: DataInputStream) extends State

  private case object Closing extends State

  private def writeMsg(s: DataOutputStream, msg: String): Unit = {
    s.writeInt(msg.length)
    s.writeChars(msg)
  }

  def setupContent(port: Int): String = {
    s"""
       |const net = require("node:net");
       |
       |// The socket for communication
       |const socket = net.connect($port, "127.0.0.1");
       |
       |// Buffers received data
       |let inBuffer = Buffer.alloc(0);
       |
       |// The exported Wasm receiver, once the module is instantiated
       |let receive = null;
       |
       |// Buffers received messages
       |let inMessages = [];
       |
       |socket.on("data", function(data) {
       |  inBuffer = Buffer.concat([inBuffer, data]);
       |
       |  while (inBuffer.length >= 4) {
       |    const msgLen = inBuffer.readInt32BE(0);
       |    const byteLen = 4 + msgLen * 2;
       |
       |    if (inBuffer.length < byteLen) return;
       |
       |    const msg = wasmI16Array.create(msgLen);
       |    for (let i = 0; i < msgLen; ++i)
       |      wasmI16Array.set(msg, i, inBuffer.readUInt16BE(4 + i * 2));
       |    inBuffer = inBuffer.slice(byteLen);
       |    if (receive === null) inMessages.push(msg);
       |    else receive(msg);
       |  }
       |});
       |
       |socket.on("error", function(err) {
       |  console.error("Scala.js MinimalWasm Com failed: " + err);
       |  process.exit(-1);
       |});
       |
       |socket.on("close", function() { process.exit(0); });
       |
       |importsObj["scalajs:testing/com"] = {
       |  send: function(msg) {
       |    const len = wasmI16Array.length(msg);
       |    const buf = Buffer.allocUnsafe(4 + len * 2);
       |    buf.writeInt32BE(len, 0);
       |    for (let i = 0; i < len; ++i)
       |      buf.writeUInt16BE(wasmI16Array.get(msg, i), 4 + i * 2);
       |    socket.write(buf);
       |  },
       |};
       |
       |function afterInstantiate(result) {
       |  const newReceive = result.instance.exports["scalajs:testing/com/receive"];
       |  if (newReceive !== undefined) {
       |    receive = newReceive;
       |    const queuedMessages = inMessages;
       |    inMessages = null;
       |    for (let i = 0; i < queuedMessages.length; ++i)
       |      receive(queuedMessages[i]);
       |  }
       |  return result;
       |}
    """.stripMargin
  }
}
