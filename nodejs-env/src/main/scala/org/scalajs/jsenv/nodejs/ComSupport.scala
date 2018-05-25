/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Node.js env       **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2017, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jsenv.nodejs

import java.io._
import java.net._

import org.scalajs.io.{VirtualBinaryFile, MemVirtualBinaryFile}
import org.scalajs.jsenv._

import scala.collection.immutable
import scala.concurrent._
import scala.util.{Failure, Success}
import scala.util.control.NonFatal

// TODO Replace this by a better execution context on the RunConfig.
import scala.concurrent.ExecutionContext.Implicits.global

private final class ComRun(run: JSRun, handleMessage: String => Unit,
    serverSocket: ServerSocket) extends JSComRun {
  import ComRun._

  /** Promise that completes once the reciever thread is completed. */
  private[this] val promise = Promise[Unit]()

  @volatile
  private[this] var state: State = AwaitingConnection(Nil)

  // If the run completes, make sure we also complete.
  run.future.onComplete {
    case Failure(t) => forceClose(t)
    case Success(_) => onJSTerminated()
  }

  // TODO replace this with scheduled tasks on the execution context.
  private[this] val receiver = new Thread {
    setName("ComRun receiver")

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
        ComRun.this.run.future.foreach { _ =>
          ComRun.this.run.close()
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

object ComRun {
  /** Starts a [[JSComRun]] using the provided [[JSRun]] launcher.
   *
   *  @param config Configuration for the run.
   *  @param onMessage callback upon message reception.
   *  @param startRun [[JSRun]] launcher. Gets passed a
   *      [[org.scalajs.io.VirtualBinaryFile VirtualBinaryFile]] that
   *      initializes `scalaJSCom` on `global`. Requires Node.js libraries.
   */
  def start(config: RunConfig, onMessage: String => Unit)(
      startRun: VirtualBinaryFile => JSRun): JSComRun = {
    try {
      val serverSocket =
        new ServerSocket(0, 0, InetAddress.getByName(null)) // Loopback address

      val run = startRun(setupFile(serverSocket.getLocalPort))

      new ComRun(run, onMessage, serverSocket)
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

  private final case object Closing extends State

  private def writeMsg(s: DataOutputStream, msg: String): Unit = {
    s.writeInt(msg.length)
    s.writeChars(msg)
  }

  private def setupFile(port: Int): VirtualBinaryFile = {
    MemVirtualBinaryFile.fromStringUTF8("comSetup.js",
        s"""
           |(function() {
           |  // The socket for communication
           |  var socket = require('net').connect($port);
           |
           |  // Buffers received data
           |  var inBuffer = new Buffer(0);
           |
           |  // Buffers received messages
           |  var inMessages = [];
           |
           |  // The callback where received messages go
           |  var recvCallback = function(msg) { inMessages.push(msg); };
           |
           |  socket.on('data', function(data) {
           |    inBuffer = Buffer.concat([inBuffer, data]);
           |
           |    while (inBuffer.length >= 4) {
           |      var msgLen = inBuffer.readInt32BE(0);
           |      var byteLen = 4 + msgLen * 2;
           |
           |      if (inBuffer.length < byteLen) return;
           |      var res = "";
           |
           |      for (var i = 0; i < msgLen; ++i)
           |        res += String.fromCharCode(inBuffer.readInt16BE(4 + i * 2));
           |
           |      inBuffer = inBuffer.slice(byteLen);
           |
           |      recvCallback(res);
           |    }
           |  });
           |
           |  socket.on('error', function(err) {
           |    console.error("Scala.js Com failed: " + err);
           |    process.exit(-1);
           |  });
           |
           |  socket.on('close', function() { process.exit(0); });
           |
           |  global.scalajsCom = {
           |    init: function(recvCB) {
           |      if (inMessages === null) throw new Error("Com already initialized");
           |      for (var i = 0; i < inMessages.length; ++i)
           |        recvCB(inMessages[i]);
           |      inMessages = null;
           |      recvCallback = recvCB;
           |    },
           |    send: function(msg) {
           |      var len = msg.length;
           |      var buf = new Buffer(4 + len * 2);
           |      buf.writeInt32BE(len, 0);
           |      for (var i = 0; i < len; ++i)
           |        buf.writeUInt16BE(msg.charCodeAt(i), 4 + i * 2);
           |      socket.write(buf);
           |    }
           |  }
           |}).call(this);
        """.stripMargin)
  }
}
