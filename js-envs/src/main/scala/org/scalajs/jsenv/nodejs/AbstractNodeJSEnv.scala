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

package org.scalajs.jsenv.nodejs

import scala.annotation.tailrec

import java.io.{Console => _, _}
import java.net._

import org.scalajs.core.ir.Utils.escapeJS
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.jsdep.ResolvedJSDependency
import org.scalajs.core.tools.logging.NullLogger
import org.scalajs.jsenv._
import org.scalajs.jsenv.Utils.OptDeadline

import scala.collection.JavaConverters._
import scala.concurrent.TimeoutException
import scala.concurrent.duration._

abstract class AbstractNodeJSEnv(
    @deprecatedName('nodejsPath)
    protected val executable: String,
    @deprecatedName('addArgs)
    args: Seq[String],
    @deprecatedName('addEnv)
    env: Map[String, String],
    val sourceMap: Boolean)
    extends ExternalJSEnv(args, env) with ComJSEnv {

  /** True, if the installed node executable supports source mapping.
   *
   *  Do `npm install source-map-support` if you need source maps.
   */
  lazy val hasSourceMapSupport: Boolean = {
    val code = new MemVirtualJSFile("source-map-support-probe.js")
      .withContent("""require('source-map-support').install();""")

    try {
      jsRunner(code).run(NullLogger, NullJSConsole)
      true
    } catch {
      case t: ExternalJSEnv.NonZeroExitException =>
        false
    }
  }

  /** Retry-timeout to wait for the JS VM to connect */
  protected val acceptTimeout = 500

  protected trait AbstractNodeRunner extends AbstractExtRunner with JSInitFiles {

    protected[this] val libCache = new VirtualFileMaterializer(true)

    /** File(s) to automatically install source-map-support.
     *  Is used by [[initFiles]], override to change/disable.
     */
    protected def installSourceMap(): Seq[VirtualJSFile] = {
      if (sourceMap) Seq(
        new MemVirtualJSFile("sourceMapSupport.js").withContent(
          """
            |try {
            |  require('source-map-support').install();
            |} catch (e) {}
          """.stripMargin
        )
      ) else Seq()
    }

    /** File(s) to hack console.log to prevent if from changing `%%` to `%`.
     *  Is used by [[initFiles]], override to change/disable.
     */
    protected def fixPercentConsole(): Seq[VirtualJSFile] = Seq(
      new MemVirtualJSFile("nodeConsoleHack.js").withContent(
        """
          |// Hack console log to duplicate double % signs
          |(function() {
          |  function startsWithAnyOf(s, prefixes) {
          |    for (var i = 0; i < prefixes.length; i++) {
          |      // ES5 does not have .startsWith() on strings
          |      if (s.substring(0, prefixes[i].length) === prefixes[i])
          |        return true;
          |    }
          |    return false;
          |  }
          |  var nodeWillDeduplicateEvenForOneArgument = startsWithAnyOf(
          |      process.version, ["v0.", "v1.", "v2.0."]);
          |  var oldLog = console.log;
          |  var newLog = function() {
          |    var args = arguments;
          |    if (args.length >= 1 && args[0] !== void 0 && args[0] !== null) {
          |      var argStr = args[0].toString();
          |      if (args.length > 1 || nodeWillDeduplicateEvenForOneArgument)
          |        argStr = argStr.replace(/%/g, "%%");
          |      args[0] = argStr;
          |    }
          |    oldLog.apply(console, args);
          |  };
          |  console.log = newLog;
          |})();
        """.stripMargin
      )
    )


    /** File(s) to define `__ScalaJSEnv`. Defines `exitFunction`.
     *  Is used by [[initFiles]], override to change/disable.
     */
    protected def runtimeEnv(): Seq[VirtualJSFile] = Seq(
      new MemVirtualJSFile("scalaJSEnvInfo.js").withContent(
        """
          |__ScalaJSEnv = {
          |  exitFunction: function(status) { process.exit(status); }
          |};
        """.stripMargin
      )
    )

    override protected def initFiles(): Seq[VirtualJSFile] =
      installSourceMap() ++ fixPercentConsole() ++ runtimeEnv()

    /** write a single JS file to a writer using an include fct if appropriate
     *  uses `require` if the file exists on the filesystem
     */
    override protected def writeJSFile(file: VirtualJSFile,
        writer: Writer): Unit = {

      def writeImport(file: File): Unit = {
        val uri = file.toURI.toASCIIString
        val importerFile = new MemVirtualJSFile("importer.js")
        importerFile.content = {
          s"""
            |import("${escapeJS(uri)}").catch(e => {
            |  /* Make sure to fail the process, but give time to Node.js to
            |   * display a good stack trace before that.
            |   */
            |  setTimeout(() => process.exit(1), 100);
            |  throw e;
            |});
          """.stripMargin
        }
        val f = libCache.materialize(importerFile)
        writer.write(s"""require("${escapeJS(f.getAbsolutePath)}");\n""")
      }

      file match {
        case file: FileVirtualJSFile =>
          val fname = file.file.getAbsolutePath
          if (fname.endsWith(".mjs"))
            writeImport(file.file)
          else
            writer.write(s"""require("${escapeJS(fname)}");\n""")
        case _ =>
          if (file.path.endsWith(".mjs"))
            writeImport(libCache.materialize(file))
          else
            super.writeJSFile(file, writer)
      }
    }

    // Node.js specific (system) environment
    override protected def getVMEnv(): Map[String, String] = {
      val baseNodePath = Option(System.getenv("NODE_PATH")).filter(_.nonEmpty)
      val nodePath = libCache.cacheDir.getAbsolutePath +
          baseNodePath.fold("")(p => File.pathSeparator + p)

      System.getenv().asScala.toMap ++ Seq(
        "NODE_MODULE_CONTEXTS" -> "0",
        "NODE_PATH" -> nodePath
      ) ++ env
    }
  }

  protected trait NodeComJSRunner extends ComJSRunner with JSInitFiles {

    /* Manipulation of the socket must be protected by synchronized, except
     * calls to `close()`.
     */
    private[this] val serverSocket =
      new ServerSocket(0, 0, InetAddress.getByName(null)) // Loopback address

    /* Those 3 fields are assigned *once* under synchronization in
     * `awaitConnection()`.
     * Read access must be protected by synchronized, or be done after a
     * successful call to `awaitConnection()`.
     */
    private var comSocket: Socket = _
    private var jvm2js: DataOutputStream = _
    private var js2jvm: DataInputStream = _

    abstract override protected def initFiles(): Seq[VirtualJSFile] =
      super.initFiles :+ comSetup

    private def comSetup(): VirtualJSFile = {
      new MemVirtualJSFile("comSetup.js").withContent(
          s"""
             |(function() {
             |  // The socket for communication
             |  var socket = null;
             |  // The callback where received messages go
             |  var recvCallback = null;
             |
             |  // Buffers received data
             |  var inBuffer = Buffer.alloc(0);
             |
             |  function onData(data) {
             |    inBuffer = Buffer.concat([inBuffer, data]);
             |    tryReadMsg();
             |  }
             |
             |  function tryReadMsg() {
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
             |  }
             |
             |  global.scalajsCom = {
             |    init: function(recvCB) {
             |      if (socket !== null) throw new Error("Com already open");
             |
             |      var net = require('net');
             |      recvCallback = recvCB;
             |      socket = net.connect(${serverSocket.getLocalPort});
             |      socket.on('data', onData);
             |      socket.on('error', function(err) {
             |        // Whatever happens, this closes the Com
             |        socket.end();
             |
             |        // Expected errors:
             |        // - EPIPE on write: JVM closes
             |        // - ECONNREFUSED on connect: JVM closes before JS opens
             |        var expected = (
             |            err.syscall === "write"   && err.code === "EPIPE" ||
             |            err.syscall === "connect" && err.code === "ECONNREFUSED"
             |        );
             |
             |        if (!expected) {
             |          console.error("Scala.js Com failed: " + err);
             |          // We must terminate with an error
             |          process.exit(-1);
             |        }
             |      });
             |    },
             |    send: function(msg) {
             |      if (socket === null) throw new Error("Com not open");
             |
             |      var len = msg.length;
             |      var buf = Buffer.allocUnsafe(4 + len * 2);
             |      buf.writeInt32BE(len, 0);
             |      for (var i = 0; i < len; ++i)
             |        buf.writeUInt16BE(msg.charCodeAt(i), 4 + i * 2);
             |      socket.write(buf);
             |    },
             |    close: function() {
             |      if (socket === null) throw new Error("Com not open");
             |      socket.end();
             |    }
             |  }
             |}).call(this);
          """.stripMargin)
    }

    def send(msg: String): Unit = {
      if (awaitConnection()) {
        jvm2js.writeInt(msg.length)
        jvm2js.writeChars(msg)
        jvm2js.flush()
      }
    }

    def receive(timeout: Duration): String = {
      if (!awaitConnection())
        throw new ComJSEnv.ComClosedException("Node.js isn't connected")

      js2jvm.mark(Int.MaxValue)
      val savedSoTimeout = comSocket.getSoTimeout()
      try {
        val optDeadline = OptDeadline(timeout)

        comSocket.setSoTimeout((optDeadline.millisLeft min Int.MaxValue).toInt)
        val len = js2jvm.readInt()
        val carr = Array.fill(len) {
          comSocket.setSoTimeout((optDeadline.millisLeft min Int.MaxValue).toInt)
          js2jvm.readChar()
        }

        js2jvm.mark(0)
        String.valueOf(carr)
      } catch {
        case e: EOFException =>
          throw new ComJSEnv.ComClosedException(e)
        case e: SocketTimeoutException =>
          js2jvm.reset()
          throw new TimeoutException("Timeout expired")
      } finally {
        comSocket.setSoTimeout(savedSoTimeout)
      }
    }

    def close(): Unit = {
      /* Close the socket first. This will cause any existing and upcoming
       * calls to `awaitConnection()` to be canceled and throw a
       * `SocketException` (unless it has already successfully completed the
       * `accept()` call).
       */
      serverSocket.close()

      /* Now wait for a possibly still-successful `awaitConnection()` to
       * complete before closing the sockets.
       */
      synchronized {
        if (comSocket != null) {
          jvm2js.close()
          js2jvm.close()
          comSocket.close()
        }
      }
    }

    /** Waits until the JS VM has established a connection or terminates
     *
     *  @return true if the connection was established
     */
    private def awaitConnection(): Boolean = synchronized {
      if (comSocket != null) {
        true
      } else {
        @tailrec
        def acceptLoop(): Option[Socket] = {
          if (!isRunning) {
            None
          } else {
            try {
              Some(serverSocket.accept())
            } catch {
              case to: SocketTimeoutException => acceptLoop()
            }
          }
        }

        serverSocket.setSoTimeout(acceptTimeout)
        val optComSocket = acceptLoop()

        optComSocket.fold {
          false
        } { comSocket0 =>
          val jvm2js0 = new DataOutputStream(
              new BufferedOutputStream(comSocket0.getOutputStream()))
          val js2jvm0 = new DataInputStream(
              new BufferedInputStream(comSocket0.getInputStream()))

          /* Assign those three fields together, without the possibility of
           * an exception happening in the middle (see #3408).
           */
          comSocket = comSocket0
          jvm2js = jvm2js0
          js2jvm = js2jvm0

          true
        }
      }
    }

    override protected def finalize(): Unit = close()
  }
}
