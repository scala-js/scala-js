/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jsenv.nodejs

import org.scalajs.jsenv._
import org.scalajs.jsenv.Utils.OptDeadline

import org.scalajs.core.ir.Utils.escapeJS

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.classpath._
import org.scalajs.core.tools.jsdep._
import org.scalajs.core.tools.logging._

import java.io.{ Console => _, _ }
import java.net._

import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.io.Source

class NodeJSEnv private (
  nodejsPath: String,
  addArgs: Seq[String],
  addEnv: Map[String, String],
  val sourceMap: Boolean
) extends ExternalJSEnv(addArgs, addEnv) with ComJSEnv {

  def this(nodejsPath: String = "node", addArgs: Seq[String] = Seq.empty,
      addEnv: Map[String, String] = Map.empty) = {
    this(nodejsPath, addArgs, addEnv, sourceMap = true)
  }

  def withSourceMap(sourceMap: Boolean): NodeJSEnv =
    new NodeJSEnv(nodejsPath, addArgs, addEnv, sourceMap)

  /** True, if the installed node executable supports source mapping.
   *
   *  Do `npm install source-map-support` if you need source maps.
   */
  lazy val hasSourceMapSupport: Boolean = {
    val code = new MemVirtualJSFile("source-map-support-probe.js")
      .withContent("""require('source-map-support').install();""")
    val runner =
      jsRunner(CompleteClasspath.empty, code, NullLogger, NullJSConsole)

    try {
      runner.run()
      true
    } catch {
      case t: ExternalJSEnv.NonZeroExitException =>
        false
    }
  }

  protected def vmName: String = "node.js"
  protected def executable: String = nodejsPath

  override def jsRunner(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole): JSRunner = {
    new NodeRunner(classpath, code, logger, console)
  }

  override def asyncRunner(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole): AsyncJSRunner = {
    new AsyncNodeRunner(classpath, code, logger, console)
  }

  override def comRunner(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole): ComJSRunner = {
    new ComNodeRunner(classpath, code, logger, console)
  }

  protected class NodeRunner(classpath: CompleteClasspath,
      code: VirtualJSFile, logger: Logger, console: JSConsole
  ) extends ExtRunner(classpath, code, logger, console)
       with AbstractNodeRunner

  protected class AsyncNodeRunner(classpath: CompleteClasspath,
      code: VirtualJSFile, logger: Logger, console: JSConsole
  ) extends AsyncExtRunner(classpath, code, logger, console)
       with AbstractNodeRunner

  protected class ComNodeRunner(classpath: CompleteClasspath,
      code: VirtualJSFile, logger: Logger, console: JSConsole
  ) extends AsyncNodeRunner(classpath, code, logger, console)
       with ComJSRunner {

    /** Retry-timeout to wait for the JS VM to connect */
    private final val acceptTimeout = 1000

    private[this] val serverSocket =
      new ServerSocket(0, 0, InetAddress.getByName(null)) // Loopback address
    private[this] var comSocket: Socket = _
    private[this] var jvm2js: DataOutputStream = _
    private[this] var js2jvm: DataInputStream = _

    private def comSetup = new MemVirtualJSFile("comSetup.js").withContent(
      s"""
      (function() {
        // The socket for communication
        var socket = null;
        // The callback where received messages go
        var recvCallback = null;

        // Buffers received data
        var inBuffer = new Buffer(0);

        function onData(data) {
          inBuffer = Buffer.concat([inBuffer, data]);
          tryReadMsg();
        }

        function tryReadMsg() {
          while (inBuffer.length >= 4) {
            var msgLen = inBuffer.readInt32BE(0);
            var byteLen = 4 + msgLen * 2;

            if (inBuffer.length < byteLen) return;
            var res = "";

            for (var i = 0; i < msgLen; ++i)
              res += String.fromCharCode(inBuffer.readInt16BE(4 + i * 2));

            inBuffer = inBuffer.slice(byteLen);

            recvCallback(res);
          }
        }

        global.scalajsCom = {
          init: function(recvCB) {
            if (socket !== null) throw new Error("Com already open");

            var net = require('net');
            recvCallback = recvCB;
            socket = net.connect(${serverSocket.getLocalPort});
            socket.on('data', onData);
            socket.on('error', function(err) {
              socket.end();
              // EPIPE on write is expected if the JVM closes
              if (err.syscall !== "write" || err.code !== "EPIPE")
                throw err;
            });
          },
          send: function(msg) {
            if (socket === null) throw new Error("Com not open");

            var len = msg.length;
            var buf = new Buffer(4 + len * 2);
            buf.writeInt32BE(len, 0);
            for (var i = 0; i < len; ++i)
              buf.writeInt16BE(msg.charCodeAt(i), 4 + i * 2);
            socket.write(buf);
          },
          close: function() {
            if (socket === null) throw new Error("Com not open");
            socket.end();
          }
        }
      }).call(this);
      """
    )

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
      serverSocket.close()
      if (jvm2js != null)
        jvm2js.close()
      if (js2jvm != null)
        js2jvm.close()
      if (comSocket != null)
        comSocket.close()
    }

    /** Waits until the JS VM has established a connection or terminates
     *  @return true if the connection was established
     */
    private def awaitConnection(): Boolean = {
      serverSocket.setSoTimeout(acceptTimeout)
      while (comSocket == null && isRunning) {
        try {
          comSocket = serverSocket.accept()
          jvm2js = new DataOutputStream(
              new BufferedOutputStream(comSocket.getOutputStream()))
          js2jvm = new DataInputStream(
              new BufferedInputStream(comSocket.getInputStream()))
        } catch {
          case to: SocketTimeoutException =>
        }
      }

      comSocket != null
    }

    override protected def initFiles(): Seq[VirtualJSFile] =
      super.initFiles :+ comSetup

    override protected def finalize(): Unit = close()
  }

  protected trait AbstractNodeRunner extends AbstractExtRunner {

    protected[this] val libCache = new VirtualFileMaterializer(true)

    /** File(s) to automatically install source-map-support.
     *  Is used by [[initFiles]], override to change/disable.
     */
    protected def installSourceMap(): Seq[VirtualJSFile] = {
      if (sourceMap) Seq(
          new MemVirtualJSFile("sourceMapSupport.js").withContent(
            """
            try {
              require('source-map-support').install();
            } catch (e) {}
            """
          )
      ) else Seq()
    }

    /** File(s) to hack console.log to prevent if from changing `%%` to `%`.
     *  Is used by [[initFiles]], override to change/disable.
     */
    protected def fixPercentConsole(): Seq[VirtualJSFile] = Seq(
        new MemVirtualJSFile("nodeConsoleHack.js").withContent(
          """
          // Hack console log to duplicate double % signs
          (function() {
            var oldLog = console.log;
            var newLog = function() {
              var args = arguments;
              if (args.length >= 1 && args[0] !== void 0 && args[0] !== null) {
                args[0] = args[0].toString().replace(/%/g, "%%");
              }
              oldLog.apply(console, args);
            };
            console.log = newLog;
          })();
          """
        )
    )

    /** File(s) to define `__ScalaJSEnv`. Defines `exitFunction`.
     *  Is used by [[initFiles]], override to change/disable.
     */
    protected def runtimeEnv(): Seq[VirtualJSFile] = Seq(
        new MemVirtualJSFile("scalaJSEnvInfo.js").withContent(
          """
          __ScalaJSEnv = {
            exitFunction: function(status) { process.exit(status); }
          };
          """
        )
    )

    /** Concatenates results from [[installSourceMap]], [[fixPercentConsole]] and
     *  [[runtimeEnv]] (in this order).
     */
    override protected def initFiles(): Seq[VirtualJSFile] =
      installSourceMap() ++ fixPercentConsole() ++ runtimeEnv()

    /** Libraries are loaded via require in Node.js */
    override protected def getLibJSFiles(): Seq[VirtualJSFile] = {
      initFiles() ++
      classpath.jsLibs.map(requireLibrary) :+
      classpath.scalaJSCode
    }

    /** Rewrites a library virtual file to a require statement if possible */
    protected def requireLibrary(dep: ResolvedJSDependency): VirtualJSFile = {
      dep.info.commonJSName.fold(dep.lib) { varname =>
        val fname = dep.lib.name
        libCache.materialize(dep.lib)
        new MemVirtualJSFile(s"require-$fname").withContent(
          s"""$varname = require("${escapeJS(fname)}");"""
        )
      }
    }

    // Send code to Stdin
    override protected def sendVMStdin(out: OutputStream): Unit = {
      sendJS(getJSFiles(), out)
    }

    /** write a single JS file to a writer using an include fct if appropriate
     *  uses `require` if the file exists on the filesystem
     */
    override protected def writeJSFile(file: VirtualJSFile,
        writer: Writer): Unit = {
      file match {
        case file: FileVirtualJSFile =>
          val fname = file.file.getAbsolutePath
          writer.write(s"""require("${escapeJS(fname)}");\n""")
        case _ =>
          super.writeJSFile(file, writer)
      }
    }

    // Node.js specific (system) environment
    override protected def getVMEnv(): Map[String, String] = {
      val baseNodePath = sys.env.get("NODE_PATH").filter(_.nonEmpty)
      val nodePath = libCache.cacheDir.getAbsolutePath +
        baseNodePath.fold("")(p => File.pathSeparator + p)

      sys.env ++ Seq(
          "NODE_MODULE_CONTEXTS" -> "0",
          "NODE_PATH" -> nodePath
      ) ++ additionalEnv
    }
  }

}
