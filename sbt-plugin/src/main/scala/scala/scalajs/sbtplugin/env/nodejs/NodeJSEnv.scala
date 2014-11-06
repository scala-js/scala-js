/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.env.nodejs

import scala.scalajs.sbtplugin.env._
import scala.scalajs.sbtplugin.JSUtils.toJSstr

import scala.scalajs.tools.io._
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.env._
import scala.scalajs.tools.jsdep._
import scala.scalajs.tools.logging._

import scala.scalajs.sbtplugin.JSUtils._

import java.io.{ Console => _, _ }
import java.net._

import scala.io.Source

class NodeJSEnv(
  nodejsPath: String = "node",
  addArgs:    Seq[String] = Seq.empty,
  addEnv:     Map[String, String] = Map.empty
) extends ExternalJSEnv(addArgs, addEnv) with ComJSEnv {

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
          if (inBuffer.length < 2) return;
          var msgLen = inBuffer.readInt16BE(0);
          var byteLen = (msgLen + 1) * 2;

          if (inBuffer.length < byteLen) return;
          var res = "";

          for (var i = 0; i < msgLen; ++i)
            res += String.fromCharCode(inBuffer.readInt16BE(2*(i+1)));

          inBuffer = inBuffer.slice(byteLen);

          recvCallback(res);
        }

        global.scalajsCom = {
          init: function(recvCB) {
            if (socket !== null) throw new Error("Com already open");

            var net = require('net');
            recvCallback = recvCB;
            socket = net.connect(${serverSocket.getLocalPort});
            socket.on('data', onData);
          },
          send: function(msg) {
            if (socket === null) throw new Error("Com not open");

            var len = msg.length;
            var buf = new Buffer(2*(len+1));
            buf.writeInt16BE(len, 0);
            for (var i = 0; i < len; ++i)
              buf.writeInt16BE(msg.charCodeAt(i), (i+1)*2);
            socket.write(buf);
          },
          close: function() {
            if (socket === null) throw new Error("Com not open");
            socket.end();
            socket.unref();
          }
        }
      }).call(this);
      """
    )

    def send(msg: String): Unit = {
      if (awaitConnection()) {
        jvm2js.writeShort(msg.length)
        jvm2js.writeChars(msg)
        jvm2js.flush()
      }
    }

    def receive(): String = {
      if (!awaitConnection())
        throw new ComJSEnv.ComClosedException
      try {
        val len = js2jvm.readShort()
        val carr = Array.fill(len)(js2jvm.readChar())
        String.valueOf(carr)
      } catch {
        case e: EOFException =>
          throw new ComJSEnv.ComClosedException
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
    protected def installSourceMap(): Seq[VirtualJSFile] = Seq(
        new MemVirtualJSFile("sourceMapSupport.js").withContent(
          """
          try {
            require('source-map-support').install();
          } catch (e) {}
          """
        )
    )

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
      classpath.jsLibs.map((requireLibrary _).tupled) :+
      classpath.scalaJSCode
    }

    /** Rewrites a library virtual file to a require statement if possible */
    protected def requireLibrary(vf: VirtualJSFile,
        info: ResolutionInfo): VirtualJSFile = {
      info.commonJSName.fold(vf) { varname =>
        val fname = vf.name
        libCache.materialize(vf)
        new MemVirtualJSFile(s"require-$fname").withContent(
          s"""$varname = require(${toJSstr(fname)});"""
        )
      }
    }

    // Send code to Stdin
    override protected def sendVMStdin(out: OutputStream): Unit = {
      sendJS(getJSFiles(), out)
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
