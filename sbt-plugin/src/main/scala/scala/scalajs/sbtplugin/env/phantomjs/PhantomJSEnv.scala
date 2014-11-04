/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.env.phantomjs

import scala.scalajs.sbtplugin.env._

import scala.scalajs.tools.io._
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.env._
import scala.scalajs.tools.logging._

import scala.scalajs.sbtplugin.JSUtils._

import java.io.{ Console => _, _ }
import java.net._

import scala.io.Source
import scala.collection.mutable

class PhantomJSEnv(
    phantomjsPath: String = "phantomjs",
    addArgs: Seq[String] = Seq.empty,
    addEnv: Map[String, String] = Map.empty,
    val autoExit: Boolean = true,
    jettyClassLoader: ClassLoader = getClass().getClassLoader()
) extends ExternalJSEnv(addArgs, addEnv) with ComJSEnv {

  protected def vmName: String = "PhantomJS"
  protected def executable: String = phantomjsPath

  override def jsRunner(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole): JSRunner = {
    new PhantomRunner(classpath, code, logger, console)
  }

  override def asyncRunner(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole): AsyncJSRunner = {
    new AsyncPhantomRunner(classpath, code, logger, console)
  }

  override def comRunner(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole): ComJSRunner = {
    new ComPhantomRunner(classpath, code, logger, console)
  }

  protected class PhantomRunner(classpath: CompleteClasspath,
      code: VirtualJSFile, logger: Logger, console: JSConsole
  ) extends ExtRunner(classpath, code, logger, console)
       with AbstractPhantomRunner

  protected class AsyncPhantomRunner(classpath: CompleteClasspath,
      code: VirtualJSFile, logger: Logger, console: JSConsole
  ) extends AsyncExtRunner(classpath, code, logger, console)
       with AbstractPhantomRunner

  protected class ComPhantomRunner(classpath: CompleteClasspath,
      code: VirtualJSFile, logger: Logger, console: JSConsole
  ) extends AsyncPhantomRunner(classpath, code, logger, console)
       with ComJSRunner with WebsocketListener {

    private def loadMgr() = {
      val clazz = jettyClassLoader.loadClass(
          "scala.scalajs.sbtplugin.env.phantomjs.JettyWebsocketManager")

      val ctors = clazz.getConstructors()
      assert(ctors.length == 1, "JettyWebsocketManager may only have one ctor")

      val mgr = ctors.head.newInstance(this)

      mgr.asInstanceOf[WebsocketManager]
    }

    val mgr: WebsocketManager = loadMgr()

    def onRunning(): Unit = synchronized(notifyAll())
    def onOpen(): Unit = synchronized(notifyAll())
    def onClose(): Unit = synchronized(notifyAll())

    def onMessage(msg: String): Unit = synchronized {
      recvBuf.enqueue(msg)
      notifyAll()
    }

    def log(msg: String): Unit = logger.debug(s"PhantomJS WS Jetty: $msg")

    mgr.start()

    private[this] val recvBuf = mutable.Queue.empty[String]

    /** The websocket server starts asynchronously, but we need the port it is
     *  running on. This method waits until the port is non-negative and
     *  returns its value.
     */
    private def waitForPort(): Int = {
      while (mgr.localPort < 0)
        wait()
      mgr.localPort
    }

    private def comSetup = {
      def maybeExit(code: Int) =
        if (autoExit)
          s"window.callPhantom({ action: 'exit', returnValue: $code });"
        else
          ""

      val serverPort = waitForPort()

      val code = s"""
        |(function() {
        |  // The socket for communication
        |  var websocket = null;
        |
        |  // Buffer for messages sent before socket is open
        |  var outMsgBuf = null;
        |
        |  window.scalajsCom = {
        |    init: function(recvCB) {
        |      if (websocket !== null) throw new Error("Com already open");
        |
        |      outMsgBuf = [];
        |
        |      websocket = new WebSocket("ws://localhost:$serverPort");
        |
        |      websocket.onopen = function(evt) {
        |        for (var i = 0; i < outMsgBuf.length; ++i)
        |          websocket.send(outMsgBuf[i]);
        |        outMsgBuf = null;
        |      };
        |      websocket.onclose = function(evt) {
        |        websocket = null;
        |        ${maybeExit(0)}
        |      };
        |      websocket.onmessage = function(evt) {
        |        recvCB(evt.data);
        |      };
        |      websocket.onerror = function(evt) {
        |        websocket = null;
        |        ${maybeExit(-1)}
        |      };
        |
        |      // Take over responsibility to auto exit
        |      window.callPhantom({
        |        action: 'setAutoExit',
        |        autoExit: false
        |      });
        |    },
        |    send: function(msg) {
        |      if (websocket === null)
        |        return; // we are closed already. ignore message
        |
        |      if (outMsgBuf !== null)
        |        outMsgBuf.push(msg);
        |      else
        |        websocket.send(msg);
        |    },
        |    close: function() {
        |      if (websocket === null)
        |        return; // we are closed already. all is well.
        |
        |      if (outMsgBuf !== null)
        |        // Reschedule ourselves to give onopen a chance to kick in
        |        window.setTimeout(window.scalajsCom.close, 10);
        |      else
        |        websocket.close();
        |    }
        |  }
        |}).call(this);""".stripMargin

      new MemVirtualJSFile("comSetup.js").withContent(code)
    }

    def send(msg: String): Unit = synchronized {
      if (awaitConnection())
        mgr.sendMessage(msg)
    }

    def receive(): String = synchronized {
      if (recvBuf.isEmpty && !awaitConnection())
        throw new ComJSEnv.ComClosedException
      while (recvBuf.isEmpty && !mgr.isClosed)
        wait()

      if (recvBuf.isEmpty)
        throw new ComJSEnv.ComClosedException
      else
        recvBuf.dequeue()
    }

    def close(): Unit = mgr.stop()

    /** Waits until the JS VM has established a connection, or the VM
     *  terminated. Returns true if a connection was established.
     */
    private def awaitConnection(): Boolean = {
      while (!mgr.isConnected && !mgr.isClosed && isRunning)
        wait(200) // We sleep-wait for isRunning

      mgr.isConnected
    }

    override protected def initFiles(): Seq[VirtualJSFile] =
      super.initFiles :+ comSetup
  }

  protected trait AbstractPhantomRunner extends AbstractExtRunner {

    override protected def getVMArgs() =
      // Add launcher file to arguments
      additionalArgs :+ createTmpLauncherFile().getAbsolutePath

    /** In phantom.js, we include JS using HTML */
    override protected def writeJSFile(file: VirtualJSFile, writer: Writer) = {
      file match {
        case file: FileVirtualJSFile =>
          val fname = htmlEscape(file.file.getAbsolutePath)
          writer.write(
              s"""<script type="text/javascript" src="$fname"></script>""" + "\n")
        case _ =>
          writer.write("""<script type="text/javascript">""" + "\n")
          writer.write(s"// Virtual File: ${file.path}\n")
          writer.write(file.content)
          writer.write("</script>\n")
      }
    }

    /**
     * PhantomJS doesn't support Function.prototype.bind. We polyfill it.
     * https://github.com/ariya/phantomjs/issues/10522
     */
    override protected def initFiles(): Seq[VirtualJSFile] = Seq(
        new MemVirtualJSFile("bindPolyfill.js").withContent(
            """
            |// Polyfill for Function.bind from Facebook react:
            |// https://github.com/facebook/react/blob/3dc10749080a460e48bee46d769763ec7191ac76/src/test/phantomjs-shims.js
            |// Originally licensed under Apache 2.0
            |(function() {
            |
            |  var Ap = Array.prototype;
            |  var slice = Ap.slice;
            |  var Fp = Function.prototype;
            |
            |  if (!Fp.bind) {
            |    // PhantomJS doesn't support Function.prototype.bind natively, so
            |    // polyfill it whenever this module is required.
            |    Fp.bind = function(context) {
            |      var func = this;
            |      var args = slice.call(arguments, 1);
            |
            |      function bound() {
            |        var invokedAsConstructor = func.prototype && (this instanceof func);
            |        return func.apply(
            |          // Ignore the context parameter when invoking the bound function
            |          // as a constructor. Note that this includes not only constructor
            |          // invocations using the new keyword but also calls to base class
            |          // constructors such as BaseClass.call(this, ...) or super(...).
            |          !invokedAsConstructor && context || this,
            |          args.concat(slice.call(arguments))
            |        );
            |      }
            |
            |      // The bound function must share the .prototype of the unbound
            |      // function so that any object created by one constructor will count
            |      // as an instance of both constructors.
            |      bound.prototype = func.prototype;
            |
            |      return bound;
            |    };
            |  }
            |
            |})();
            |""".stripMargin
        ),
        new MemVirtualJSFile("scalaJSEnvInfo.js").withContent(
            """
            |__ScalaJSEnv = {
            |  exitFunction: function(status) {
            |    window.callPhantom({
            |      action: 'exit',
            |      returnValue: status | 0
            |    });
            |  }
            |};
            """.stripMargin
        )
    )

    protected def writeWebpageLauncher(out: Writer): Unit = {
      out.write("<html>\n<head>\n<title>Phantom.js Launcher</title>\n")
      sendJS(getLibJSFiles(), out)
      writeCodeLauncher(code, out)
      out.write("</head>\n<body></body>\n</html>\n")
    }

    protected def createTmpLauncherFile(): File = {
      val webF = createTmpWebpage()

      val launcherTmpF = File.createTempFile("phantomjs-launcher", ".js")
      launcherTmpF.deleteOnExit()

      val out = new FileWriter(launcherTmpF)

      try {
        out.write(
            s"""// Scala.js Phantom.js launcher
               |var page = require('webpage').create();
               |var url = ${toJSstr(webF.getAbsolutePath)};
               |var autoExit = $autoExit;
               |page.onConsoleMessage = function(msg) {
               |  console.log(msg);
               |};
               |page.onError = function(msg, trace) {
               |  console.error(msg);
               |  if (trace && trace.length) {
               |    console.error('');
               |    trace.forEach(function(t) {
               |      console.error('  ' + t.file + ':' + t.line + (t.function ? ' (in function "' + t.function +'")' : ''));
               |    });
               |  }
               |
               |  phantom.exit(2);
               |};
               |page.onCallback = function(data) {
               |  if (!data.action) {
               |    console.error('Called callback without action');
               |    phantom.exit(3);
               |  } else if (data.action === 'exit') {
               |    phantom.exit(data.returnValue || 0);
               |  } else if (data.action === 'setAutoExit') {
               |    if (typeof(data.autoExit) === 'boolean')
               |      autoExit = data.autoExit;
               |    else
               |      autoExit = true;
               |  } else {
               |    console.error('Unknown callback action ' + data.action);
               |    phantom.exit(4);
               |  }
               |};
               |page.open(url, function (status) {
               |  if (autoExit || status !== 'success')
               |    phantom.exit(status !== 'success');
               |});
               |""".stripMargin)
      } finally {
        out.close()
      }

      logger.debug(
          "PhantomJS using launcher at: " + launcherTmpF.getAbsolutePath())

      launcherTmpF
    }

    protected def createTmpWebpage(): File = {
      val webTmpF = File.createTempFile("phantomjs-launcher-webpage", ".html")
      webTmpF.deleteOnExit()

      val out = new BufferedWriter(new FileWriter(webTmpF))
      try {
        writeWebpageLauncher(out)
      } finally {
        out.close()
      }

      logger.debug(
          "PhantomJS using webpage launcher at: " + webTmpF.getAbsolutePath())

      webTmpF
    }

    protected def writeCodeLauncher(code: VirtualJSFile, out: Writer): Unit = {
      out.write("""<script type="text/javascript">""" + "\n")
      out.write("// Phantom.js code launcher\n")
      out.write(s"// Origin: ${code.path}\n")
      out.write("window.addEventListener('load', function() {\n")
      out.write(code.content)
      out.write("}, false);\n")
      out.write("</script>\n")
    }
  }

  protected def htmlEscape(str: String): String = str.flatMap {
    case '<' => "&lt;"
    case '>' => "&gt;"
    case '"' => "&quot;"
    case '&' => "&amp;"
    case c   => c :: Nil
  }

}
