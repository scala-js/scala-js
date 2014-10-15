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
import scala.io.Source

import scala.util.DynamicVariable

class NodeJSEnv(
  nodejsPath: Option[String],
  addArgs:    Seq[String],
  addEnv:     Map[String, String]) extends ExternalJSEnv(addArgs, addEnv) {

  import ExternalJSEnv._

  protected def vmName: String = "node.js"
  protected def executable: String = nodejsPath.getOrElse("node")

  private val _libCache = new DynamicVariable[VirtualFileMaterializer](null)
  protected def libCache: VirtualFileMaterializer = _libCache.value

  final protected def withLibCache[T](body: => T): T = {
    _libCache.withValue(new VirtualFileMaterializer(true)) {
      try body
      finally libCache.close()
    }
  }

  // Helper constructors

  def this(
      nodejsPath: String,
      args: Seq[String] = Seq.empty,
      env: Map[String, String] = Map.empty) =
    this(Some(nodejsPath), args, env)

  def this() = this(None, Seq.empty, Map.empty[String, String])

  // We need to initialize the libCache first
  override def runJS(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole): Unit =
    withLibCache(super.runJS(classpath, code, logger, console))

  /** File(s) to automatically install source-map-support.
   *  Is used by [[initFiles]], override to change/disable.
   */
  protected def installSourceMap(args: RunJSArgs): Seq[VirtualJSFile] = Seq(
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
  protected def fixPercentConsole(args: RunJSArgs): Seq[VirtualJSFile] = Seq(
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
  protected def runtimeEnv(args: RunJSArgs): Seq[VirtualJSFile] = Seq(
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
  override protected def initFiles(args: RunJSArgs): Seq[VirtualJSFile] =
    installSourceMap(args) ++ fixPercentConsole(args) ++ runtimeEnv(args)

  /** Libraries are loaded via require in Node.js */
  override protected def getLibJSFiles(args: RunJSArgs): Seq[VirtualJSFile] = {
    initFiles(args) ++
    args.classpath.jsLibs.map((requireLibrary _).tupled) :+
    args.classpath.scalaJSCode
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
  override protected def sendVMStdin(args: RunJSArgs, out: OutputStream): Unit = {
    sendJS(getJSFiles(args), out)
  }

  // Node.js specific (system) environment
  override protected def getVMEnv(args: RunJSArgs): Map[String, String] = {
    val baseNodePath = sys.env.get("NODE_PATH").filter(_.nonEmpty)
    val nodePath = libCache.cacheDir.getAbsolutePath +
      baseNodePath.fold("")(p => File.pathSeparator + p)

    sys.env ++ Seq(
        "NODE_MODULE_CONTEXTS" -> "0",
        "NODE_PATH" -> nodePath
    ) ++ additionalEnv
  }

}
