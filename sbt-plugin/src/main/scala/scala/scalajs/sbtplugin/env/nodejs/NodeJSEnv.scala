/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.env.nodejs

import scala.scalajs.sbtplugin.env._

import scala.scalajs.tools.io._
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.env._
import scala.scalajs.tools.logging._

import scala.scalajs.sbtplugin.JSUtils._

import java.io.{ Console => _, _ }
import scala.io.Source

class NodeJSEnv(
  nodejsPath: Option[String],
  addArgs:    Seq[String],
  addEnv:     Seq[String]) extends ExternalJSEnv(addArgs, addEnv) {

  import ExternalJSEnv._

  protected def vmName: String = "node.js"
  protected def executable: String = nodejsPath.getOrElse("node")

  // Helper constructors
  def this(
      nodejsPath: String,
      args: Seq[String] = Seq.empty,
      env: Seq[String] = Seq.empty) = this(Some(nodejsPath), args, env)

  def this() = this(None, Seq.empty, Seq.empty)

  // We need to define importScripts and hack console.log
  override protected def initFiles: Seq[VirtualJSFile] = Seq(
    new MemVirtualJSFile("importScripts.js").withContent(
        """
        global.importScripts = function() {
          for (i in arguments) {
            // Require script and add stuff to global object
            var module = require(arguments[i]);
            for (elem in module)
              global[elem] = module[elem];
          }
        };
        """),
     new MemVirtualJSFile("nodeConsoleHack.js").withContent(
        """
        // Hack console log to duplicate double % signs
        (function() {
          var oldLog = console.log;
          var newLog = function() {
            var args = arguments;
            if (args.length >= 1) {
              args[0] = args[0].replace(/%/g, "%%");
            }
            oldLog.apply(console, args);
          };
          console.log = newLog;
        })();
        """)
  )

  // Send code to Stdin
  override protected def sendVMStdin(args: RunJSArgs, out: OutputStream): Unit = {
    sendJS(getJSFiles(args), out)
  }

  // Node.js specific (system) environment
  override protected def getVMEnv(args: RunJSArgs) = {
    val nodeIncludePath = getRequirePath(args).getAbsolutePath

    Seq(s"NODE_PATH=$nodeIncludePath",
         "NODE_MODULE_CONTEXTS=0") ++ super.getVMEnv(args)
  }

}
