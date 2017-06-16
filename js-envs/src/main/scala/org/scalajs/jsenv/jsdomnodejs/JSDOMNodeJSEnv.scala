/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js JS envs           **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.jsenv.jsdomnodejs

import scala.collection.immutable

import java.io.OutputStream

import org.scalajs.core.tools.io._
import org.scalajs.jsenv._
import org.scalajs.jsenv.nodejs.AbstractNodeJSEnv

import org.scalajs.core.ir.Utils.escapeJS

class JSDOMNodeJSEnv(config: JSDOMNodeJSEnv.Config) extends AbstractNodeJSEnv {

  def this() = this(JSDOMNodeJSEnv.Config())

  protected def vmName: String = "Node.js with JSDOM"

  protected def executable: String = config.executable

  override protected def args: immutable.Seq[String] = config.args

  override protected def env: Map[String, String] = config.env

  // TODO We might want to make this configurable - not sure why it isn't
  override protected def wantSourceMap: Boolean = false

  override def jsRunner(files: Seq[VirtualJSFile]): JSRunner =
    new DOMNodeRunner(files)

  override def asyncRunner(files: Seq[VirtualJSFile]): AsyncJSRunner =
    new AsyncDOMNodeRunner(files)

  override def comRunner(files: Seq[VirtualJSFile]): ComJSRunner =
    new ComDOMNodeRunner(files)

  protected class DOMNodeRunner(files: Seq[VirtualJSFile])
      extends ExtRunner(files) with AbstractDOMNodeRunner

  protected class AsyncDOMNodeRunner(files: Seq[VirtualJSFile])
      extends AsyncExtRunner(files) with AbstractDOMNodeRunner

  protected class ComDOMNodeRunner(files: Seq[VirtualJSFile])
      extends AsyncDOMNodeRunner(files) with NodeComJSRunner

  protected trait AbstractDOMNodeRunner extends AbstractNodeRunner {

    protected def codeWithJSDOMContext(): Seq[VirtualJSFile] = {
      val scriptsPaths = getScriptsJSFiles().map {
        case file: FileVirtualFile => file.path
        case file                  => libCache.materialize(file).getAbsolutePath
      }
      val scriptsURIs =
        scriptsPaths.map(path => new java.io.File(path).toURI.toASCIIString)
      val scriptsURIsAsJSStrings = scriptsURIs.map('"' + escapeJS(_) + '"')
      val jsDOMCode = {
        s"""
           |(function () {
           |  var jsdom;
           |  try {
           |    jsdom = require("jsdom/lib/old-api.js"); // jsdom >= 10.x
           |  } catch (e) {
           |    jsdom = require("jsdom"); // jsdom <= 9.x
           |  }
           |
           |  var virtualConsole = jsdom.createVirtualConsole()
           |    .sendTo(console, { omitJsdomErrors: true });
           |  virtualConsole.on("jsdomError", function (error) {
           |    /* This inelegant if + console.error is the only way I found
           |     * to make sure the stack trace of the original error is
           |     * printed out.
           |     */
           |    if (error.detail && error.detail.stack)
           |      console.error(error.detail.stack);
           |
           |    // Throw the error anew to make sure the whole execution fails
           |    throw error;
           |  });
           |
           |  jsdom.env({
           |    html: "",
           |    url: "http://localhost/",
           |    virtualConsole: virtualConsole,
           |    created: function (error, window) {
           |      if (error == null) {
           |        window["__ScalaJSEnv"] = __ScalaJSEnv;
           |        window["scalajsCom"] = global.scalajsCom;
           |      } else {
           |        throw error;
           |      }
           |    },
           |    scripts: [${scriptsURIsAsJSStrings.mkString(", ")}]
           |  });
           |})();
           |""".stripMargin
      }
      Seq(new MemVirtualJSFile("codeWithJSDOMContext.js").withContent(jsDOMCode))
    }

    /** All the JS files that are passed to the VM.
     *
     *  This method can overridden to provide custom behavior in subclasses.
     *
     *  This method is overridden in `JSDOMNodeJSEnv` so that user-provided
     *  JS files (excluding "init" files) are executed as *scripts* within the
     *  jsdom environment, rather than being directly executed by the VM.
     *
     *  The value returned by this method in `JSDOMNodeJSEnv` is
     *  `initFiles() ++ customInitFiles() ++ codeWithJSDOMContext()`.
     */
    override protected def getJSFiles(): Seq[VirtualJSFile] =
      initFiles() ++ customInitFiles() ++ codeWithJSDOMContext()

    /** JS files to be loaded via scripts in the jsdom environment.
     *
     *  This method can be overridden to provide a different list of scripts.
     *
     *  The default value in `JSDOMNodeJSEnv` is `files`.
     */
    protected def getScriptsJSFiles(): Seq[VirtualJSFile] =
      files

    // Send code to Stdin
    override protected def sendVMStdin(out: OutputStream): Unit = {
      /* Do not factor this method out into AbstractNodeRunner or when mixin in
       * the traits it would use AbstractExtRunner.sendVMStdin due to
       * linearization order.
       */
      sendJS(getJSFiles(), out)
    }
  }
}

object JSDOMNodeJSEnv {
  final class Config private (
      val executable: String,
      val args: List[String],
      val env: Map[String, String]
  ) {
    private def this() = {
      this(
          executable = "node",
          args = Nil,
          env = Map.empty
      )
    }

    def withExecutable(executable: String): Config =
      copy(executable = executable)

    def withArgs(args: List[String]): Config =
      copy(args = args)

    def withEnv(env: Map[String, String]): Config =
      copy(env = env)

    private def copy(
        executable: String = executable,
        args: List[String] = args,
        env: Map[String, String] = env
    ): Config = {
      new Config(executable, args, env)
    }
  }

  object Config {
    /** Returns a default configuration for a [[JSDOMNodeJSEnv]].
     *
     *  The defaults are:
     *
     *  - `executable`: `"node"`
     *  - `args`: `Nil`
     *  - `env`: `Map.empty`
     */
    def apply(): Config = new Config()
  }
}
