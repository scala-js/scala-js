/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jsenv.nodejs

import java.io.{Console => _, _}

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.jsdep.ResolvedJSDependency
import org.scalajs.jsenv._
import org.scalajs.core.ir.Utils.escapeJS

import scala.sys.process.{Process, ProcessLogger}

/**
  * @param jsdomDirectory Directory where jsdom will be installed
  * @param jsdomVersion Version of jsdom to use
  */
class JSDOMNodeJSEnv(
  jsdomDirectory: File,
  jsdomVersion: String,
  nodejsPath: String = "node",
  addArgs: Seq[String] = Seq.empty,
  addEnv: Map[String, String] = Map.empty
) extends AbstractNodeJSEnv(nodejsPath, addArgs, addEnv, sourceMap = false) {

  /**
    * Secondary constructor provided for binary compatibility only. Uses the current working directory
    * as the jsdom installation directory.
    */
  def this(
    nodejsPath: String,
    addArgs: Seq[String],
    addEnv: Map[String, String]
  ) = this(new File(System.getProperty("user.dir")), "9.8.3", nodejsPath, addArgs, addEnv)

  protected def vmName: String = "Node.js with JSDOM"

  override def jsRunner(libs: Seq[ResolvedJSDependency],
      code: VirtualJSFile): JSRunner = {
    new DOMNodeRunner(libs, code)
  }

  override def asyncRunner(libs: Seq[ResolvedJSDependency],
      code: VirtualJSFile): AsyncJSRunner = {
    new AsyncDOMNodeRunner(libs, code)
  }

  override def comRunner(libs: Seq[ResolvedJSDependency],
      code: VirtualJSFile): ComJSRunner = {
    new ComDOMNodeRunner(libs, code)
  }

  protected class DOMNodeRunner(libs: Seq[ResolvedJSDependency], code: VirtualJSFile)
      extends ExtRunner(libs, code) with AbstractDOMNodeRunner

  protected class AsyncDOMNodeRunner(libs: Seq[ResolvedJSDependency], code: VirtualJSFile)
      extends AsyncExtRunner(libs, code) with AbstractDOMNodeRunner

  protected class ComDOMNodeRunner(libs: Seq[ResolvedJSDependency], code: VirtualJSFile)
      extends AsyncDOMNodeRunner(libs, code) with NodeComJSRunner

  protected trait AbstractDOMNodeRunner extends AbstractNodeRunner {

    /**
      * Locally install jsdom (in the `jsdomDirectory`) if necessary, and then create the .js file in
      * the same directory.
      *
      * Be careful to ''not'' call this method ''concurrently''.
      *
      * @return The .js file containing the code to run.
      */
    protected def codeWithJSDOMContext(): Seq[VirtualJSFile] = {
      val scriptsJSPaths = getLibJSFiles().map {
        case file: FileVirtualFile => file.path
        case file                  => libCache.materialize(file).getAbsolutePath
      }
      // Install jsdom on the fly, if necessary
      val jsdomModule = new File(jsdomDirectory, "node_modules/jsdom")
      if (!jsdomModule.exists()) {
        logger.info(s"Installing jsdom at ${jsdomModule.getAbsolutePath}")
        jsdomDirectory.mkdirs()
        val npm = sys.props("os.name").toLowerCase match {
          case os if os.contains("win") ⇒ "cmd /c npm"
          case _ ⇒ "npm"
        }
        val process = Process(s"$npm install jsdom@$jsdomVersion", jsdomDirectory)
        val processLogger = ProcessLogger(s => logger.info(s), s => logger.error(s))
        val code = process ! processLogger
        if (code != 0) {
          sys.error(s"Non-zero exit code: $code")
        }
        assert(jsdomModule.exists(), "Installation of jsdom failed")
      }
      val scriptsStringPath = scriptsJSPaths.map('"' + escapeJS(_) + '"')
      val jsDOMCode = {
        s"""
           |(function () {
           |  const jsdom = require("jsdom");
           |  var windowKeys = [];
           |
           |  jsdom.env({
           |    html: "",
           |    virtualConsole: jsdom.createVirtualConsole().sendTo(console),
           |    created: function (error, window) {
           |      if (error == null) {
           |        window["__ScalaJSEnv"] = __ScalaJSEnv;
           |        window["scalajsCom"] = global.scalajsCom;
           |        windowKeys = Object.keys(window);
           |      } else {
           |        console.log(error);
           |      }
           |    },
           |    scripts: [${scriptsStringPath.mkString(", ")}],
           |    onload: function (window) {
           |      for (var k in window) {
           |        if (windowKeys.indexOf(k) == -1)
           |          global[k] = window[k];
           |      }
           |
           |      ${code.content}
           |    }
           |  });
           |})();
           |""".stripMargin
      }
      val codeFile = new File(jsdomDirectory, "codeWithJSDOMContext.js")
      val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(codeFile), "UTF-8"))
      try {
        writer.write(jsDOMCode)
        Seq(FileVirtualJSFile(codeFile))
      } finally {
        writer.close()
      }
    }

    override protected def getJSFiles(): Seq[VirtualJSFile] =
      initFiles() ++ customInitFiles() ++ codeWithJSDOMContext()

    /** Libraries are loaded via scripts in Node.js */
    override protected def getLibJSFiles(): Seq[VirtualJSFile] =
      libs.map(_.lib)

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
