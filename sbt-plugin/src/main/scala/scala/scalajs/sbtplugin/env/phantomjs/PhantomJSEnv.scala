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
import scala.io.Source

class PhantomJSEnv(
    phantomjsPath: Option[String],
    addEnv: Seq[String]) extends ExternalJSEnv(Seq.empty, addEnv) {

  import ExternalJSEnv._

  protected def vmName: String = "PhantomJS"
  protected def executable: String = phantomjsPath.getOrElse("phantomjs")

  // Helper constructors
  def this(
      phantomjsPath: String,
      env: Seq[String] = Seq.empty) = this(Some(phantomjsPath), env)

  def this() = this(None, Seq.empty)

  override protected def getVMArgs(args: RunJSArgs) =
    // Add launcher file to arguments
    Seq(createTmpLauncherFile(args).getAbsolutePath)

  /** In phantom.js, we include JS using HTML */
  override protected def writeJSFile(file: VirtualJSFile, writer: Writer) = {
    file match {
      case file: FileVirtualJSFile =>
        val fname = toJSstr(file.file.getAbsolutePath)
        writer.write(
            s"""<script type="text/javascript" src=$fname></script>""" + "\n")
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
  override protected def initFiles(args: RunJSArgs): Seq[VirtualJSFile] = Seq(
      new MemVirtualJSFile("bindPolyfill.js").withContent(
          """
          |// Polyfill for Function.bind in Mozilla MDN by Mozilla Contributors
          |// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function/bind
          |// Licensed under CC-BY-SA 2.5
          |if (!Function.prototype.bind) {
          |  Function.prototype.bind = function (oThis) {
          |    if (typeof this !== "function") {
          |      // closest thing possible to the ECMAScript 5 internal IsCallable function
          |      throw new TypeError("Function.prototype.bind - what is trying to be bound is not callable");
          |    }
          |
          |    var aArgs = Array.prototype.slice.call(arguments, 1),
          |        fToBind = this,
          |        fNOP = function () {},
          |        fBound = function () {
          |          return fToBind.apply(this instanceof fNOP && oThis
          |                                 ? this
          |                                 : oThis,
          |                               aArgs.concat(Array.prototype.slice.call(arguments)));
          |        };
          |
          |    fNOP.prototype = this.prototype;
          |    fBound.prototype = new fNOP();
          |
          |    return fBound;
          |  };
          |}
          |""".stripMargin
      )
  )

  protected def writeWebpageLauncher(args: RunJSArgs, out: Writer): Unit = {
    out.write("<html>\n<head>\n<title>Phantom.js Launcher</title>\n")
    sendJS(getLibJSFiles(args), out)
    writeCodeLauncher(args.code, out)
    out.write("</head>\n<body></body>\n</html>\n")
  }

  private def writeCodeLauncher(code: VirtualJSFile, out: Writer): Unit = {
    out.write("""<script type="text/javascript">""" + "\n")
    out.write("// Phantom.js code launcher\n")
    out.write(s"// Origin: ${code.path}\n")
    out.write("window.addEventListener('load', function() {\n")
    out.write(code.content)
    out.write("}, false);\n")
    out.write("</script>\n")
  }

  private def createTmpLauncherFile(args: RunJSArgs): File = {
    val webF = createTmpWebpage(args)

    val launcherTmpF = File.createTempFile("phantomjs-launcher", ".js")
    launcherTmpF.deleteOnExit()

    val out = new FileWriter(launcherTmpF)

    try {
      out.write(
          s"""// Scala.js Phantom.js launcher
             |var page = require('webpage').create();
             |var url = ${toJSstr(webF.getAbsolutePath)};
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
             |page.open(url, function (status) {
             |  phantom.exit(status != 'success');
             |});
             |""".stripMargin)
    } finally {
      out.close()
    }

    launcherTmpF
  }

  private def createTmpWebpage(args: RunJSArgs): File = {
    val webTmpF = File.createTempFile("phantomjs-launcher-webpage", ".html")
    webTmpF.deleteOnExit()

    val out = new BufferedWriter(new FileWriter(webTmpF))
    try {
      writeWebpageLauncher(args, out)
    } finally {
      out.close()
    }

    args.logger.debug(
        "PhantomJS using webpage launcher at: " + webTmpF.getAbsolutePath())

    webTmpF
  }

}
