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
    addArgs: Seq[String],
    addEnv: Map[String, String]) extends ExternalJSEnv(addArgs, addEnv) {

  import ExternalJSEnv._

  protected def vmName: String = "PhantomJS"
  protected def executable: String = phantomjsPath.getOrElse("phantomjs")

  // Helper constructors

  def this(phantomjsPath: Option[String], addArgs: Seq[String]) =
    this(phantomjsPath, addArgs, Map.empty[String, String])

  def this(phantomjsPath: Option[String], addEnv: Map[String, String]) =
    this(phantomjsPath, Seq.empty, addEnv)

  def this(phantomjsPath: String, args: Seq[String], env: Map[String, String]) =
    this(Some(phantomjsPath), args, env)

  def this(phantomjsPath: String, args: Seq[String]) =
    this(Some(phantomjsPath), args, Map.empty[String, String])

  def this(phantomjsPath: String, env: Map[String, String]) =
    this(Some(phantomjsPath), Seq.empty, env)

  def this(args: Seq[String], env: Map[String, String]) = this(None, args, env)

  def this(args: Seq[String]) = this(None, args, Map.empty[String, String])

  def this(env: Map[String, String]) = this(None, Seq.empty, env)

  def this() = this(None, Seq.empty, Map.empty[String, String])

  override protected def getVMArgs(args: RunJSArgs) =
    // Add launcher file to arguments
    additionalArgs :+ createTmpLauncherFile(args).getAbsolutePath

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

  def htmlEscape(str: String): String = str.flatMap {
    case '<' => "&lt;"
    case '>' => "&gt;"
    case '"' => "&quot;"
    case '&' => "&amp;"
    case c   => c :: Nil
  }

  /**
   * PhantomJS doesn't support Function.prototype.bind. We polyfill it.
   * https://github.com/ariya/phantomjs/issues/10522
   */
  override protected def initFiles(args: RunJSArgs): Seq[VirtualJSFile] = Seq(
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
