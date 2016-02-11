package org.scalajs.jsenv.selenium

import org.scalajs.core.tools.io.{MemVirtualJSFile, VirtualJSFile}
import org.scalajs.core.tools.jsdep.ResolvedJSDependency
import org.scalajs.core.tools.logging.Logger
import org.scalajs.jsenv.{VirtualFileMaterializer, JSConsole}

abstract class AbstractSeleniumJSRunner(browserProvider: SeleniumBrowser,
    libs: Seq[ResolvedJSDependency], code: VirtualJSFile) {

  protected val browser = browserProvider.newDriver

  private[this] var _logger: Logger = _
  private[this] var _console: JSConsole = _

  protected def logger: Logger = _logger
  protected def console: JSConsole = _console

  protected def setupLoggerAndConsole(logger: Logger, console: JSConsole) = {
    require(_logger == null && _console == null)
    _logger = logger
    _console = console
  }

  protected[this] val libCache = new VirtualFileMaterializer(true)

  protected def initFiles(): Seq[VirtualJSFile] =
    browserProvider.initFiles() ++ runtimeEnv()

  protected def runAllScripts(): Unit = {
    val inits = initFiles()
    val cacheDir = libCache.cacheDir.getAbsolutePath
    def absolutePath(fileName: String): String =
      "file://" + cacheDir + "/" + fileName

    val jsFiles = {
      inits.map(file => absolutePath(file.path)) ++
      libs.map(dep => absolutePath(dep.info.relPath.split('/').last)) :+
      code.path
    }
    val page = htmlPage(jsFiles)

    inits.foreach(libCache.materialize)
    libs.foreach(dep => libCache.materialize(dep.lib))
    libCache.materialize(code)
    libCache.materialize(page)

    browser.getWebDriver.get("file://" + cacheDir + "/" + page.path)
    browser.processConsoleLogs(console)
  }

  /** File(s) to define `__ScalaJSEnv`. Defines `exitFunction`. */
  protected def runtimeEnv(): Seq[VirtualJSFile] = Seq(
    new MemVirtualJSFile("scalaJSEnvInfo.js").withContent(
      """
          __ScalaJSEnv = {
            exitFunction: function(status) { window.close(); }
          };
      """
    )
  )

  protected def htmlPage(jsFilesPaths: Seq[String]): VirtualJSFile = {
    val scriptTags = jsFilesPaths.map(path => s"<script src='$path'></script>")
    val pageCode = {
      s"""<html>
         |  <meta charset="ASCII">
         |  <body>
         |    ${scriptTags.mkString("\n    ")}
         |  </body>
         |</html>
      """.stripMargin
    }
    new MemVirtualJSFile("scalajsRun.html").withContent(pageCode)
  }
}
