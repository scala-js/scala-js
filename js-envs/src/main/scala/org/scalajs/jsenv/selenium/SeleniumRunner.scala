package org.scalajs.jsenv.selenium

import org.scalajs.core.tools.io.VirtualJSFile
import org.scalajs.core.tools.jsdep.ResolvedJSDependency
import org.scalajs.core.tools.logging.Logger
import org.scalajs.jsenv._

class SeleniumRunner(browserProvider: SeleniumBrowser,
    libs: Seq[ResolvedJSDependency], code: VirtualJSFile, keepAlive: Boolean)
    extends AbstractSeleniumJSRunner(browserProvider, libs, code) with JSRunner {

  def run(logger: Logger, console: JSConsole): Unit = {
    setupLoggerAndConsole(logger, console)
    browser.start()
    runAllScripts()
    if (!keepAlive)
      browser.close()
  }
}
