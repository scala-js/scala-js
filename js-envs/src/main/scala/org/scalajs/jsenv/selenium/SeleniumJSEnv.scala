package org.scalajs.jsenv.selenium

import org.scalajs.core.tools.io.VirtualJSFile
import org.scalajs.core.tools.jsdep.ResolvedJSDependency
import org.scalajs.jsenv._

class SeleniumJSEnv(browser: SeleniumBrowser, keepAlive: Boolean)
    extends AsyncJSEnv with ComJSEnv {

  def browserName: String = browser.name

  def name: String = "SeleniumJSEnv for " + browserName

  def jsRunner(libs: Seq[ResolvedJSDependency], code: VirtualJSFile): JSRunner =
    new SeleniumRunner(browser, libs, code, keepAlive)

  def asyncRunner(libs: Seq[ResolvedJSDependency],
      code: VirtualJSFile): AsyncJSRunner = {
    new SeleniumAsyncJSRunner(browser, libs, code, keepAlive)
  }

  def comRunner(libs: Seq[ResolvedJSDependency],
      code: VirtualJSFile): ComJSRunner = {
    new SeleniumComJSRunner(browser, libs, code, keepAlive)
  }
}
