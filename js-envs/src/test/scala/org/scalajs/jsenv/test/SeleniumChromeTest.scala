package org.scalajs.jsenv.test

import org.scalajs.jsenv.selenium._

class SeleniumChromeTest extends JSEnvTest with ComTests {
  protected def newJSEnv: SeleniumJSEnv = new SeleniumJSEnv(Chrome)
}
