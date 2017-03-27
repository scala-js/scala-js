package org.scalajs.jsenv.phantomjs

import org.scalajs.jsenv.test._

class PhantomJSTest extends JSEnvTest with ComTests {
  protected def newJSEnv: PhantomJSEnv = new PhantomJSEnv
}
