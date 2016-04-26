package org.scalajs.jsenv.test

import org.scalajs.jsenv.phantomjs.PhantomJSEnv

class PhantomJSTest extends JSEnvTest with ComTests {
  protected def newJSEnv: PhantomJSEnv = new PhantomJSEnv
}
