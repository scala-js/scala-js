package org.scalajs.jsenv.test

import org.scalajs.jsenv.phantomjs.PhantomJSEnv

import org.junit.Test

class PhantomJSTest extends JSEnvTest with ComTests {
  protected def newJSEnv: PhantomJSEnv = new PhantomJSEnv
}
