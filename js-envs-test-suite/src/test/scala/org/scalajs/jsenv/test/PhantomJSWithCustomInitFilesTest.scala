package org.scalajs.jsenv.test

import org.scalajs.jsenv.phantomjs.PhantomJSEnv

class PhantomJSWithCustomInitFilesTest extends CustomInitFilesTest {
  protected def newJSEnv: PhantomJSEnv = new PhantomJSEnv {
    override def customInitFiles() = makeCustomInitFiles()
  }
}
