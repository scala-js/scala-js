package org.scalajs.jsenv.phantomjs

import org.scalajs.jsenv.test._

class PhantomJSWithCustomInitFilesTest extends CustomInitFilesTest {
  protected def newJSEnv: PhantomJSEnv = new PhantomJSEnv {
    override def customInitFiles() = makeCustomInitFiles()
  }
}
