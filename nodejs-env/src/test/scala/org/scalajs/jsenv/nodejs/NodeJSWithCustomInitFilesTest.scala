package org.scalajs.jsenv.nodejs

import org.scalajs.jsenv.test._

class NodeJSWithCustomInitFilesTest extends CustomInitFilesTest {
  protected def newJSEnv: NodeJSEnv = new NodeJSEnv {
    override def customInitFiles() = makeCustomInitFiles()
  }
}
