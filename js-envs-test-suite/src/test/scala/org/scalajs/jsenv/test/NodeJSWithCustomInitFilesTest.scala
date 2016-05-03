package org.scalajs.jsenv.test

import org.scalajs.jsenv.nodejs.NodeJSEnv

class NodeJSWithCustomInitFilesTest extends CustomInitFilesTest {
  protected def newJSEnv: NodeJSEnv = new NodeJSEnv {
    override def customInitFiles() = makeCustomInitFiles()
  }
}
