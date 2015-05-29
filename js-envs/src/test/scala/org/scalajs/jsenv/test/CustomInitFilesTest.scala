package org.scalajs.jsenv.test

import org.scalajs.jsenv._
import org.scalajs.jsenv.nodejs.NodeJSEnv
import org.scalajs.jsenv.phantomjs.PhantomJSEnv

import org.scalajs.core.tools.io._

import org.junit.Test

abstract class CustomInitFilesTest extends JSEnvTest {
  def makeCustomInitFiles(): Seq[VirtualJSFile] = {
    Seq(new MemVirtualJSFile("custominit.js").withContent("""
      function customPrint(s) {
        console.log("custom: " + s);
      }
    """))
  }

  @Test
  def customInitFilesTest: Unit = {
    """
    customPrint("hello");
    """ hasOutput
    """|custom: hello
       |""".stripMargin
  }
}

class NodeJSWithCustomInitFilesTest extends CustomInitFilesTest {
  protected def newJSEnv: NodeJSEnv = new NodeJSEnv {
    override def customInitFiles() = makeCustomInitFiles()
  }
}

class PhantomJSWithCustomInitFilesTest extends CustomInitFilesTest {
  protected def newJSEnv: PhantomJSEnv = new PhantomJSEnv {
    override def customInitFiles() = makeCustomInitFiles()
  }
}
