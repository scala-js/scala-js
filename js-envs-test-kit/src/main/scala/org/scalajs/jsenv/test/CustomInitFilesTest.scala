package org.scalajs.jsenv.test

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
