/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

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
