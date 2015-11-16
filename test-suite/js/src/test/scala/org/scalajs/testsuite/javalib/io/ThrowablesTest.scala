/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.io

import org.scalajs.jasminetest.JasmineTest

object ThrowablesTest extends JasmineTest {
  describe("java.io.Throwables") {

    it("should define all java.io Errors/Exceptions") {
      import java.io._
      new IOException("", new Exception())
      new EOFException("")
      new UTFDataFormatException("")
      new UnsupportedEncodingException("")
      new NotSerializableException("")
    }
  }
}
