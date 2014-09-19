/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.testsuite.jsinterop

import org.scalajs.jasminetest.JasmineTest

object `1_TestName` extends JasmineTest {
  describe("a test with name 1_TestName") {
    it("should run") {}
  }
}

object eval extends JasmineTest {
  describe("a test with name eval") {
    it("should run") {}
  }
}

object `\u1f4a7` extends JasmineTest {
  describe("a test with name \u1f4a9") {
    it("should run") {}
  }
}
