/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite

import org.scalajs.jasminetest.JasmineTest

object OnJDK8OrHigher extends JasmineTest {

  describe("require-jdk8 tests") {
    it("should only be running JDK8") {
      // This method did not exist id JDK7 and hence this class will not link
      // java.util.Collections.asLifoQueue[Int](null)
      // TODO remove comment one the implementation is added
    }
  }

}
