/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.noircheck

import org.scalajs.jasminetest.JasmineTest

import scala.scalajs.js

object RhinoLinkFailureTest extends JasmineTest {

  when("rhino").
  describe("Rhino Linking") {

    it("should throw an exception if it fails loading a class") {

      // scala.collection.parallel.Splitter$ is not defined
      try {
        val pool = scala.collection.parallel.Splitter.empty
        sys.error("Should not reach here")
      } catch {
        case js.JavaScriptException(e) =>
          // Make sure offending class is reported
          expect(e.toString).toContain("sc_parallel_Splitter$")
      }
    }

  }

}
