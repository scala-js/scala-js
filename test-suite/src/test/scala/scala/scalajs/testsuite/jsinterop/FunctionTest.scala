/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.testsuite.jsinterop

import scala.scalajs.js
import org.scalajs.jasminetest.JasmineTest

object FunctionTest extends JasmineTest {

  import js.Dynamic.{literal => lit}

  describe("scala.scalajs.js.Function") {

    it("should support call() with expanded arguments") {
      val f = js.eval("""
          var f = function() { return arguments; }; f;
      """).asInstanceOf[js.Function]

      expect(f.call(null, 42, true)).toEqual(lit(
          `0` = 42,
          `1` = true))
    }

    it("should support call() with the :_* notation to expand a Seq") {
      val f = js.eval("""
          var f = function() { return arguments; }; f;
      """).asInstanceOf[js.Function]

      val args = Seq[js.Any](42, true)
      expect(f.call(null, args: _*)).toEqual(lit(
          `0` = 42,
          `1` = true))
    }

  }
}
