/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package scalalib

import scala.scalajs.test.JasmineTest

object EnumerationTest extends JasmineTest {

  describe("scala.Enumeration") {

    it("should use explicit naming for enumerated values - #38") {
      object HelpLevel extends Enumeration {
        type HelpLevel = Value
        val None = Value("None")
        val Basic = Value("Basic")
        val Medium = Value("Medium")
        val Full = Value("Full")
      }

      val h = HelpLevel.None

      expect(h.toString).toEqual("None")
    }

  }
}
