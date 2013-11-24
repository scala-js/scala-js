/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, Jonas Fonseca    **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala

import scala.scalajs.test.ScalaJSTest
import scala.scalajs.js.Any.fromInt

object EnumerationTest extends ScalaJSTest {

  describe("scala.Enumeration") {

    it("requires reflection for populating the name map") {
      object HelpLevel extends Enumeration {
        type HelpLevel = Value
        val None, Basic, Medium, Full = Value
      }

      val h = HelpLevel.None

      expect(h.toString).toEqual("None")
    }

  }
}