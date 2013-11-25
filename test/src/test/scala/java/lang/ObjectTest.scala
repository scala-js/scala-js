/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, Jonas Fonseca    **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package java.lang

import scala.scalajs.test.ScalaJSTest

object ObjectTest extends ScalaJSTest {

  describe("java.lang.Object") {

    it("should provide `equals`") {
      case class xy(x: Int, y: Int)

      val l = List(xy(1, 2), xy(2, 1))
      val xy12 = xy(1, 2)

      expect(l.contains(xy12)).toBeTruthy
      expect(l.exists(_ == xy12)).toBeTruthy // the workaround
    }

  }
}
