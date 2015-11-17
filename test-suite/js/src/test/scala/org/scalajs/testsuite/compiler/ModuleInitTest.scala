/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import org.scalajs.jasminetest.JasmineTest
import scala.scalajs.js

object ModuleInitTest extends JasmineTest {

  describe("Scala Modules") {
    when("compliant-moduleinit").
    it("Should only execute module initializers once") {
      val x = A.Y
      val y = A.cs.head
      expect(x ne null).toBeTruthy
      expect(y eq null).toBeTruthy
      expect(x eq A.Y).toBeTruthy
      expect(Counter.c).toBe(1)
    }
  }

  object Counter {
    var c: Int = 0
  }

  object A {
    private def blankSym = ""

    sealed abstract class C(symbol: String)
    object Y extends C(blankSym) {
      Counter.c += 1
    }

    val cs = Vector[C](Y)
  }
}
