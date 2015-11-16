/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.scalajs.js

import org.scalajs.jasminetest.JasmineTest

object ArraySAMTest extends JasmineTest {

  import js.JSArrayOps._

  describe("scala.scalajs.js.Array with SAM support") {

    it("should provide jsMap") {
      expect(js.Array("Sc", "ala", ".", "js").jsMap(_.length)).toEqual(
          js.Array(2, 3, 1, 2))
    }

    it("should provide jsFilter") {
      expect(js.Array(56, 30, -20, 33, 54, 86).jsFilter(_ % 3 != 0)).toEqual(
          js.Array(56, -20, 86))
    }

  }

}
