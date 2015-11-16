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

object BooleanTest extends JasmineTest {

  describe("Boolean primitives") {

    it("&, | and ^ on booleans should return booleans") {
      expect(js.typeOf(true & false)).toEqual("boolean")
      expect(js.typeOf(true | false)).toEqual("boolean")
      expect(js.typeOf(true ^ false)).toEqual("boolean")
    }

    it("&, | and ^ on booleans should return correct results") {
      expect(false & false).toBeFalsy
      expect(false & true).toBeFalsy
      expect(true & false).toBeFalsy
      expect(true & true).toBeTruthy

      expect(false | false).toBeFalsy
      expect(true | false).toBeTruthy
      expect(false | true).toBeTruthy
      expect(true | true).toBeTruthy

      expect(false ^ false).toBeFalsy
      expect(true ^ false).toBeTruthy
      expect(false ^ true).toBeTruthy
      expect(true ^ true).toBeFalsy
    }

  }
}
