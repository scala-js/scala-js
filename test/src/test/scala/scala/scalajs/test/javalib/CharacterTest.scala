/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package javalib

import scala.scalajs.test.JasmineTest
import scala.scalajs.js.Any.fromInt

object CharacterTest extends JasmineTest {

  describe("java.lang.Character") {

    it("should provide `digit`") {
      expect(Character.digit('a', 16)).toEqual(10)
      expect(Character.digit('}',  5)).toEqual(-1)
      expect(Character.digit('1', 50)).toEqual(-1)
      expect(Character.digit('1', 36)).toEqual(1)
      expect(Character.digit('Z', 36)).toEqual(35)
      expect(Character.digit('\uFF22', 20)).toEqual(11)
    }

  }
}
