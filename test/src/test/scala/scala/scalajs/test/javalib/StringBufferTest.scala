/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package javalib

import scala.scalajs.js
import scala.scalajs.test.JasmineTest

object StringBufferTest extends JasmineTest {

  describe("java.lang.StringBuffer") {

    def newBuf = new java.lang.StringBuffer

    it("should respond to `append`") {
      expect(newBuf.append("asdf").toString).toEqual("asdf")
      expect(newBuf.append(null: AnyRef).toString).toEqual("null")
      expect(newBuf.append(null: String).toString).toEqual("null")
      expect(newBuf.append(null: CharSequence,0,2).toString).toEqual("nu")
      expect(newBuf.append((): js.Undefined).toString).toEqual("undefined")
    }

  }

  describe("java.lang.StringBuilder") {

    def newBuf = new java.lang.StringBuilder

    it("should respond to `append`") {
      expect(newBuf.append("asdf").toString).toEqual("asdf")
      expect(newBuf.append(null: AnyRef).toString).toEqual("null")
      expect(newBuf.append(null: String).toString).toEqual("null")
      expect(newBuf.append(null: CharSequence,0,2).toString).toEqual("nu")
      expect(newBuf.append((): js.Undefined).toString).toEqual("undefined")
    }

    it("should allow string interpolation to survive `null` and `undefined`") {
      expect(s"${null}").toEqual("null")
      expect(s"${(): js.Undefined}").toEqual("undefined")
    }

  }
}
