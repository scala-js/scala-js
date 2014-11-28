/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.typedarray

import org.scalajs.jasminetest.JasmineTest

import scala.scalajs.js.typedarray._

object ArrayBufferTest extends JasmineTest {

  when("typedarry").
  describe("ArrayBuffer") {

    it("should provide a length constructor") {
      val x = new ArrayBuffer(100)
      expect(x.isInstanceOf[ArrayBuffer]).toBeTruthy
      expect(x.byteLength).toBe(100)
    }

    it("should provide `slice` with one argument") {
      val x = new ArrayBuffer(100)
      val y = x.slice(10)
      expect(y.byteLength).toBe(90)
    }

    it("should provide `slice` with two arguments") {
      val x = new ArrayBuffer(100)
      val y = x.slice(10, 20)
      expect(y.byteLength).toBe(10)
    }

  }

}
