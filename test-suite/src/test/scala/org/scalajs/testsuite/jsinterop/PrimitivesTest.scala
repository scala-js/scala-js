/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.scalajs.js
import org.scalajs.jasminetest.JasmineTest

object PrimitivesTest extends JasmineTest {

  describe("Interoperability for primitive types") {

    it("should convert Java boxed types to js.Any") {
      expect(new java.lang.Boolean(false)).toBe(false)
      expect(null: java.lang.Boolean).toBeNull

      expect(new java.lang.Byte(42.toByte)).toBe(42)
      expect(null: java.lang.Byte).toBeNull

      expect(new java.lang.Short(42.toShort)).toBe(42)
      expect(null: java.lang.Short).toBeNull

      expect(new java.lang.Integer(42)).toBe(42)
      expect(null: java.lang.Integer).toBeNull

      expect(new java.lang.Long(42L)).toBe(42)
      expect(null: java.lang.Long).toBeNull

      expect(new java.lang.Float(42.0f)).toBe(42)
      expect(null: java.lang.Float).toBeNull

      expect(new java.lang.Double(42.0)).toBe(42)
      expect(null: java.lang.Double).toBeNull
    }

  }

}
