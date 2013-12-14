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

object IntegerTest extends JasmineTest {

  describe("java.lang.Integer") {

    it("should provide `reverseBytes` used by scala.Enumeration") {
      expect(Integer.reverseBytes(0xdeadbeef)).toEqual(0xefbeadde)
    }

    it("should provide `bitCount` used by Map") {
      abstract sealed class Status
      case object Used extends Status
      case object Current extends Status
      case object OneMove extends Status
      case object MultipleMoves extends Status
      case object Other extends Status

      val map = Map(Used -> 0, Other -> 0, Current -> 0, MultipleMoves -> 1, OneMove -> 2)

      expect(map.size).toEqual(5)
      expect(map(MultipleMoves)).toEqual(1)
    }

    it("should provide `numberOfTrailingZeros`") {
      expect(Integer.numberOfTrailingZeros(0xa3c49000)).toEqual(12)
      expect(Integer.numberOfTrailingZeros(0x43f49020)).toEqual(5)
      expect(Integer.numberOfTrailingZeros(0x43c08000)).toEqual(15)
      expect(Integer.numberOfTrailingZeros(0)).toEqual(32)
    }

  }
}
