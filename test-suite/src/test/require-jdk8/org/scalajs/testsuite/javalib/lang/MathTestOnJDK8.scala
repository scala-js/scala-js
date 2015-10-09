package org.scalajs.testsuite.javalib.lang

import org.scalajs.jasminetest.JasmineTest
import org.scalajs.testsuite.javalib.ExpectExceptions

object MathTestOnJDK8 extends JasmineTest with ExpectExceptions {

  describe("java.lang.Math on JDK8 or higher") {
    it("should respond to `addExact`") {
      expect(Math.addExact(0, 0) == 0).toBeTruthy
      expect(Math.addExact(0, 1) == 1).toBeTruthy
      expect(Math.addExact(1, 0) == 1).toBeTruthy
      expect(Math.addExact(0, -1) == -1).toBeTruthy
      expect(Math.addExact(-1, 0) == -1).toBeTruthy
      expect(Math.addExact(1, -1) == 0).toBeTruthy
      expect(Math.addExact(-1, 1) == 0).toBeTruthy
      expect(Math.addExact(Int.MinValue, 0) == Int.MinValue).toBeTruthy
      expect(Math.addExact(0, Int.MinValue) == Int.MinValue).toBeTruthy
      expect(Math.addExact(Int.MinValue, 1) == -2147483647).toBeTruthy
      expect(Math.addExact(1, Int.MinValue) == -2147483647).toBeTruthy
      expect(Math.addExact(-1, -2147483647) == Int.MinValue).toBeTruthy
      expect(Math.addExact(-2147483647, -1) == Int.MinValue).toBeTruthy
      expect(Math.addExact(Int.MaxValue, 0) == Int.MaxValue).toBeTruthy
      expect(Math.addExact(0, Int.MaxValue) == Int.MaxValue).toBeTruthy
      expect(Math.addExact(Int.MaxValue, -1) == 2147483646).toBeTruthy
      expect(Math.addExact(-1, Int.MaxValue) == 2147483646).toBeTruthy
      expect(Math.addExact(2147483646, 1) == Int.MaxValue).toBeTruthy
      expect(Math.addExact(1, 2147483646) == Int.MaxValue).toBeTruthy
      expect(Math.addExact(-1073741824, -1073741824) == Int.MinValue).toBeTruthy

      expectThrows[ArithmeticException](Math.addExact(Int.MinValue, -1))
      expectThrows[ArithmeticException](Math.addExact(-1, Int.MinValue))
      expectThrows[ArithmeticException](Math.addExact(Int.MinValue, Int.MinValue))
      expectThrows[ArithmeticException](Math.addExact(Int.MaxValue, 1))
      expectThrows[ArithmeticException](Math.addExact(1, Int.MaxValue))
      expectThrows[ArithmeticException](Math.addExact(Int.MaxValue, Int.MaxValue))
      expectThrows[ArithmeticException](Math.addExact(1073741824, 1073741824))

      expect(Math.addExact(0L, 0L) == 0L).toBeTruthy
      expect(Math.addExact(0L, 1L) == 1L).toBeTruthy
      expect(Math.addExact(1L, 0L) == 1L).toBeTruthy
      expect(Math.addExact(0L, -1L) == -1L).toBeTruthy
      expect(Math.addExact(-1L, 0L) == -1L).toBeTruthy
      expect(Math.addExact(1L, -1L) == 0L).toBeTruthy
      expect(Math.addExact(-1L, 1L) == 0L).toBeTruthy
      expect(Math.addExact(Long.MinValue, 0) == Long.MinValue).toBeTruthy
      expect(Math.addExact(0, Long.MinValue) == Long.MinValue).toBeTruthy
      expect(Math.addExact(Long.MinValue, 1) == -9223372036854775807L).toBeTruthy
      expect(Math.addExact(1, Long.MinValue) == -9223372036854775807L).toBeTruthy
      expect(Math.addExact(-9223372036854775807L, -1) == Long.MinValue).toBeTruthy
      expect(Math.addExact(-1, -9223372036854775807L) == Long.MinValue).toBeTruthy
      expect(Math.addExact(Long.MaxValue, 0) == Long.MaxValue).toBeTruthy
      expect(Math.addExact(0, Long.MaxValue) == Long.MaxValue).toBeTruthy
      expect(Math.addExact(Long.MaxValue, -1) == 9223372036854775806L).toBeTruthy
      expect(Math.addExact(-1, Long.MaxValue) == 9223372036854775806L).toBeTruthy
      expect(Math.addExact(9223372036854775806L, 1) == Long.MaxValue).toBeTruthy
      expect(Math.addExact(1, 9223372036854775806L) == Long.MaxValue).toBeTruthy
      expect(Math.addExact(-4611686018427387904L, -4611686018427387904L) == Long.MinValue).toBeTruthy

      expectThrows[ArithmeticException](Math.addExact(Long.MinValue, -1))
      expectThrows[ArithmeticException](Math.addExact(-1, Long.MinValue))
      expectThrows[ArithmeticException](Math.addExact(Long.MinValue, Long.MinValue))
      expectThrows[ArithmeticException](Math.addExact(Long.MaxValue, 1))
      expectThrows[ArithmeticException](Math.addExact(1, Long.MaxValue))
      expectThrows[ArithmeticException](Math.addExact(Long.MaxValue, Long.MaxValue))
      expectThrows[ArithmeticException](Math.addExact(4611686018427387904L, 4611686018427387904L))
    }

    it("should respond to `subtractExact`") {
      expect(Math.subtractExact(0, 0) == 0).toBeTruthy
      expect(Math.subtractExact(1, 0) == 1).toBeTruthy
      expect(Math.subtractExact(0, 1) == -1).toBeTruthy
      expect(Math.subtractExact(1, 1) == 0).toBeTruthy
      expect(Math.subtractExact(0, -1) == 1).toBeTruthy
      expect(Math.subtractExact(-1, 0) == -1).toBeTruthy
      expect(Math.subtractExact(-1, -1) == 0).toBeTruthy
      expect(Math.subtractExact(Int.MinValue, 0) == Int.MinValue).toBeTruthy
      expect(Math.subtractExact(Int.MaxValue, 0) == Int.MaxValue).toBeTruthy
      expect(Math.subtractExact(Int.MinValue, -1) == -2147483647).toBeTruthy
      expect(Math.subtractExact(Int.MaxValue, 1) == 2147483646).toBeTruthy
      expect(Math.subtractExact(-1, Int.MaxValue) == Int.MinValue).toBeTruthy
      expect(Math.subtractExact(0, -Int.MaxValue) == Int.MaxValue).toBeTruthy
      expect(Math.subtractExact(0, Int.MaxValue) == -2147483647).toBeTruthy
      expect(Math.subtractExact(-1, Int.MinValue) == Int.MaxValue).toBeTruthy
      expect(Math.subtractExact(-1073741824, 1073741824) == Int.MinValue).toBeTruthy
      expectThrows[ArithmeticException](Math.subtractExact(0, Int.MinValue))
      expectThrows[ArithmeticException](Math.subtractExact(Int.MinValue, 1))
      expectThrows[ArithmeticException](Math.subtractExact(Int.MinValue, Int.MaxValue))
      expectThrows[ArithmeticException](Math.subtractExact(-2, Int.MaxValue))
      expectThrows[ArithmeticException](Math.subtractExact(Int.MaxValue, -1))
      expectThrows[ArithmeticException](Math.subtractExact(Int.MaxValue, Int.MinValue))
      expectThrows[ArithmeticException](Math.subtractExact(1073741824, -1073741824))

      expect(Math.subtractExact(0L, 0L) == 0L).toBeTruthy
      expect(Math.subtractExact(1L, 0L) == 1L).toBeTruthy
      expect(Math.subtractExact(0L, 1L) == -1L).toBeTruthy
      expect(Math.subtractExact(1L, 1L) == 0L).toBeTruthy
      expect(Math.subtractExact(0L, -1L) == 1L).toBeTruthy
      expect(Math.subtractExact(-1L, 0L) == -1L).toBeTruthy
      expect(Math.subtractExact(-1L, -1L) == 0L).toBeTruthy
      expect(Math.subtractExact(Long.MinValue, 0) == Long.MinValue).toBeTruthy
      expect(Math.subtractExact(Long.MaxValue, 0) == Long.MaxValue).toBeTruthy
      expect(Math.subtractExact(Long.MinValue, -1) == -9223372036854775807L).toBeTruthy
      expect(Math.subtractExact(Long.MaxValue, 1) == 9223372036854775806L).toBeTruthy
      expect(Math.subtractExact(-1, Long.MaxValue) == Long.MinValue).toBeTruthy
      expect(Math.subtractExact(0, -Long.MaxValue) == Long.MaxValue).toBeTruthy
      expect(Math.subtractExact(0, Long.MaxValue) == -9223372036854775807L).toBeTruthy
      expect(Math.subtractExact(-1, Long.MinValue) == Long.MaxValue).toBeTruthy
      expect(Math.subtractExact(-4611686018427387904L, 4611686018427387904L) == Long.MinValue).toBeTruthy

      expectThrows[ArithmeticException](Math.subtractExact(0, Long.MinValue))
      expectThrows[ArithmeticException](Math.subtractExact(Long.MinValue, 1))
      expectThrows[ArithmeticException](Math.subtractExact(Long.MinValue, Long.MaxValue))
      expectThrows[ArithmeticException](Math.subtractExact(Long.MinValue, 1))
      expectThrows[ArithmeticException](Math.subtractExact(-2, Long.MaxValue))
      expectThrows[ArithmeticException](Math.subtractExact(Long.MaxValue, -1))
      expectThrows[ArithmeticException](Math.subtractExact(Long.MaxValue, Long.MinValue))
      expectThrows[ArithmeticException](Math.subtractExact(4611686018427387904L, -4611686018427387904L))
    }

    it("should respond to `multiplyExact") {
      for (n <- Seq(Int.MinValue, -1, 0, 1, Int.MaxValue)) {
        expect(Math.multiplyExact(n, 0) == 0).toBeTruthy
        expect(Math.multiplyExact(0, n) == 0).toBeTruthy
        expect(Math.multiplyExact(n, 1) == n).toBeTruthy
        expect(Math.multiplyExact(1, n) == n).toBeTruthy
      }
      expect(Math.multiplyExact(-2147483647, -1) == Int.MaxValue).toBeTruthy
      expect(Math.multiplyExact(-1, -2147483647) == Int.MaxValue).toBeTruthy
      expect(Math.multiplyExact(1073741823, 2) == 2147483646).toBeTruthy
      expect(Math.multiplyExact(2, 1073741823) == 2147483646).toBeTruthy
      expect(Math.multiplyExact(1073741824, -2) == Int.MinValue).toBeTruthy
      expect(Math.multiplyExact(-2, 1073741824) == Int.MinValue).toBeTruthy

      expectThrows[ArithmeticException](Math.multiplyExact(Int.MinValue, -1))
      expectThrows[ArithmeticException](Math.multiplyExact(-1, Int.MinValue))
      expectThrows[ArithmeticException](Math.multiplyExact(Int.MinValue, Int.MinValue))
      expectThrows[ArithmeticException](Math.multiplyExact(Int.MaxValue, Int.MaxValue))
      expectThrows[ArithmeticException](Math.multiplyExact(Int.MinValue, Int.MaxValue))
      expectThrows[ArithmeticException](Math.multiplyExact(Int.MaxValue, Int.MinValue))
      expectThrows[ArithmeticException](Math.multiplyExact(1073741824, 2))
      expectThrows[ArithmeticException](Math.multiplyExact(2, 1073741824))
      expectThrows[ArithmeticException](Math.multiplyExact(1073741825, -2))
      expectThrows[ArithmeticException](Math.multiplyExact(-2, 1073741825))

      for (n <- Seq(Long.MinValue, -1L, 0L, 1L, Long.MaxValue)) {
        expect(Math.multiplyExact(n, 0) == 0).toBeTruthy
        expect(Math.multiplyExact(0, n) == 0).toBeTruthy
        expect(Math.multiplyExact(n, 1) == n).toBeTruthy
        expect(Math.multiplyExact(1, n) == n).toBeTruthy
      }
      expect(Math.multiplyExact(Long.MinValue, 0) == 0).toBeTruthy
      expect(Math.multiplyExact(0, Long.MinValue) == 0).toBeTruthy
      expect(Math.multiplyExact(-9223372036854775807L, -1) == Long.MaxValue).toBeTruthy
      expect(Math.multiplyExact(-1, -9223372036854775807L) == Long.MaxValue).toBeTruthy
      expect(Math.multiplyExact(4611686018427387903L, 2) == 9223372036854775806L).toBeTruthy
      expect(Math.multiplyExact(2, 4611686018427387903L) == 9223372036854775806L).toBeTruthy
      expect(Math.multiplyExact(4611686018427387904L, -2) == Long.MinValue).toBeTruthy
      expect(Math.multiplyExact(-2, 4611686018427387904L) == Long.MinValue).toBeTruthy

      expectThrows[ArithmeticException](Math.multiplyExact(Long.MinValue, -1))
      expectThrows[ArithmeticException](Math.multiplyExact(-1, Long.MinValue))
      expectThrows[ArithmeticException](Math.multiplyExact(Long.MinValue, Long.MinValue))
      expectThrows[ArithmeticException](Math.multiplyExact(Long.MaxValue, Long.MaxValue))
      expectThrows[ArithmeticException](Math.multiplyExact(Long.MinValue, Long.MaxValue))
      expectThrows[ArithmeticException](Math.multiplyExact(Long.MaxValue, Long.MinValue))
      expectThrows[ArithmeticException](Math.multiplyExact(4611686018427387904L, 2))
      expectThrows[ArithmeticException](Math.multiplyExact(2, 4611686018427387904L))
      expectThrows[ArithmeticException](Math.multiplyExact(4611686018427387905L, -2))
      expectThrows[ArithmeticException](Math.multiplyExact(-2, 4611686018427387905L))
    }

    it("should respond to `incrementExact`") {
      expect(Math.incrementExact(Int.MaxValue - 1) == Int.MaxValue).toBeTruthy
      expect(Math.incrementExact(Long.MaxValue - 1) == Long.MaxValue).toBeTruthy

      expectThrows[ArithmeticException](Math.incrementExact(Int.MaxValue))
      expectThrows[ArithmeticException](Math.incrementExact(Long.MaxValue))
    }

    it("should respond to `decrementExact`") {
      expect(Math.decrementExact(Int.MinValue + 1) == Int.MinValue).toBeTruthy
      expect(Math.decrementExact(Long.MinValue + 1) == Long.MinValue).toBeTruthy

      expectThrows[ArithmeticException](Math.decrementExact(Int.MinValue))
      expectThrows[ArithmeticException](Math.decrementExact(Long.MinValue))
    }

    it("should respond to `negateExact`") {
      expect(Math.negateExact(Int.MinValue + 1) == Int.MaxValue).toBeTruthy
      expect(Math.negateExact(Int.MaxValue) == Int.MinValue + 1).toBeTruthy
      expect(Math.negateExact(Long.MinValue + 1) == Long.MaxValue).toBeTruthy
      expect(Math.negateExact(Long.MaxValue) == Long.MinValue + 1).toBeTruthy

      expectThrows[ArithmeticException](Math.negateExact(Int.MinValue))
      expectThrows[ArithmeticException](Math.negateExact(Long.MinValue))
    }

    it("should respond to `toIntExact`") {
      expect(Math.toIntExact(-2147483648L)).toEqual(Int.MinValue)
      expect(Math.toIntExact(2147483647L)).toEqual(Int.MaxValue)

      expectThrows[ArithmeticException](Math.toIntExact(-2147483649L))
      expectThrows[ArithmeticException](Math.toIntExact(2147483648L))
    }

    it("should respond to `floorDiv`") {
      expect(Math.floorDiv(0, 1)).toEqual(0)
      expect(Math.floorDiv(0, -1)).toEqual(0)
      expect(Math.floorDiv(1, 1)).toEqual(1)
      expect(Math.floorDiv(1, -1)).toEqual(-1)
      expect(Math.floorDiv(1, 2)).toEqual(0)
      expect(Math.floorDiv(1, -2)).toEqual(-1)
      expect(Math.floorDiv(-1, 2)).toEqual(-1)
      expect(Math.floorDiv(-1, -2)).toEqual(0)
      expect(Math.floorDiv(1, Int.MaxValue)).toEqual(0)
      expect(Math.floorDiv(1, Int.MinValue)).toEqual(-1)
      expect(Math.floorDiv(-1, Int.MaxValue)).toEqual(-1)
      expect(Math.floorDiv(-1, Int.MinValue)).toEqual(0)
      expect(Math.floorDiv(Int.MaxValue, 1)).toEqual(Int.MaxValue)
      expect(Math.floorDiv(Int.MaxValue, -1)).toEqual(-Int.MaxValue)
      expect(Math.floorDiv(Int.MinValue, 1)).toEqual(Int.MinValue)
      expect(Math.floorDiv(Int.MinValue, -1)).toEqual(Int.MinValue)

      expect(Math.floorDiv(0L, 1L) == 0L).toBeTruthy
      expect(Math.floorDiv(0L, -1L) == 0L).toBeTruthy
      expect(Math.floorDiv(1L, 1L) == 1L).toBeTruthy
      expect(Math.floorDiv(1L, -1L) == -1L).toBeTruthy
      expect(Math.floorDiv(1L, 2L) == 0L).toBeTruthy
      expect(Math.floorDiv(1L, -2L) == -1L).toBeTruthy
      expect(Math.floorDiv(-1L, 2L) == -1L).toBeTruthy
      expect(Math.floorDiv(-1L, -2L) == 0L).toBeTruthy
      expect(Math.floorDiv(1L, Long.MaxValue) == 0L).toBeTruthy
      expect(Math.floorDiv(1L, Long.MinValue) == -1L).toBeTruthy
      expect(Math.floorDiv(-1L, Long.MaxValue) == -1L).toBeTruthy
      expect(Math.floorDiv(-1L, Long.MinValue) == 0L).toBeTruthy
      expect(Math.floorDiv(Long.MaxValue, 1) == Long.MaxValue).toBeTruthy
      expect(Math.floorDiv(Long.MaxValue, -1) == -Long.MaxValue).toBeTruthy
      expect(Math.floorDiv(Long.MinValue, 1) == Long.MinValue).toBeTruthy
      expect(Math.floorDiv(Long.MinValue, -1) == Long.MinValue).toBeTruthy

      for (n <- Seq(0L, 1L, -1L, Long.MaxValue, Long.MinValue))
        expectThrows[ArithmeticException](Math.floorDiv(n, 0))
    }

    unless("phantomjs"). // crashes otherwise, see #593
    it("should respond to `floorMod`") {
      expect(Math.floorMod(0, 1)).toEqual(0)
      expect(Math.floorMod(0, -1)).toEqual(0)
      expect(Math.floorMod(1, 1)).toEqual(0)
      expect(Math.floorMod(1, -1)).toEqual(0)
      expect(Math.floorMod(1, 3)).toEqual(1)
      expect(Math.floorMod(1, -3)).toEqual(-2)
      expect(Math.floorMod(-1, 3)).toEqual(2)
      expect(Math.floorMod(-1, -3)).toEqual(-1)
      expect(Math.floorMod(1, Int.MaxValue)).toEqual(1)
      expect(Math.floorMod(1, Int.MinValue)).toEqual(-2147483647)
      expect(Math.floorMod(-1, Int.MaxValue)).toEqual(2147483646)
      expect(Math.floorMod(-1, Int.MinValue)).toEqual(-1)
      expect(Math.floorMod(Int.MaxValue, 1)).toEqual(0)
      expect(Math.floorMod(Int.MaxValue, -1)).toEqual(0)
      expect(Math.floorMod(Int.MinValue, 1)).toEqual(0)
      expect(Math.floorMod(Int.MinValue, -1)).toEqual(0)

      expect(Math.floorMod(0L, 1L) == 0L).toBeTruthy
      expect(Math.floorMod(0L, -1L) == 0L).toBeTruthy
      expect(Math.floorMod(1L, 1L) == 0L).toBeTruthy
      expect(Math.floorMod(1L, -1L) == 0L).toBeTruthy
      expect(Math.floorMod(1L, 3L) == 1L).toBeTruthy
      expect(Math.floorMod(1L, -3L) == -2L).toBeTruthy
      expect(Math.floorMod(-1L, 3L) == 2L).toBeTruthy
      expect(Math.floorMod(-1L, -3L) == -1L).toBeTruthy
      expect(Math.floorMod(1L, Long.MaxValue) == 1L).toBeTruthy
      expect(Math.floorMod(1L, Long.MinValue) == -9223372036854775807L).toBeTruthy
      expect(Math.floorMod(-1L, Long.MaxValue) == 9223372036854775806L).toBeTruthy
      expect(Math.floorMod(-1, Long.MinValue) == -1L).toBeTruthy
      expect(Math.floorMod(Long.MaxValue, 1L) == 0L).toBeTruthy
      expect(Math.floorMod(Long.MaxValue, -1L) == 0L).toBeTruthy
      expect(Math.floorMod(Long.MinValue, 1L) == 0L).toBeTruthy
      expect(Math.floorMod(Long.MinValue, -1L) == 0L).toBeTruthy

      for (n <- Seq(0L, 1L, -1L, Long.MaxValue, Long.MinValue))
        expectThrows[ArithmeticException](Math.floorMod(n, 0))
    }
  }
}
