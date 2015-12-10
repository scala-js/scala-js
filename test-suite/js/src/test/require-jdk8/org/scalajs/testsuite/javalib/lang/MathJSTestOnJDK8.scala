package org.scalajs.testsuite.javalib.lang

import org.scalajs.jasminetest.JasmineTest
import org.scalajs.testsuite.utils.ExpectExceptions

object MathJSTestOnJDK8 extends JasmineTest with ExpectExceptions {

  describe("java.lang.Math on JDK8 or higher") {
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
