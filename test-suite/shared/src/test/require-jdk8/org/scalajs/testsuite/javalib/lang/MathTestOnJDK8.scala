package org.scalajs.testsuite.javalib.lang

import org.junit.Assume._
import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

class MathTestOnJDK8 {

  @Test def addExact(): Unit = {
    assertEquals(0, Math.addExact(0, 0))
    assertEquals(1, Math.addExact(0, 1))
    assertEquals(1, Math.addExact(1, 0))
    assertEquals(-1, Math.addExact(0, -1))
    assertEquals(-1, Math.addExact(-1, 0))
    assertEquals(0, Math.addExact(1, -1))
    assertEquals(0, Math.addExact(-1, 1))
    assertEquals(Int.MinValue, Math.addExact(Int.MinValue, 0))
    assertEquals(Int.MinValue, Math.addExact(0, Int.MinValue))
    assertEquals(-2147483647, Math.addExact(Int.MinValue, 1))
    assertEquals(-2147483647, Math.addExact(1, Int.MinValue))
    assertEquals(Int.MinValue, Math.addExact(-1, -2147483647))
    assertEquals(Int.MinValue, Math.addExact(-2147483647, -1))
    assertEquals(Int.MaxValue, Math.addExact(Int.MaxValue, 0))
    assertEquals(Int.MaxValue, Math.addExact(0, Int.MaxValue))
    assertEquals(2147483646, Math.addExact(Int.MaxValue, -1))
    assertEquals(2147483646, Math.addExact(-1, Int.MaxValue))
    assertEquals(Int.MaxValue, Math.addExact(2147483646, 1))
    assertEquals(Int.MaxValue, Math.addExact(1, 2147483646))
    assertEquals(Int.MinValue, Math.addExact(-1073741824, -1073741824))

    expectThrows(classOf[ArithmeticException], Math.addExact(Int.MinValue, -1))
    expectThrows(classOf[ArithmeticException], Math.addExact(-1, Int.MinValue))
    expectThrows(classOf[ArithmeticException], Math.addExact(Int.MinValue, Int.MinValue))
    expectThrows(classOf[ArithmeticException], Math.addExact(Int.MaxValue, 1))
    expectThrows(classOf[ArithmeticException], Math.addExact(1, Int.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.addExact(Int.MaxValue, Int.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.addExact(1073741824, 1073741824))

    assertEquals(0L, Math.addExact(0L, 0L))
    assertEquals(1L, Math.addExact(0L, 1L))
    assertEquals(1L, Math.addExact(1L, 0L))
    assertEquals(-1L, Math.addExact(0L, -1L))
    assertEquals(-1L, Math.addExact(-1L, 0L))
    assertEquals(0L, Math.addExact(1L, -1L))
    assertEquals(0L, Math.addExact(-1L, 1L))
    assertEquals(Long.MinValue, Math.addExact(Long.MinValue, 0))
    assertEquals(Long.MinValue, Math.addExact(0, Long.MinValue))
    assertEquals(-9223372036854775807L, Math.addExact(Long.MinValue, 1))
    assertEquals(-9223372036854775807L, Math.addExact(1, Long.MinValue))
    assertEquals(Long.MinValue, Math.addExact(-9223372036854775807L, -1))
    assertEquals(Long.MinValue, Math.addExact(-1, -9223372036854775807L))
    assertEquals(Long.MaxValue, Math.addExact(Long.MaxValue, 0))
    assertEquals(Long.MaxValue, Math.addExact(0, Long.MaxValue))
    assertEquals(9223372036854775806L, Math.addExact(Long.MaxValue, -1))
    assertEquals(9223372036854775806L, Math.addExact(-1, Long.MaxValue))
    assertEquals(Long.MaxValue, Math.addExact(9223372036854775806L, 1))
    assertEquals(Long.MaxValue, Math.addExact(1, 9223372036854775806L))
    assertEquals(Long.MinValue, Math.addExact(-4611686018427387904L, -4611686018427387904L))

    expectThrows(classOf[ArithmeticException], Math.addExact(Long.MinValue, -1))
    expectThrows(classOf[ArithmeticException], Math.addExact(-1, Long.MinValue))
    expectThrows(classOf[ArithmeticException], Math.addExact(Long.MinValue, Long.MinValue))
    expectThrows(classOf[ArithmeticException], Math.addExact(Long.MaxValue, 1))
    expectThrows(classOf[ArithmeticException], Math.addExact(1, Long.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.addExact(Long.MaxValue, Long.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.addExact(4611686018427387904L, 4611686018427387904L))
  }

  @Test def subtractExact(): Unit = {
    assertEquals(0, Math.subtractExact(0, 0))
    assertEquals(1, Math.subtractExact(1, 0))
    assertEquals(-1, Math.subtractExact(0, 1))
    assertEquals(0, Math.subtractExact(1, 1))
    assertEquals(1, Math.subtractExact(0, -1))
    assertEquals(-1, Math.subtractExact(-1, 0))
    assertEquals(0, Math.subtractExact(-1, -1))
    assertEquals(Int.MinValue, Math.subtractExact(Int.MinValue, 0))
    assertEquals(Int.MaxValue, Math.subtractExact(Int.MaxValue, 0))
    assertEquals(-2147483647, Math.subtractExact(Int.MinValue, -1))
    assertEquals(2147483646, Math.subtractExact(Int.MaxValue, 1))
    assertEquals(Int.MinValue, Math.subtractExact(-1, Int.MaxValue))
    assertEquals(Int.MaxValue, Math.subtractExact(0, -Int.MaxValue))
    assertEquals(-2147483647, Math.subtractExact(0, Int.MaxValue))
    assertEquals(Int.MaxValue, Math.subtractExact(-1, Int.MinValue))
    assertEquals(Int.MinValue, Math.subtractExact(-1073741824, 1073741824))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(0, Int.MinValue))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(Int.MinValue, 1))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(Int.MinValue, Int.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(-2, Int.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(Int.MaxValue, -1))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(Int.MaxValue, Int.MinValue))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(1073741824, -1073741824))

    assertEquals(0L, Math.subtractExact(0L, 0L))
    assertEquals(1L, Math.subtractExact(1L, 0L))
    assertEquals(-1L, Math.subtractExact(0L, 1L))
    assertEquals(0L, Math.subtractExact(1L, 1L))
    assertEquals(1L, Math.subtractExact(0L, -1L))
    assertEquals(-1L, Math.subtractExact(-1L, 0L))
    assertEquals(0L, Math.subtractExact(-1L, -1L))
    assertEquals(Long.MinValue, Math.subtractExact(Long.MinValue, 0))
    assertEquals(Long.MaxValue, Math.subtractExact(Long.MaxValue, 0))
    assertEquals(-9223372036854775807L, Math.subtractExact(Long.MinValue, -1))
    assertEquals(9223372036854775806L, Math.subtractExact(Long.MaxValue, 1))
    assertEquals(Long.MinValue, Math.subtractExact(-1, Long.MaxValue))
    assertEquals(Long.MaxValue, Math.subtractExact(0, -Long.MaxValue))
    assertEquals(-9223372036854775807L, Math.subtractExact(0, Long.MaxValue))
    assertEquals(Long.MaxValue, Math.subtractExact(-1, Long.MinValue))
    assertEquals(Long.MinValue, Math.subtractExact(-4611686018427387904L, 4611686018427387904L))

    expectThrows(classOf[ArithmeticException], Math.subtractExact(0, Long.MinValue))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(Long.MinValue, 1))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(Long.MinValue, Long.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(Long.MinValue, 1))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(-2, Long.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(Long.MaxValue, -1))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(Long.MaxValue, Long.MinValue))
    expectThrows(classOf[ArithmeticException], Math.subtractExact(4611686018427387904L, -4611686018427387904L))
  }

  @Test def multiplyExact(): Unit = {
    for (n <- Seq(Int.MinValue, -1, 0, 1, Int.MaxValue)) {
      assertEquals(0, Math.multiplyExact(n, 0))
      assertEquals(0, Math.multiplyExact(0, n))
      assertEquals(n, Math.multiplyExact(n, 1))
      assertEquals(n, Math.multiplyExact(1, n))
    }
    assertEquals(Int.MaxValue, Math.multiplyExact(-2147483647, -1))
    assertEquals(Int.MaxValue, Math.multiplyExact(-1, -2147483647))
    assertEquals(2147483646, Math.multiplyExact(1073741823, 2))
    assertEquals(2147483646, Math.multiplyExact(2, 1073741823))
    assertEquals(Int.MinValue, Math.multiplyExact(1073741824, -2))
    assertEquals(Int.MinValue, Math.multiplyExact(-2, 1073741824))

    expectThrows(classOf[ArithmeticException], Math.multiplyExact(Int.MinValue, -1))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(-1, Int.MinValue))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(Int.MinValue, Int.MinValue))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(Int.MaxValue, Int.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(Int.MinValue, Int.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(Int.MaxValue, Int.MinValue))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(1073741824, 2))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(2, 1073741824))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(1073741825, -2))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(-2, 1073741825))

    for (n <- Seq(Long.MinValue, -1L, 0L, 1L, Long.MaxValue)) {
      assertEquals(0L, Math.multiplyExact(n, 0))
      assertEquals(0L, Math.multiplyExact(0, n))
      assertEquals(n, Math.multiplyExact(n, 1))
      assertEquals(n, Math.multiplyExact(1, n))
    }
    assertEquals(0L, Math.multiplyExact(Long.MinValue, 0))
    assertEquals(0L, Math.multiplyExact(0, Long.MinValue))
    assertEquals(Long.MaxValue, Math.multiplyExact(-9223372036854775807L, -1))
    assertEquals(Long.MaxValue, Math.multiplyExact(-1, -9223372036854775807L))
    assertEquals(9223372036854775806L, Math.multiplyExact(4611686018427387903L, 2))
    assertEquals(9223372036854775806L, Math.multiplyExact(2, 4611686018427387903L))
    assertEquals(Long.MinValue, Math.multiplyExact(4611686018427387904L, -2))
    assertEquals(Long.MinValue, Math.multiplyExact(-2, 4611686018427387904L))

    expectThrows(classOf[ArithmeticException], Math.multiplyExact(Long.MinValue, -1))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(-1, Long.MinValue))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(Long.MinValue, Long.MinValue))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(Long.MaxValue, Long.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(Long.MinValue, Long.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(Long.MaxValue, Long.MinValue))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(4611686018427387904L, 2))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(2, 4611686018427387904L))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(4611686018427387905L, -2))
    expectThrows(classOf[ArithmeticException], Math.multiplyExact(-2, 4611686018427387905L))
  }

  @Test def incrementExact(): Unit = {
    assertEquals(Int.MaxValue, Math.incrementExact(Int.MaxValue - 1))
    assertEquals(Long.MaxValue, Math.incrementExact(Long.MaxValue - 1))

    expectThrows(classOf[ArithmeticException], Math.incrementExact(Int.MaxValue))
    expectThrows(classOf[ArithmeticException], Math.incrementExact(Long.MaxValue))
  }

  @Test def decrementExact(): Unit = {
    assertEquals(Int.MinValue, Math.decrementExact(Int.MinValue + 1))
    assertEquals(Long.MinValue, Math.decrementExact(Long.MinValue + 1))

    expectThrows(classOf[ArithmeticException], Math.decrementExact(Int.MinValue))
    expectThrows(classOf[ArithmeticException], Math.decrementExact(Long.MinValue))
  }

  @Test def negateExact(): Unit = {
    assertEquals(Int.MaxValue, Math.negateExact(Int.MinValue + 1))
    assertEquals(Int.MinValue + 1, Math.negateExact(Int.MaxValue))
    assertEquals(Long.MaxValue, Math.negateExact(Long.MinValue + 1))
    assertEquals(Long.MinValue + 1, Math.negateExact(Long.MaxValue))

    expectThrows(classOf[ArithmeticException], Math.negateExact(Int.MinValue))
    expectThrows(classOf[ArithmeticException], Math.negateExact(Long.MinValue))
  }

  @Test def toIntExact(): Unit = {
    assertEquals(Int.MinValue, Math.toIntExact(-2147483648L))
    assertEquals(Int.MaxValue, Math.toIntExact(2147483647L))

    expectThrows(classOf[ArithmeticException], Math.toIntExact(-2147483649L))
    expectThrows(classOf[ArithmeticException], Math.toIntExact(2147483648L))
  }

  @Test def floorDiv(): Unit = {
    assertEquals(0, Math.floorDiv(0, 1))
    assertEquals(0, Math.floorDiv(0, -1))
    assertEquals(1, Math.floorDiv(1, 1))
    assertEquals(-1, Math.floorDiv(1, -1))
    assertEquals(0, Math.floorDiv(1, 2))
    assertEquals(-1, Math.floorDiv(1, -2))
    assertEquals(-1, Math.floorDiv(-1, 2))
    assertEquals(0, Math.floorDiv(-1, -2))
    assertEquals(0, Math.floorDiv(1, Int.MaxValue))
    assertEquals(-1, Math.floorDiv(1, Int.MinValue))
    assertEquals(-1, Math.floorDiv(-1, Int.MaxValue))
    assertEquals(0, Math.floorDiv(-1, Int.MinValue))
    assertEquals(Int.MaxValue, Math.floorDiv(Int.MaxValue, 1))
    assertEquals(-Int.MaxValue, Math.floorDiv(Int.MaxValue, -1))
    assertEquals(Int.MinValue, Math.floorDiv(Int.MinValue, 1))
    assertEquals(Int.MinValue, Math.floorDiv(Int.MinValue, -1))

    assertEquals(0L, Math.floorDiv(0L, 1L))
    assertEquals(0L, Math.floorDiv(0L, -1L))
    assertEquals(1L, Math.floorDiv(1L, 1L))
    assertEquals(-1L, Math.floorDiv(1L, -1L))
    assertEquals(0L, Math.floorDiv(1L, 2L))
    assertEquals(-1L, Math.floorDiv(1L, -2L))
    assertEquals(-1L, Math.floorDiv(-1L, 2L))
    assertEquals(0L, Math.floorDiv(-1L, -2L))
    assertEquals(0L, Math.floorDiv(1L, Long.MaxValue))
    assertEquals(-1L, Math.floorDiv(1L, Long.MinValue))
    assertEquals(-1L, Math.floorDiv(-1L, Long.MaxValue))
    assertEquals(0L, Math.floorDiv(-1L, Long.MinValue))
    assertEquals(Long.MaxValue, Math.floorDiv(Long.MaxValue, 1))
    assertEquals(-Long.MaxValue, Math.floorDiv(Long.MaxValue, -1))
    assertEquals(Long.MinValue, Math.floorDiv(Long.MinValue, 1))
    assertEquals(Long.MinValue, Math.floorDiv(Long.MinValue, -1))
    for (n <- Seq(0L, 1L, -1L, Long.MaxValue, Long.MinValue))
      expectThrows(classOf[ArithmeticException], Math.floorDiv(n, 0))
  }

  @Test def floorMod() = {
    assumeFalse("Assumed not executing in PhantomJS", executingInPhantomJS) // crashes otherwise, see #593

    assertEquals(0, Math.floorMod(0, 1))
    assertEquals(0, Math.floorMod(0, -1))
    assertEquals(0, Math.floorMod(1, 1))
    assertEquals(0, Math.floorMod(1, -1))
    assertEquals(1, Math.floorMod(1, 3))
    assertEquals(-2, Math.floorMod(1, -3))
    assertEquals(2, Math.floorMod(-1, 3))
    assertEquals(-1, Math.floorMod(-1, -3))
    assertEquals(1, Math.floorMod(1, Int.MaxValue))
    assertEquals(-2147483647, Math.floorMod(1, Int.MinValue))
    assertEquals(2147483646, Math.floorMod(-1, Int.MaxValue))
    assertEquals(-1, Math.floorMod(-1, Int.MinValue))
    assertEquals(0, Math.floorMod(Int.MaxValue, 1))
    assertEquals(0, Math.floorMod(Int.MaxValue, -1))
    assertEquals(0, Math.floorMod(Int.MinValue, 1))
    assertEquals(0, Math.floorMod(Int.MinValue, -1))

    assertEquals(0L, Math.floorMod(0L, 1L))
    assertEquals(0L, Math.floorMod(0L, -1L))
    assertEquals(0L, Math.floorMod(1L, 1L))
    assertEquals(0L, Math.floorMod(1L, -1L))
    assertEquals(1L, Math.floorMod(1L, 3L))
    assertEquals(-2L, Math.floorMod(1L, -3L))
    assertEquals(2L, Math.floorMod(-1L, 3L))
    assertEquals(-1L, Math.floorMod(-1L, -3L))
    assertEquals(1L, Math.floorMod(1L, Long.MaxValue))
    assertEquals(-9223372036854775807L, Math.floorMod(1L, Long.MinValue))
    assertEquals(9223372036854775806L, Math.floorMod(-1L, Long.MaxValue))
    assertEquals(-1L, Math.floorMod(-1, Long.MinValue))
    assertEquals(0L, Math.floorMod(Long.MaxValue, 1L))
    assertEquals(0L, Math.floorMod(Long.MaxValue, -1L))
    assertEquals(0L, Math.floorMod(Long.MinValue, 1L))
    assertEquals(0L, Math.floorMod(Long.MinValue, -1L))

    for (n <- Seq(0L, 1L, -1L, Long.MaxValue, Long.MinValue))
      assertThrows(classOf[ArithmeticException], Math.floorMod(n, 0))
  }
}
