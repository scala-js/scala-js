/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.testsuite.javalib.lang

import scala.scalajs.LinkingInfo.{linkTimeIf, moduleKind}
import scala.scalajs.LinkingInfo.ModuleKind.WasmModule

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

class SystemTest {

  @Test def setIn(): Unit = linkTimeIf(moduleKind == WasmModule) {
    assumeFalse("WasmModule does not support System.setIn()", true)
  } {
    val savedIn = System.in
    try {
      val testIn = new java.io.ByteArrayInputStream(Array[Byte]())
      System.setIn(testIn)
      assertTrue(System.in eq testIn)
    } finally {
      System.setIn(savedIn)
    }
  }

  @Test def setOut(): Unit = linkTimeIf(moduleKind == WasmModule) {
    assumeFalse("WasmModule does not support System.setOut()", true)
  } {
    val savedOut = System.out
    try {
      val testOut = new java.io.PrintStream(new java.io.ByteArrayOutputStream)
      System.setOut(testOut)
      assertTrue(System.out eq testOut)
    } finally {
      System.setOut(savedOut)
    }
  }

  @Test def setErr(): Unit = linkTimeIf(moduleKind == WasmModule) {
    assumeFalse("WasmModule does not support System.setErr()", true)
  } {
    val savedErr = System.err
    try {
      val testErr = new java.io.PrintStream(new java.io.ByteArrayOutputStream)
      System.setErr(testErr)
      assertTrue(System.err eq testErr)
    } finally {
      System.setErr(savedErr)
    }
  }

  @Test def currentTimeMillis(): Unit = linkTimeIf(moduleKind == WasmModule) {
    assumeFalse("WasmModule does not support System.currentTimeMillis()", true)
  } {
    // Test that the "scale" (order of magnitude) of currentTimeMillis() is correct
    val result = System.currentTimeMillis()
    assertTrue(result.toString(), result >= 1360059308000L) // timestamp of the first commit of Scala.js
    assertTrue(result.toString(), result <= 2937896108000L) // 50 years later
  }

  @Test def nanoTime(): Unit = linkTimeIf(moduleKind == WasmModule) {
    assumeFalse("WasmModule does not support System.nanoTime()", true)
  } {
    /* nanoTime() can return arbitrary results; even negative values.
     * It is supposed to be monotonic, but apparently it sometimes incorrectly
     * goes back in time: https://bugs.java.com/bugdatabase/view_bug?bug_id=6458294
     *
     * So the only thing we can test is that it links.
     */
    System.nanoTime()
  }

  @Test def identityHashCode(): Unit = {
    class HasIDHashCode

    val x1 = new HasIDHashCode
    val x2 = new HasIDHashCode
    val x1FirstHash = x1.hashCode()
    assertEquals(x1FirstHash, x1.hashCode())
    if (!executingInJVM)
      assertNotEquals(x1.hashCode(), x2.hashCode())
    assertEquals(x1FirstHash, x1.hashCode())

    assertEquals(x1FirstHash, System.identityHashCode(x1))
    assertEquals(x2.hashCode(), System.identityHashCode(x2))
  }

  @Test def identityHashCodeNotEqualHashCodeForList(): Unit = {
    val list1 = List(1, 3, 5)
    val list2 = List(1, 3, 5)
    assertEquals(list2, list1)
    assertEquals(list2.hashCode(), list1.hashCode())
    if (!executingInJVM)
      assertNotEquals(System.identityHashCode(list1), System.identityHashCode(list2))
  }

  @Test def identityHashCodeOfNull(): Unit =
    assertEquals(0, System.identityHashCode(null))

  @Test def lineSeparator(): Unit = {
    val lineSep = System.lineSeparator()

    if (!executingInJVM)
      assertEquals("\n", lineSep)
    else
      assertTrue(Set("\n", "\r", "\r\n").contains(lineSep))
  }

  @Test def identityHashCodeOfHijackedClasses(): Unit = {
    assumeFalse("the tested hash codes are Scala.js-specific", executingInJVM)

    /* None of the specific values here are by-spec. This test is highly
     * implementation-dependent. It is written like this to make sure that:
     *
     * - we are returning different values for different arguments, and
     * - the values are stable for the same value.
     *
     * However, the specific values are irrelevant and could be changed at any
     * time.
     */

    @noinline def test(hash: Int, x: Any): Unit =
      assertEquals("" + x, hash, System.identityHashCode(x))

    // force the creation of a new box (assuming there is a box at all)
    @noinline def doubleSum(a: Double, b: Double): Any = a + b

    // force the creation of a new string box
    @noinline def stringConcat(a: String, b: String): Any = a + b

    for (_ <- 0 until 2) { // do it twice to ensure stability
      test(101574, "foo")
      test(101574, stringConcat("f", "oo"))
      test(0, "")
      test(0, stringConcat("a", "b").toString().substring(2))

      test(1237, false)
      test(1231, true)

      test(5, 5)
      test(789456, 789456)

      test(0, 0.0)
      test(-2147483648, -0.0)
      test(1234, 1234.0)
      test(1073217536, 1.5)
      test(340593891, Math.PI)
      test(-54, -54.0)

      test(0, doubleSum(5.0, -5.0))
      test(-2147483648, doubleSum(-0.0, -0.0))
      test(1234, doubleSum(1000.0, 234.0))
      test(1073217536, doubleSum(1.0, 0.5))
      test(340593891, doubleSum(0.0, Math.PI))
      test(-54, doubleSum(-60.0, 6.0))

      test(1, Double.MinPositiveValue)
      test(1048576, Double.MinValue)
      test(-2146435072, Double.MaxValue)

      test(2146959360, Double.NaN)
      test(2146435072, Double.PositiveInfinity)
      test(-1048576, Double.NegativeInfinity)

      // See the comment in CoreWasmLib.genConstantBoxGlobals()
      val expectedUnitIDHashCode = if (isWasmModule) -1 else 0
      test(expectedUnitIDHashCode, ())
    }
  }

  @Test def getenvReturnsUnmodifiableMap(): Unit = {
    assertTrue(System.getenv().isInstanceOf[java.util.Map[String, String]])

    assertThrows(classOf[Exception], System.getenv.put("", ""))
  }

  @Test def getenvLinksAndDoesNotThrow(): Unit =
    assertEquals(null, System.getenv(":${PATH}"))
}
