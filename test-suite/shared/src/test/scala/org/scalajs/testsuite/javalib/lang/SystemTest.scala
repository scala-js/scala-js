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

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

class SystemTest {

  @Test def setIn(): Unit = {
    val savedIn = System.in
    try {
      val testIn = new java.io.ByteArrayInputStream(Array[Byte]())
      System.setIn(testIn)
      assertTrue(System.in eq testIn)
    } finally {
      System.setIn(savedIn)
    }
  }

  @Test def setOut(): Unit = {
    val savedOut = System.out
    try {
      val testOut = new java.io.PrintStream(new java.io.ByteArrayOutputStream)
      System.setOut(testOut)
      assertTrue(System.out eq testOut)
    } finally {
      System.setOut(savedOut)
    }
  }

  @Test def setErr(): Unit = {
    val savedErr = System.err
    try {
      val testErr = new java.io.PrintStream(new java.io.ByteArrayOutputStream)
      System.setErr(testErr)
      assertTrue(System.err eq testErr)
    } finally {
      System.setErr(savedErr)
    }
  }

  @Test def arraycopy(): Unit = {
    val object0 = Array[Any]("[", "b", "c", "d", "e", "f", "]")
    val object1 = Array[Any](() => true, 1, "2", '3', 4.0, true, object0)

    System.arraycopy(object1, 1, object0, 1, 5)
    if (executingInJVM) {
      assertEquals("[1234.0true]", object0.mkString)
    } else {
      assertEquals("[1234true]", object0.mkString)
    }

    val string0 = Array("a", "b", "c", "d", "e", "f")
    val string1 = Array("1", "2", "3", "4")

    System.arraycopy(string1, 0, string0, 3, 3)
    assertEquals("abc123", string0.mkString)

    val ab01Chars = Array("ab".toCharArray, "01".toCharArray)
    val chars = new Array[Array[Char]](32)
    System.arraycopy(ab01Chars, 0, chars, 0, 2)
    for (i <- Seq(0, 2, 4, 8, 16)) {
      System.arraycopy(chars, i / 4, chars, i, i)
    }

    assertEquals(12, chars.filter(_ == null).length)
    assertEquals("ab01ab0101ab01ab0101ab0101ab01ab0101ab01",
        chars.filter(_ != null).map(_.mkString).mkString)
  }

  @Test def arraycopyWithRangeOverlapsForTheSameArray(): Unit = {
    val array = new Array[Int](10)

    for (i <- 1 to 6) {
      array(i) = i
    }

    assertArrayEquals(Array(0, 1, 2, 3, 4, 5, 6, 0, 0, 0), array)
    System.arraycopy(array, 0, array, 3, 7)
    assertArrayEquals(Array(0, 1, 2, 0, 1, 2, 3, 4, 5, 6), array)

    System.arraycopy(array, 0, array, 1, 0)
    assertArrayEquals(Array(0, 1, 2, 0, 1, 2, 3, 4, 5, 6), array)

    System.arraycopy(array, 0, array, 1, 9)
    assertArrayEquals(Array(0, 0, 1, 2, 0, 1, 2, 3, 4, 5), array)

    System.arraycopy(array, 1, array, 0, 9)
    assertArrayEquals(Array(0, 1, 2, 0, 1, 2, 3, 4, 5, 5), array)

    System.arraycopy(array, 0, array, 0, 10)
    assertArrayEquals(Array(0, 1, 2, 0, 1, 2, 3, 4, 5, 5), array)

    val reversed = array.reverse
    System.arraycopy(reversed, 5, array, 5, 5)
    assertArrayEquals(Array(0, 1, 2, 0, 1, 1, 0, 2, 1, 0), array)
  }

  @Test def arraycopyIndexOutOfBounds(): Unit = {
    assumeTrue("Assuming compliant array errors", hasCompliantArrayErrors)

    val src = Array(0, 1, 2, 3, 4, 5, 6, 0, 0, 0)
    val dest = Array(11, 12, 13, 15, 15, 16)
    val original = Array(11, 12, 13, 15, 15, 16)

    assertThrows(classOf[ArrayIndexOutOfBoundsException],
        System.arraycopy(src, -1, dest, 3, 4))
    assertArrayEquals(original, dest)

    assertThrows(classOf[ArrayIndexOutOfBoundsException],
        System.arraycopy(src, 8, dest, 3, 4))
    assertArrayEquals(original, dest)

    assertThrows(classOf[ArrayIndexOutOfBoundsException],
        System.arraycopy(src, 1, dest, -1, 4))
    assertArrayEquals(original, dest)

    assertThrows(classOf[ArrayIndexOutOfBoundsException],
        System.arraycopy(src, 1, dest, 4, 4))
    assertArrayEquals(original, dest)

    assertThrows(classOf[ArrayIndexOutOfBoundsException],
        System.arraycopy(src, 11, dest, 3, 4))
    assertArrayEquals(original, dest)

    assertThrows(classOf[ArrayIndexOutOfBoundsException],
        System.arraycopy(src, 1, dest, 13, 4))
    assertArrayEquals(original, dest)

    assertThrows(classOf[ArrayIndexOutOfBoundsException],
        System.arraycopy(src, 1, dest, 3, Int.MaxValue))
    assertArrayEquals(original, dest)
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

  @Test def identityHashCodeOfNull(): Unit = {
    assertEquals(0, System.identityHashCode(null))
  }

  @Test def lineSeparator(): Unit = {
    val lineSep = System.lineSeparator()

    if (!executingInJVM)
      assertEquals("\n", lineSep)
    else
      assertTrue(Set("\n", "\r", "\r\n").contains(lineSep))
  }

  @Test def getenvReturnsUnmodifiableMap(): Unit = {
    assertTrue(System.getenv().isInstanceOf[java.util.Map[String, String]])

    assertThrows(classOf[Exception], System.getenv.put("", ""))
  }

  @Test def getenvLinksAndDoesNotThrow(): Unit = {
    assertEquals(null, System.getenv(":${PATH}"))
  }
}
