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

package org.scalajs.testsuite.javalib.util

import java.nio.{ByteBuffer, LongBuffer}
import java.util.BitSet
import org.junit.Assert.{assertThrows => junitAssertThrows, _}
import org.junit.Assume._
import org.junit.Test
import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

class BitSetTest {
  @Test def test_Constructor_empty(): Unit = {
    val bs = new BitSet

    assertEquals("Create BitSet of incorrect size", 64, bs.size())
    assertEquals("New BitSet had invalid string representation", "{}", bs.toString())
  }

  @Test def test_Constructor_Int(): Unit = {
    var bs = new BitSet(128)

    assertEquals("Create BitSet of incorrect size", 128, bs.size())
    assertEquals("New BitSet had invalid string representation: " + bs.toString, "{}", bs.toString())

    // All BitSets are created with elements of multiples of 64 on jvm, 32 in JS
    bs = new BitSet(89)

    if (executingInJVM) {
      assertEquals("Failed to round BitSet element size", 128, bs.size())
    } else {
      assertEquals("Failed to round BitSet element size", 96, bs.size())
    }

    // "Failed to throw exception when creating a new BitSet with negative element value"
    assertThrows(classOf[NegativeArraySizeException], new BitSet(-9))
  }

  @Test def test_clone(): Unit = {
    val eightbs: BitSet = makeEightBS()
    val bs: BitSet = eightbs.clone.asInstanceOf[BitSet]
    assertEquals("clone failed to return equal BitSet", bs, eightbs)
  }

  @Test def test_equals(): Unit = {
    val eightbs: BitSet = makeEightBS()
    var bs: BitSet = makeEightBS()
    assertEquals("Same BitSet returned false", eightbs, eightbs)
    assertEquals("Identical BitSet returned false", bs, eightbs)
    bs.clear(6)
    assertFalse("Different BitSets returned true", eightbs == bs)

    bs = makeEightBS()
    bs.set(128)
    assertFalse("Different sized BitSet with higher bit set returned true", eightbs == bs)
    bs.clear(128)
    assertTrue("Different sized BitSet with higher bits not set returned false", eightbs == bs)
  }

  @Test def test_hashCode(): Unit = {
    val bs: BitSet = makeEightBS()
    bs.clear(2)
    bs.clear(6)
    assertEquals("BitSet returns wrong hash value", 1129, bs.hashCode)
    bs.set(10)
    bs.clear(3)
    assertEquals("BitSet returns wrong hash value", 97, bs.hashCode)
  }

  @Test def test_clear(): Unit = {
    val eightbs = makeEightBS()
    eightbs.clear()

    for (i <- 0 until 8)
      assertFalse("Clear didn't clear bit " + i, eightbs.get(i))

    assertEquals("Test1: Wrong length", 0, eightbs.length())
    val bs = new BitSet(3400)
    bs.set(0, bs.size - 1) // ensure all bits are 1's

    bs.set(bs.size - 1)
    bs.clear()
    assertEquals("Test2: Wrong length", 0, bs.length())
    assertTrue("Test2: isEmpty() returned incorrect value", bs.isEmpty())
    assertEquals("Test2: cardinality() returned incorrect value", 0, bs.cardinality())
  }

  @Test def test_clear_bitIndex(): Unit = {
    val eightbs  = makeEightBS()
    eightbs.clear(7)
    assertFalse("Failed to clear bit", eightbs.get(7))

    // Check to see all other bits are still set
    for (i <- 0 until 7)
      assertTrue("Clear cleared incorrect bits", eightbs.get(i))

    eightbs.clear(165)
    assertFalse("Failed to clear bit", eightbs.get(165))
    // Try out of range
    assertThrows(classOf[IndexOutOfBoundsException], eightbs.clear(-1))

    val bs = new BitSet(0)
    assertEquals("Test1: Wrong length,", 0, bs.length())
    assertEquals("Test1: Wrong size,", 0, bs.size())
    bs.clear(0)
    assertEquals("Test2: Wrong length,", 0, bs.length())
    assertEquals("Test2: Wrong size,", 0, bs.size())
    bs.clear(60)
    assertEquals("Test3: Wrong length,", 0, bs.length())
    assertEquals("Test3: Wrong size,", 0, bs.size())
    bs.clear(120)
    assertEquals("Test4: Wrong size,", 0, bs.size())
    assertEquals("Test4: Wrong length,", 0, bs.length())
    bs.set(25)
    if (executingInJVM) {
      assertEquals("Test5: Wrong size,", 64, bs.size())
    } else {
      assertEquals("Test5: Wrong size,", 32, bs.size())
    }

    assertEquals("Test5: Wrong length,", 26, bs.length())
    bs.clear(80)

    if (executingInJVM) {
      assertEquals("Test6: Wrong size,", 64, bs.size())
    } else {
      assertEquals("Test6: Wrong size,", 32, bs.size())
    }

    assertEquals("Test6: Wrong length,", 26, bs.length())
    bs.clear(25)

    if (executingInJVM) {
      assertEquals("Test7: Wrong size,", 64, bs.size())
    } else {
      assertEquals("Test7: Wrong size,", 32, bs.size())
    }

    assertEquals("Test7: Wrong length,", 0, bs.length())
  }

  @Test def test_clear_fromIndex_toIndex(): Unit = {
    val bitset = new BitSet
    for (i <- 0 until 20)
      bitset.set(i)

    bitset.clear(10, 10)
    // Test for method void java.BitSet.clear(int, int)
    // pos1 and pos2 are in the same bitset element
    var bs = new BitSet(16)
    var initialSize = bs.size
    bs.set(0, initialSize)
    bs.clear(5)
    bs.clear(15)
    bs.clear(7, 11)

    for (i <- 0 until 7) {
      if (i == 5)
        assertFalse("Shouldn't have flipped bit " + i, bs.get(i))
      else
        assertTrue("Shouldn't have cleared bit " + i, bs.get(i))
    }

    for (i <- 7 until 11)
      assertFalse("Failed to clear bit " + i, bs.get(i))

    for (i <- 11 until initialSize) {
      if (i == 15)
        assertFalse("Shouldn't have flipped bit " + i, bs.get(i))
      else
        assertTrue("Shouldn't have cleared bit " + i, bs.get(i))
    }

    for (i <- initialSize until bs.size())
      assertFalse("Shouldn't have flipped bit " + i, bs.get(i))

    // pos1 and pos2 is in the same bitset element, boundry testing
    bs = new BitSet(16)
    initialSize = bs.size
    bs.set(0, initialSize)
    bs.clear(7, 64)

    if (executingInJVM) {
      assertEquals("Failed to grow BitSet", 64, bs.size())
    } else {
      assertEquals("Failed to grow BitSet", 32, bs.size())
    }

    for (i <- 0 until 7)
      assertTrue("Shouldn't have cleared bit " + i, bs.get(i))

    for (i <- 7 until 64)
      assertFalse("Failed to clear bit " + i, bs.get(i))

    for (i <- 64 until bs.size())
      assertTrue("Shouldn't have flipped bit " + i, !bs.get(i))

    // more boundary testing
    bs = new BitSet(32)
    initialSize = bs.size
    bs.set(0, initialSize)
    bs.clear(0, 64)

    for (i <- 0 until 64)
      assertFalse("Failed to clear bit " + i, bs.get(i))

    for (i <- 64 until bs.size())
      assertFalse("Shouldn't have flipped bit " + i, bs.get(i))

    bs = new BitSet(32)
    initialSize = bs.size
    bs.set(0, initialSize)
    bs.clear(0, 65)

    for (i <- 0 until 65)
      assertFalse("Failed to clear bit " + i, bs.get(i))

    for (i <- 65 until bs.size())
      assertFalse("Shouldn't have flipped bit " + i, bs.get(i))

    // pos1 and pos2 are in two sequential bitset elements
    bs = new BitSet(128)
    initialSize = bs.size
    bs.set(0, initialSize)
    bs.clear(7)
    bs.clear(110)
    bs.clear(9, 74)

    for (i <- 0 until 9) {
      if (i == 7)
        assertFalse("Shouldn't have flipped bit " + i, bs.get(i))
      else
        assertTrue("Shouldn't have cleared bit " + i, bs.get(i))
    }

    for (i <- 9 until 74)
      assertFalse("Failed to clear bit " + i, bs.get(i))

    for (i <- 74 until initialSize) {
      if (i == 110)
        assertFalse("Shouldn't have flipped bit " + i, bs.get(i))
      else
        assertTrue("Shouldn't have cleared bit " + i, bs.get(i))
    }

    for (i <- initialSize until bs.size())
      assertFalse("Shouldn't have flipped bit " + i, bs.get(i))

    // pos1 and pos2 are in two non-sequential bitset elements
    bs = new BitSet(256)
    bs.set(0, 256)
    bs.clear(7)
    bs.clear(255)
    bs.clear(9, 219)

    for (i <- 0 until 9) {
      if (i == 7)
        assertFalse("Shouldn't have flipped bit " + i, bs.get(i))
      else
        assertTrue("Shouldn't have cleared bit " + i, bs.get(i))
    }

    for (i <- 9 until 219)
      assertFalse("failed to clear bit " + i, bs.get(i))

    for (i <- 219 until 255)
      assertTrue("Shouldn't have cleared bit " + i, bs.get(i))

    for (i <- 255 until bs.size())
      assertFalse("Shouldn't have flipped bit " + i, bs.get(i))

    // test illegal args
    bs = new BitSet(10)
    // "Test1: Attempt to flip with  negative index failed to generate exception"
    assertThrows(classOf[IndexOutOfBoundsException], bs.clear(-1, 3))
    // "Test2: Attempt to flip with negative index failed to generate exception"
    assertThrows(classOf[IndexOutOfBoundsException], bs.clear(2, -1))

    bs.set(2, 4)
    bs.clear(2, 2)
    assertTrue("Bit got cleared incorrectly ", bs.get(2))

    // "Test4: Attempt to flip with illegal args failed to generate exception"
    assertThrows(classOf[IndexOutOfBoundsException], bs.clear(4, 2))

    bs = new BitSet(0)
    assertEquals("Test1: Wrong length,", 0, bs.length())
    assertEquals("Test1: Wrong size,", 0, bs.size())

    bs.clear(0, 2)
    assertEquals("Test2: Wrong length,", 0, bs.length())
    assertEquals("Test2: Wrong size,", 0, bs.size())

    bs.clear(60, 64)
    assertEquals("Test3: Wrong length,", 0, bs.length())
    assertEquals("Test3: Wrong size,", 0, bs.size())

    bs.clear(64, 120)
    assertEquals("Test4: Wrong length,", 0, bs.length())
    assertEquals("Test4: Wrong size,", 0, bs.size())

    bs.set(25)
    assertEquals("Test5: Wrong length,", 26, bs.length())

    if (executingInJVM) {
      assertEquals("Test5: Wrong size,", 64, bs.size())
    } else {
      assertEquals("Test5: Wrong size,", 32, bs.size())
    }

    bs.clear(60, 64)
    assertEquals("Test6: Wrong length,", 26, bs.length())

    if (executingInJVM) {
      assertEquals("Test6: Wrong size,", 64, bs.size())
    } else {
      assertEquals("Test6: Wrong size,", 32, bs.size())
    }

    bs.clear(64, 120)

    if (executingInJVM) {
      assertEquals("Test7: Wrong size,", 64, bs.size())
    } else {
      assertEquals("Test7: Wrong size,", 32, bs.size())
    }

    assertEquals("Test7: Wrong length,", 26, bs.length())

    bs.clear(80)

    if (executingInJVM) {
      assertEquals("Test8: Wrong size,", 64, bs.size())
    } else {
      assertEquals("Test8: Wrong size,", 32, bs.size())
    }

    assertEquals("Test8: Wrong length,", 26, bs.length())

    bs.clear(25)
    if (executingInJVM) {
      assertEquals("Test9: Wrong size,", 64, bs.size())
    } else {
      assertEquals("Test9: Wrong size,", 32, bs.size())
    }

    assertEquals("Test9: Wrong length,", 0, bs.length())
  }

  @Test def test_get_bitIndex(): Unit = {
    val eightbs = makeEightBS()
    var bs = new BitSet
    bs.set(8)
    assertFalse("Get returned true for index out of range", eightbs.get(99))
    assertTrue("Get returned false for set value", eightbs.get(3))
    assertFalse("Get returned true for a non set value", bs.get(0))

    // "Attempt to get at negative index failed to generate exception"
    assertThrows(classOf[IndexOutOfBoundsException], bs.get(-1))

    bs = new BitSet(1)
    assertFalse("Access greater than size", bs.get(64))

    bs = new BitSet
    bs.set(63)
    assertTrue("Test highest bit", bs.get(63))

    bs = new BitSet(0)
    assertEquals("Test1: Wrong length,", 0, bs.length())
    assertEquals("Test1: Wrong size,", 0, bs.size())

    bs.get(2)
    assertEquals("Test2: Wrong length,", 0, bs.length())
    assertEquals("Test2: Wrong size,", 0, bs.size())

    bs.get(70)
    assertEquals("Test3: Wrong length,", 0, bs.length())
    assertEquals("Test3: Wrong size,", 0, bs.size())
  }

  @Test def test_get_fromIndex_toIndex(): Unit = {
    val bitset = new BitSet(30)
    bitset.get(3, 3)

    var bs: BitSet = new BitSet(512)
    var resultbs: BitSet = null
    var correctbs: BitSet = null

    bs.set(3, 9)
    bs.set(10, 20)
    bs.set(60, 75)
    bs.set(121)
    bs.set(130, 140)

    // pos1 and pos2 are in the same bitset element, at index0
    resultbs = bs.get(3, 6)
    correctbs = new BitSet(3)
    correctbs.set(0, 3)
    assertEquals("Test1: Returned incorrect BitSet", correctbs, resultbs)
    // pos1 and pos2 are in the same bitset element, at index 1
    resultbs = bs.get(100, 125)
    correctbs = new BitSet(25)
    correctbs.set(21)
    assertEquals("Test2: Returned incorrect BitSet", correctbs, resultbs)
    // pos1 in bitset element at index 0, and pos2 in bitset element at
    // index 1
    resultbs = bs.get(15, 125)
    correctbs = new BitSet(25)
    correctbs.set(0, 5)
    correctbs.set(45, 60)
    correctbs.set(121 - 15)
    assertEquals("Test3: Returned incorrect BitSet", correctbs, resultbs)
    // pos1 in bitset element at index 1, and pos2 in bitset element at
    // index 2
    resultbs = bs.get(70, 145)
    correctbs = new BitSet(75)
    correctbs.set(0, 5)
    correctbs.set(51)
    correctbs.set(60, 70)
    assertEquals("Test4: Returned incorrect BitSet", correctbs, resultbs)
    resultbs = bs.get(5, 145)
    correctbs = new BitSet(140)
    correctbs.set(0, 4)
    correctbs.set(5, 15)
    correctbs.set(55, 70)
    correctbs.set(116)
    correctbs.set(125, 135)
    assertEquals("Test5: Returned incorrect BitSet", correctbs, resultbs)
    // index 3
    resultbs = bs.get(5, 250)
    correctbs = new BitSet(200)
    correctbs.set(0, 4)
    correctbs.set(5, 15)
    correctbs.set(55, 70)
    correctbs.set(116)
    correctbs.set(125, 135)
    assertEquals("Test6: Returned incorrect BitSet", correctbs, resultbs)
    assertEquals("equality principle 1 ", bs.get(0, bs.size()), bs)
    // more tests
    var bs2 = new BitSet(129)
    bs2.set(0, 20)
    bs2.set(62, 65)
    bs2.set(121, 123)
    resultbs = bs2.get(1, 124)
    correctbs = new BitSet(129)
    correctbs.set(0, 19)
    correctbs.set(61, 64)
    correctbs.set(120, 122)
    assertEquals("Test7: Returned incorrect BitSet", correctbs, resultbs)
    // equality principle with some boundary conditions
    bs2 = new BitSet(128)
    bs2.set(2, 20)
    bs2.set(62)
    bs2.set(121, 123)
    bs2.set(127)
    resultbs = bs2.get(0, bs2.size())
    assertEquals("equality principle 2 ", resultbs, bs2)
    bs2 = new BitSet(128)
    bs2.set(2, 20)
    bs2.set(62)
    bs2.set(121, 123)
    bs2.set(127)
    bs2.flip(0, 128)
    resultbs = bs2.get(0, bs.size())
    assertEquals("equality principle 3 ", resultbs, bs2)
    bs = new BitSet(0)
    assertEquals("Test1: Wrong length,", 0, bs.length())
    assertEquals("Test1: Wrong size,", 0, bs.size())
    bs.get(0, 2)
    assertEquals("Test2: Wrong length,", 0, bs.length())
    assertEquals("Test2: Wrong size,", 0, bs.size())
    bs.get(60, 64)
    assertEquals("Test3: Wrong length,", 0, bs.length())
    assertEquals("Test3: Wrong size,", 0, bs.size())
    bs.get(64, 120)
    assertEquals("Test4: Wrong length,", 0, bs.length())
    assertEquals("Test4: Wrong size,", 0, bs.size())
    bs.set(25)
    assertEquals("Test5: Wrong length,", 26, bs.length())

    if (executingInJVM) {
      assertEquals("Test5: Wrong size,", 64, bs.size())
    } else {
      assertEquals("Test5: Wrong size,", 32, bs.size())
    }

    bs.get(60, 64)
    assertEquals("Test6: Wrong length,", 26, bs.length())

    if (executingInJVM) {
      assertEquals("Test6: Wrong size,", 64, bs.size())
    } else {
      assertEquals("Test6: Wrong size,", 32, bs.size())
    }

    bs.get(64, 120)

    if (executingInJVM) {
      assertEquals("Test7: Wrong size,", 64, bs.size())
    } else {
      assertEquals("Test7: Wrong size,", 32, bs.size())
    }

    assertEquals("Test7: Wrong length,", 26, bs.length())
    bs.get(80)

    if (executingInJVM) {
      assertEquals("Test8: Wrong size,", 64, bs.size())
    } else {
      assertEquals("Test8: Wrong size,", 32, bs.size())
    }

    assertEquals("Test8: Wrong length,", 26, bs.length())
    bs.get(25)

    if (executingInJVM) {
      assertEquals("Test9: Wrong size,", 64, bs.size())
    } else {
      assertEquals("Test9: Wrong size,", 32, bs.size())
    }

    assertEquals("Test9: Wrong length,", 26, bs.length())
  }

  @Test def test_flip_bitIndex(): Unit = {
    val eightbs = makeEightBS()
    var bs = new BitSet
    bs.clear(8)
    bs.clear(9)
    bs.set(10)
    bs.flip(9)
    assertFalse("Failed to flip bit", bs.get(8))
    assertTrue("Failed to flip bit", bs.get(9))
    assertTrue("Failed to flip bit", bs.get(10))
    bs.set(8)
    bs.set(9)
    bs.clear(10)
    bs.flip(9)
    assertTrue("Failed to flip bit", bs.get(8))
    assertFalse("Failed to flip bit", bs.get(9))
    assertFalse("Failed to flip bit", bs.get(10))

    // "Attempt to flip at negative index failed to generate exception"
    assertThrows(classOf[IndexOutOfBoundsException], bs.flip(-1))

    // Try setting a bit on a 64 boundary
    bs.flip(128)

    if (executingInJVM) {
      assertEquals("Failed to grow BitSet", 192, bs.size())
    } else {
      assertEquals("Failed to grow BitSet", 160, bs.size())
    }

    assertTrue("Failed to flip bit", bs.get(128))

    bs = new BitSet(64)
    var i = bs.size - 1
    while (i >= 0) {
      bs.flip(i)
      assertTrue("Test1: Incorrectly flipped bit" + i, bs.get(i))
      assertEquals("Incorrect length", i + 1, bs.length())
      var j: Int = bs.size
      while ({j -= 1; j} > i)
        assertTrue("Test2: Incorrectly flipped bit" + j, !bs.get(j))

      j = i
      while ({j -= 1; j} >= 0)
        assertTrue("Test3: Incorrectly flipped bit" + j, !bs.get(j))

      bs.flip(i)
      i -= 1
    }

    val bs0 = new BitSet(0)
    assertEquals("Test1: Wrong size", 0, bs0.size())
    assertEquals("Test1: Wrong length", 0, bs0.length())

    bs0.flip(0)

    if (executingInJVM) {
      assertEquals("Test2: Wrong size", 64, bs0.size())
    } else {
      assertEquals("Test2: Wrong size", 32, bs0.size())
    }

    assertEquals("Test2: Wrong length", 1, bs0.length())

    bs0.flip(63)
    assertEquals("Test3: Wrong size", 64, bs0.size())
    assertEquals("Test3: Wrong length", 64, bs0.length())

    eightbs.flip(7)
    assertTrue("Failed to flip bit 7", !eightbs.get(7))

    for (i <- 0 until 7)
      assertTrue("Flip flipped incorrect bits", eightbs.get(i))

    eightbs.flip(127)
    assertTrue("Failed to flip bit 127", eightbs.get(127))

    eightbs.flip(127)
    assertTrue("Failed to flip bit 127", !eightbs.get(127))
  }

  @Test def test_flip_fromIndex_toIndex(): Unit = {
    val bitset = new BitSet
    for (i <- 0 until 20)
      bitset.set(i)

    bitset.flip(10, 10)
    // Test for method void java.BitSet.flip(int, int)
    var bs = new BitSet(16)
    bs.set(7)
    bs.set(10)
    bs.flip(7, 11)

    for (i <- 0 until 7)
      assertTrue("Shouldn't have flipped bit " + i, !bs.get(i))

    assertFalse("Failed to flip bit 7", bs.get(7))
    assertTrue("Failed to flip bit 8", bs.get(8))
    assertTrue("Failed to flip bit 9", bs.get(9))
    assertFalse("Failed to flip bit 10", bs.get(10))

    for (i <- 11 until bs.size())
      assertTrue("Shouldn't have flipped bit " + i, !bs.get(i))

    bs = new BitSet(16)
    bs.set(7)
    bs.set(10)
    bs.flip(7, 64)
    assertEquals("Failed to grow BitSet", 64, bs.size())

    for (i <- 0 until 7)
      assertTrue("Shouldn't have flipped bit " + i, !bs.get(i))

    assertFalse("Failed to flip bit 7", bs.get(7))
    assertTrue("Failed to flip bit 8", bs.get(8))
    assertTrue("Failed to flip bit 9", bs.get(9))
    assertFalse("Failed to flip bit 10", bs.get(10))

    for (i <- 11 until 64)
      assertTrue("failed to flip bit " + i, bs.get(i))

    assertFalse("Shouldn't have flipped bit 64", bs.get(64))

    bs = new BitSet(32)
    bs.flip(0, 64)

    for (i <- 0 until 64)
      assertTrue("Failed to flip bit " + i, bs.get(i))

    assertFalse("Shouldn't have flipped bit 64", bs.get(64))

    bs = new BitSet(32)
    bs.flip(0, 65)

    for (i <- 0 until 65)
      assertTrue("Failed to flip bit " + i, bs.get(i))

    assertFalse("Shouldn't have flipped bit 65", bs.get(65))

    bs = new BitSet(128)
    bs.set(7)
    bs.set(10)
    bs.set(72)
    bs.set(110)
    bs.flip(9, 74)

    for (i <- 0 until 7)
      assertFalse("Shouldn't have flipped bit " + i, bs.get(i))

    assertTrue("Shouldn't have flipped bit 7", bs.get(7))
    assertFalse("Shouldn't have flipped bit 8", bs.get(8))
    assertTrue("Failed to flip bit 9", bs.get(9))
    assertFalse("Failed to flip bit 10", bs.get(10))

    for (i <- 11 until 72)
      assertTrue("failed to flip bit " + i, bs.get(i))

    assertFalse("Failed to flip bit 72", bs.get(72))
    assertTrue("Failed to flip bit 73", bs.get(73))

    for (i <- 74 until 110)
      assertFalse("Shouldn't have flipped bit " + i, bs.get(i))

    assertTrue("Shouldn't have flipped bit 110", bs.get(110))

    for (i <- 111 until bs.size())
      assertFalse("Shouldn't have flipped bit " + i, bs.get(i))

    bs = new BitSet(256)
    bs.set(7)
    bs.set(10)
    bs.set(72)
    bs.set(110)
    bs.set(181)
    bs.set(220)
    bs.flip(9, 219)

    for (i <- 0 until 7)
      assertFalse("Shouldn't have flipped bit " + i, bs.get(i))

    assertTrue("Shouldn't have flipped bit 7", bs.get(7))
    assertFalse("Shouldn't have flipped bit 8", bs.get(8))
    assertTrue("Failed to flip bit 9", bs.get(9))
    assertFalse("Failed to flip bit 10", bs.get(10))

    for (i <- 11 until 72)
      assertTrue("failed to flip bit " + i, bs.get(i))

    assertFalse("Failed to flip bit 72", bs.get(72))

    for (i <- 73 until 110)
      assertTrue("failed to flip bit " + i, bs.get(i))

    assertFalse("Failed to flip bit 110", bs.get(110))

    for (i <- 111 until 181)
      assertTrue("failed to flip bit " + i, bs.get(i))

    assertFalse("Failed to flip bit 181", bs.get(181))

    for (i <- 182 until 219)
      assertTrue("failed to flip bit " + i, bs.get(i))

    assertFalse("Shouldn't have flipped bit 219", bs.get(219))
    assertTrue("Shouldn't have flipped bit 220", bs.get(220))

    for (i <- 221 until bs.size())
      assertTrue("Shouldn't have flipped bit " + i, !bs.get(i))

    bs = new BitSet(10)
    // "Test1: Attempt to flip with  negative index failed to generate exception"
    assertThrows(classOf[IndexOutOfBoundsException], bs.flip(-1, 3))

    // "Test2: Attempt to flip with negative index failed to generate exception"
    assertThrows(classOf[IndexOutOfBoundsException], bs.flip(2, -1))

    // "Test4: Attempt to flip with illegal args failed to generate exception"
    assertThrows(classOf[IndexOutOfBoundsException], bs.flip(4, 2))
  }

  @Test def test_set_bitIndex(): Unit = {
    var bs = new BitSet
    bs.set(8)
    assertTrue("Failed to set bit", bs.get(8))

    // "Attempt to set at negative index failed to generate exception"
    assertThrows(classOf[IndexOutOfBoundsException], bs.set(-1))

    bs.set(128)
    if (executingInJVM) {
      assertEquals("Failed to grow BitSet", 192, bs.size())
    }  else {
      assertEquals("Failed to grow BitSet", 160, bs.size())
    }

    assertTrue("Failed to set bit", bs.get(128))

    bs = new BitSet(64)
    var i = bs.size
    while ({i -= 1; i} >= 0) {
      bs.set(i)
      assertTrue("Incorrectly set", bs.get(i))
      assertEquals("Incorrect length", i + 1, bs.length())

      var j = bs.size
      while ({j -= 1; j} > i)
        assertFalse("Incorrectly set bit " + j, bs.get(j))

      var k = i
      while ({k -= 1; k} >= 0)
        assertFalse("Incorrectly set bit " + k, bs.get(k))

      bs.clear(i)
    }

    bs = new BitSet(0)
    assertEquals("Test1: Wrong length", 0, bs.length())

    bs.set(0)
    assertEquals("Test2: Wrong length", 1, bs.length())
  }

  @Test def set_bitIndex_bool(): Unit = {
    val eightbs = makeEightBS()

    eightbs.set(5, false)
    assertFalse("Should have set bit 5 to true", eightbs.get(5))

    eightbs.set(5, true)
    assertTrue("Should have set bit 5 to false", eightbs.get(5))
  }

  @Test def set_fromIndex_toIndex(): Unit = {
    val bitset = new BitSet(30)
    bitset.set(29, 29)

    var bs = new BitSet(16)
    bs.set(5)
    bs.set(15)
    bs.set(7, 11)

    for (i <- 0 until 7) {
      if (i == 5)
        assertTrue("Shouldn't have flipped bit " + i, bs.get(i))
      else
        assertFalse("Shouldn't have set bit " + i, bs.get(i))
    }

    for (i <- 7 until 11)
      assertTrue("Failed to set bit " + i, bs.get(i))

    for (i <- 11 until bs.size()) {
      if (i == 15)
        assertTrue("Shouldn't have flipped bit " + i, bs.get(i))
      else
        assertFalse("Shouldn't have set bit " + i, bs.get(i))
    }

    bs = new BitSet(16)
    bs.set(7, 64)
    assertEquals("Failed to grow BitSet", 64, bs.size())

    for (i <- 0 until 7)
      assertFalse("Shouldn't have set bit " + i, bs.get(i))

    for (i <- 7 until 64)
      assertTrue("Failed to set bit " + i, bs.get(i))

    assertFalse("Shouldn't have set bit 64", bs.get(64))

    bs = new BitSet(32)
    bs.set(0, 64)

    for (i <- 0 until 64)
      assertTrue("Failed to set bit " + i, bs.get(i))

    assertFalse("Shouldn't have set bit 64", bs.get(64))

    bs = new BitSet(32)
    bs.set(0, 65)

    for (i <- 0 until 65)
      assertTrue("Failed to set bit " + i, bs.get(i))

    assertFalse("Shouldn't have set bit 65", bs.get(65))

    bs = new BitSet(128)
    bs.set(7)
    bs.set(110)
    bs.set(9, 74)

    for (i <- 0 until 9) {
      if (i == 7)
        assertTrue("Shouldn't have flipped bit " + i, bs.get(i))
      else
        assertFalse("Shouldn't have set bit " + i, bs.get(i))
    }

    for (i <- 9 until 74)
      assertTrue("Failed to set bit " + i, bs.get(i))

    for (i <- 74 until bs.size()) {
      if (i == 110)
        assertTrue("Shouldn't have flipped bit " + i, bs.get(i))
      else
        assertFalse("Shouldn't have set bit " + i, bs.get(i))
    }

    bs = new BitSet(256)
    bs.set(7)
    bs.set(255)
    bs.set(9, 219)

    for (i <- 0 until 9) {
      if (i == 7)
        assertTrue("Shouldn't have set flipped " + i, bs.get(i))
      else
        assertFalse("Shouldn't have set bit " + i, bs.get(i))
    }

    for (i <- 9 until 219)
      assertTrue("failed to set bit " + i, bs.get(i))

    for (i <- 219 until 255)
      assertFalse("Shouldn't have set bit " + i, bs.get(i))

    assertTrue("Shouldn't have flipped bit 255", bs.get(255))

    bs = new BitSet(10)
    // "Test1: Attempt to flip with  negative index failed to generate exception"
    assertThrows(classOf[IndexOutOfBoundsException], bs.set(-1, 3))

    // "Test2: Attempt to flip with negative index failed to generate exception"
    assertThrows(classOf[IndexOutOfBoundsException], bs.set(2, -1))

    bs.set(2, 2)
    assertFalse("Bit got set incorrectly ", bs.get(2))

    // "Test4: Attempt to flip with illegal args failed to generate exception"
    assertThrows(classOf[IndexOutOfBoundsException], bs.set(4, 2))
  }

  @Test def set_fromIndex_toIndex_bool(): Unit = {
    val eightbs = makeEightBS()

    eightbs.set(3, 6, false)
    assertTrue("Should have set bits 3, 4, and 5 to false",
        !eightbs.get(3) && !eightbs.get(4) && !eightbs.get(5))
    eightbs.set(3, 6, true)
    assertTrue("Should have set bits 3, 4, and 5 to true",
        eightbs.get(3) && eightbs.get(4) && eightbs.get(5))
  }

  @Test def intersects(): Unit = {
    val bs = new BitSet(500)
    bs.set(5)
    bs.set(63)
    bs.set(64)
    bs.set(71, 110)
    bs.set(127, 130)
    bs.set(192)
    bs.set(450)
    val bs2 = new BitSet(8)
    assertFalse(bs.intersects(bs2))
    assertFalse(bs2.intersects(bs))

    bs2.set(4)
    assertFalse(bs.intersects(bs2))
    assertFalse(bs2.intersects(bs))

    bs2.clear()
    bs2.set(5)
    assertTrue(bs.intersects(bs2))
    assertTrue(bs2.intersects(bs))

    bs2.clear()
    bs2.set(63)
    assertTrue(bs.intersects(bs2))
    assertTrue(bs2.intersects(bs))

    bs2.clear()
    bs2.set(80)
    assertTrue(bs.intersects(bs2))
    assertTrue(bs2.intersects(bs))

    bs2.clear()
    bs2.set(127)
    assertTrue(bs.intersects(bs2))
    assertTrue(bs2.intersects(bs))

    bs2.clear()
    bs2.set(192)
    assertTrue(bs.intersects(bs2))
    assertTrue(bs2.intersects(bs))

    bs2.clear()
    bs2.set(450)
    assertTrue(bs.intersects(bs2))
    assertTrue(bs2.intersects(bs))

    bs2.clear()
    bs2.set(500)
    assertFalse(bs.intersects(bs2))
    assertFalse(bs2.intersects(bs))
  }

  @Test def and(): Unit = {
    val eightbs = makeEightBS()
    val bs = new BitSet(128)

    // Initialize the bottom half of the BitSet
    for (i <- 64 until 128)
      bs.set(i)

    eightbs.and(bs)
    assertFalse(eightbs == bs)
    eightbs.set(3)
    bs.set(3)
    eightbs.and(bs)
    assertTrue(bs.get(3))
    bs.and(eightbs)

    for (i <- 64 until 128)
      assertFalse(bs.get(i))
  }

  def andNot(): Unit = {
    var bs = makeEightBS()
    bs.clear(5)
    val bs2 = new BitSet
    bs2.set(2)
    bs2.set(3)
    bs.andNot(bs2)
    assertEquals("{0, 1, 4, 6, 7}", bs.toString())
    bs = new BitSet(0)
    bs.andNot(bs2)
    assertEquals(0, bs.size())
  }

  @Test def or(): Unit = {
    val eightbs = makeEightBS()
    var bs = new BitSet(128)
    bs.or(eightbs)

    for (i <- 0 until 8)
      assertTrue("OR failed to set bits", bs.get(i))

    bs = new BitSet(0)
    bs.or(eightbs)

    for (i <- 0 until 8)
      assertTrue(bs.get(i))

    eightbs.clear(5)
    bs = new BitSet(128)
    bs.or(eightbs)
    assertFalse(bs.get(5))
  }

  @Test def xor(): Unit = {
    val eightbs = makeEightBS()
    var bs = makeEightBS()
    bs.xor(eightbs)

    for (i <- 0 until 8)
      assertFalse(bs.get(i))

    bs.xor(eightbs)

    for (i <- 0 until 8)
      assertTrue(bs.get(i))

    bs = new BitSet(0)
    bs.xor(eightbs)

    for (i <- 0 until 8)
      assertTrue(bs.get(i))

    bs = new BitSet
    bs.set(63)
    assertEquals("{63}", bs.toString())
  }

  @Test def size(): Unit = {
    val eightbs = makeEightBS()
    assertEquals(64, eightbs.size())
    eightbs.set(129)
    assertTrue(eightbs.size >= 129)
  }

  @Test def toStringTest(): Unit = {
    val bs = new BitSet
    assertEquals("{}", bs.toString())

    bs.set(0)
    assertEquals("{0}", bs.toString())

    bs.clear(0)
    assertEquals("{}", bs.toString())

    bs.set(0)
    bs.set(100)
    bs.set(200)
    bs.set(400)
    assertEquals("{0, 100, 200, 400}", bs.toString())

    bs.clear(200)
    assertEquals("{0, 100, 400}", bs.toString())

    bs.set(500)
    assertEquals("{0, 100, 400, 500}", bs.toString())
  }

  @Test def length(): Unit = {
    val bs = new BitSet
    assertEquals(0, bs.length())
    bs.set(5)
    assertEquals(6, bs.length())
    bs.set(10)
    assertEquals(11, bs.length())
    bs.set(432)
    assertEquals(433, bs.length())
    bs.set(300)
    assertEquals(433, bs.length())
  }

  @Test def previousClearBit(): Unit = {
    val bs = new BitSet(500)
    bs.set(5)
    bs.set(32)
    bs.set(63)
    bs.set(64)
    bs.set(71, 110)
    bs.set(127, 130)
    bs.set(193)
    bs.set(450)

    assertEquals(-1, bs.previousClearBit(-1))
    assertThrows(classOf[IndexOutOfBoundsException], bs.previousClearBit(-2))

    assertEquals(0, bs.previousClearBit(0))
    assertEquals(4, bs.previousClearBit(4))
    assertEquals(4, bs.previousClearBit(5))
    assertEquals(6, bs.previousClearBit(6))

    assertEquals(31, bs.previousClearBit(31))
    assertEquals(31, bs.previousClearBit(32))
    assertEquals(33, bs.previousClearBit(33))

    assertEquals(62, bs.previousClearBit(62))
    assertEquals(62, bs.previousClearBit(63))
    assertEquals(62, bs.previousClearBit(64))
    assertEquals(65, bs.previousClearBit(65))

    assertEquals(70, bs.previousClearBit(70))
    assertEquals(70, bs.previousClearBit(71))
    assertEquals(70, bs.previousClearBit(80))
    assertEquals(70, bs.previousClearBit(109))
    assertEquals(110, bs.previousClearBit(110))
    assertEquals(111, bs.previousClearBit(111))

    assertEquals(126, bs.previousClearBit(126))
    assertEquals(126, bs.previousClearBit(127))
    assertEquals(126, bs.previousClearBit(128))
    assertEquals(126, bs.previousClearBit(129))
    assertEquals(130, bs.previousClearBit(130))

    assertEquals(192, bs.previousClearBit(192))
    assertEquals(192, bs.previousClearBit(193))
    assertEquals(194, bs.previousClearBit(194))

    assertEquals(255, bs.previousClearBit(255))
    assertEquals(256, bs.previousClearBit(256))

    assertEquals(449, bs.previousClearBit(449))
    assertEquals(449, bs.previousClearBit(450))
    assertEquals(451, bs.previousClearBit(451))

    assertEquals(500, bs.previousClearBit(500))
    assertEquals(800, bs.previousClearBit(800))
  }

  @Test def previousSetBit(): Unit = {
    val bs = new BitSet(500)
    bs.set(5)
    bs.set(32)
    bs.set(63)
    bs.set(64)
    bs.set(71, 110)
    bs.set(127, 130)
    bs.set(193)
    bs.set(450)

    assertEquals(-1, bs.previousSetBit(-1))
    assertThrows(classOf[IndexOutOfBoundsException], bs.previousSetBit(-2))

    assertEquals(-1, bs.previousSetBit(0))
    assertEquals(-1, bs.previousSetBit(4))
    assertEquals(5, bs.previousSetBit(5))
    assertEquals(5, bs.previousSetBit(6))

    assertEquals(5, bs.previousSetBit(31))
    assertEquals(32, bs.previousSetBit(32))
    assertEquals(32, bs.previousSetBit(33))

    assertEquals(32, bs.previousSetBit(62))
    assertEquals(63, bs.previousSetBit(63))
    assertEquals(64, bs.previousSetBit(64))
    assertEquals(64, bs.previousSetBit(65))

    assertEquals(64, bs.previousSetBit(70))
    assertEquals(71, bs.previousSetBit(71))
    assertEquals(72, bs.previousSetBit(72))
    assertEquals(80, bs.previousSetBit(80))
    assertEquals(109, bs.previousSetBit(109))
    assertEquals(109, bs.previousSetBit(110))
    assertEquals(109, bs.previousSetBit(111))

    assertEquals(109, bs.previousSetBit(126))
    assertEquals(127, bs.previousSetBit(127))
    assertEquals(128, bs.previousSetBit(128))
    assertEquals(129, bs.previousSetBit(129))
    assertEquals(129, bs.previousSetBit(130))
    assertEquals(129, bs.previousSetBit(131))

    assertEquals(129, bs.previousSetBit(191))
    assertEquals(129, bs.previousSetBit(192))
    assertEquals(193, bs.previousSetBit(193))
    assertEquals(193, bs.previousSetBit(194))

    assertEquals(193, bs.previousSetBit(255))
    assertEquals(193, bs.previousSetBit(256))

    assertEquals(450, bs.previousSetBit(450))
    assertEquals(450, bs.previousSetBit(500))
    assertEquals(450, bs.previousSetBit(800))
  }

  @Test def nextSetBit(): Unit = {
    val bs = new BitSet(500)
    bs.set(5)
    bs.set(32)
    bs.set(63)
    bs.set(64)
    bs.set(71, 110)
    bs.set(127, 130)
    bs.set(193)
    bs.set(450)

    // "Expected IndexOutOfBoundsException for negative index"
    assertThrows(classOf[IndexOutOfBoundsException], bs.nextSetBit(-1))

    assertEquals(5, bs.nextSetBit(0))
    assertEquals(5, bs.nextSetBit(5))
    assertEquals(32, bs.nextSetBit(6))
    assertEquals(32, bs.nextSetBit(32))
    assertEquals(63, bs.nextSetBit(33))

    // boundary tests
    assertEquals(63, bs.nextSetBit(63))
    assertEquals(64, bs.nextSetBit(64))

    // at bitset element 1
    assertEquals(71, bs.nextSetBit(65))
    assertEquals(71, bs.nextSetBit(71))
    assertEquals(72, bs.nextSetBit(72))
    assertEquals(127, bs.nextSetBit(110))
    assertEquals(127, bs.nextSetBit(127))
    assertEquals(128, bs.nextSetBit(128))

    // at bitset element 2
    assertEquals(193, bs.nextSetBit(130))
    assertEquals(193, bs.nextSetBit(191))
    assertEquals(193, bs.nextSetBit(192))
    assertEquals(193, bs.nextSetBit(193))
    assertEquals(450, bs.nextSetBit(194))
    assertEquals(450, bs.nextSetBit(255))
    assertEquals(450, bs.nextSetBit(256))
    assertEquals(450, bs.nextSetBit(450))
    assertEquals(-1, bs.nextSetBit(451))
    assertEquals(-1, bs.nextSetBit(511))
    assertEquals(-1, bs.nextSetBit(512))
    assertEquals(-1, bs.nextSetBit(800))
  }

  @Test def test_nextClearBit_bitIndex(): Unit = {
    val bs = new BitSet(500)
    bs.set(0, bs.size - 1) // ensure all the bits from 0 to bs.size()

    // -1
    bs.set(bs.size - 1) // are set to true

    bs.clear(5)
    bs.clear(32)
    bs.clear(63)
    bs.clear(64)
    bs.clear(71, 110)
    bs.clear(127, 130)
    bs.clear(193)
    bs.clear(450)

    // "Expected IndexOutOfBoundsException for negative index"
    assertThrows(classOf[IndexOutOfBoundsException], bs.nextClearBit(-1))

    assertEquals(5, bs.nextClearBit(0))
    assertEquals(5, bs.nextClearBit(5))
    assertEquals(32, bs.nextClearBit(6))
    assertEquals(32, bs.nextClearBit(32))
    assertEquals(63, bs.nextClearBit(33))
    assertEquals(63, bs.nextClearBit(63))
    assertEquals(64, bs.nextClearBit(64))
    assertEquals(71, bs.nextClearBit(65))
    assertEquals(71, bs.nextClearBit(71))
    assertEquals(72, bs.nextClearBit(72))
    assertEquals(127, bs.nextClearBit(110))
    assertEquals(127, bs.nextClearBit(127))
    assertEquals(128, bs.nextClearBit(128))
    assertEquals(193, bs.nextClearBit(130))
    assertEquals(193, bs.nextClearBit(191))
    assertEquals(193, bs.nextClearBit(192))
    assertEquals(193, bs.nextClearBit(193))
    assertEquals(450, bs.nextClearBit(194))
    assertEquals(450, bs.nextClearBit(255))
    assertEquals(450, bs.nextClearBit(256))
    assertEquals(450, bs.nextClearBit(450))

    // bitset has 1 still the end of bs.size() -1, but calling nextClearBit with any index value
    // after the last true bit should return bs.size(),
    assertEquals(512, bs.nextClearBit(451))
    assertEquals(512, bs.nextClearBit(511))
    assertEquals(512, bs.nextClearBit(512))

    // if the index is larger than bs.size(), nextClearBit should return index;
    assertEquals(513, bs.nextClearBit(513))
    assertEquals(800, bs.nextClearBit(800))
  }

  @Test def isEmpty(): Unit = {
    val bs = new BitSet(500)
    assertTrue(bs.isEmpty())

    bs.set(3)
    assertFalse(bs.isEmpty())

    bs.clear()
    bs.set(12)
    assertFalse(bs.isEmpty())

    bs.clear()
    bs.set(128)
    assertFalse(bs.isEmpty())

    bs.clear()
    bs.set(459)
    assertFalse(bs.isEmpty())

    bs.clear()
    bs.set(511)
    assertFalse(bs.isEmpty())
  }

  @Test def cardinality(): Unit = {
    val bs = new BitSet(500)
    bs.set(5)
    bs.set(32)
    bs.set(63)
    bs.set(64)
    bs.set(71, 110)
    bs.set(127, 130)
    bs.set(193)
    bs.set(450)
    assertEquals(48, bs.cardinality())

    bs.flip(0, 500)
    assertEquals(452, bs.cardinality())

    bs.clear()
    assertEquals(0, bs.cardinality())

    bs.set(0, 500)
    assertEquals(500, bs.cardinality())
  }

  @Test def toByteArray(): Unit = {
    val bs = new BitSet(500)
    assertArrayEquals(Array[Byte](), bs.toByteArray())
    bs.set(5)
    assertArrayEquals(Array[Byte](32), bs.toByteArray())
    bs.set(32)
    assertArrayEquals(Array[Byte](32, 0, 0, 0, 1), bs.toByteArray())
    bs.set(63)
    assertArrayEquals(Array[Byte](32, 0, 0, 0, 1, 0, 0, -128), bs.toByteArray())
    bs.set(64)
    assertArrayEquals(Array[Byte](32, 0, 0, 0, 1, 0, 0, -128, 1), bs.toByteArray())
    bs.set(71, 110)
    assertArrayEquals(Array[Byte](32, 0, 0, 0, 1, 0, 0, -128, -127, -1, -1, -1, -1, 63), bs.toByteArray())
    bs.set(127, 130)
    assertArrayEquals(
        Array[Byte](32, 0, 0, 0, 1, 0, 0, -128, -127, -1, -1, -1, -1, 63, 0, -128, 3), bs.toByteArray())
    bs.set(193)
    assertArrayEquals(Array[Byte](32, 0, 0, 0, 1, 0, 0, -128, -127, -1, -1, -1, -1, 63, 0,
        -128, 3, 0, 0, 0, 0, 0, 0, 0, 2), bs.toByteArray())
    bs.set(450)
    assertArrayEquals(Array[Byte](32, 0, 0, 0, 1, 0, 0, -128, -127, -1, -1, -1, -1, 63, 0,
        -128, 3, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4), bs.toByteArray())
  }

  @Test def toLongArray(): Unit = {
    val bs = new BitSet(500)
    assertArrayEquals(Array[Long](), bs.toLongArray())
    bs.set(5)
    assertArrayEquals(Array[Long](32L), bs.toLongArray())
    bs.set(32)
    assertArrayEquals(Array[Long](4294967328L), bs.toLongArray())
    bs.set(63)
    assertArrayEquals(Array[Long](-9223372032559808480L), bs.toLongArray())
    bs.set(64)
    assertArrayEquals(Array[Long](-9223372032559808480L, 1L), bs.toLongArray())
    bs.set(71, 110)
    assertArrayEquals(Array[Long](-9223372032559808480L, 70368744177537L), bs.toLongArray())
    bs.set(127, 130)
    assertArrayEquals(Array[Long](-9223372032559808480L, -9223301668110598271L, 3L), bs.toLongArray())
    bs.set(193)
    assertArrayEquals(Array[Long](-9223372032559808480L, -9223301668110598271L, 3L, 2L), bs.toLongArray())
    bs.set(450)
    assertArrayEquals(Array[Long](-9223372032559808480L, -9223301668110598271L, 3L, 2L,
        0L, 0L, 0L, 4L), bs.toLongArray())
  }

  @Test def valueOf_ByteArray(): Unit = {
    val bs = new BitSet(500)
    assertEquals(bs, BitSet.valueOf(Array[Byte]()))
    bs.set(5)
    assertEquals(bs, BitSet.valueOf(Array[Byte](32)))
    bs.set(32)
    assertEquals(bs, BitSet.valueOf(Array[Byte](32, 0, 0, 0, 1)))
    bs.set(63)
    assertEquals(bs, BitSet.valueOf(Array[Byte](32, 0, 0, 0, 1, 0, 0, -128)))
    bs.set(64)
    assertEquals(bs, BitSet.valueOf(Array[Byte](32, 0, 0, 0, 1, 0, 0, -128, 1)))
    bs.set(71, 110)
    assertEquals(bs, BitSet.valueOf(Array[Byte](32, 0, 0, 0, 1, 0, 0, -128, -127, -1, -1, -1, -1, 63)))
    bs.set(127, 130)
    assertEquals(bs,
      BitSet.valueOf(Array[Byte](32, 0, 0, 0, 1, 0, 0, -128, -127, -1, -1, -1, -1, 63, 0, -128, 3)))
    bs.set(193)
    assertEquals(bs, BitSet.valueOf(Array[Byte](32, 0, 0, 0, 1, 0, 0, -128, -127, -1, -1, -1, -1, 63, 0,
      -128, 3, 0, 0, 0, 0, 0, 0, 0, 2)))
    bs.set(450)
    assertEquals(bs, BitSet.valueOf(Array[Byte](32, 0, 0, 0, 1, 0, 0, -128, -127, -1, -1, -1, -1, 63, 0,
      -128, 3, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4)))
  }

  @Test def valueOf_LongArray(): Unit = {
    val bs = new BitSet(500)
    assertEquals(bs, BitSet.valueOf(Array[Long]()))
    bs.set(5)
    assertEquals(bs, BitSet.valueOf(Array[Long](32L)))
    bs.set(32)
    assertEquals(bs, BitSet.valueOf(Array[Long](4294967328L)))
    bs.set(63)
    assertEquals(bs, BitSet.valueOf(Array[Long](-9223372032559808480L)))
    bs.set(64)
    assertEquals(bs, BitSet.valueOf(Array[Long](-9223372032559808480L, 1L)))
    bs.set(71, 110)
    assertEquals(bs, BitSet.valueOf(Array[Long](-9223372032559808480L, 70368744177537L)))
    bs.set(127, 130)
    assertEquals(bs, BitSet.valueOf(Array[Long](-9223372032559808480L, -9223301668110598271L, 3L)))
    bs.set(193)
    assertEquals(bs, BitSet.valueOf(Array[Long](-9223372032559808480L, -9223301668110598271L, 3L, 2L)))
    bs.set(450)
    assertEquals(bs, BitSet.valueOf(Array[Long](-9223372032559808480L, -9223301668110598271L, 3L, 2L,
      0L, 0L, 0L, 4L)))
  }

  @Test def valueOf_ByteBuffer(): Unit = {
    // Array-wrapped ByteBuffer
    val emptyBS = new BitSet
    val emptyBytes = emptyBS.toByteArray()

    assertEquals(emptyBS, BitSet.valueOf(ByteBuffer.wrap(emptyBytes)))

    val eightBS = makeEightBS()
    val eightBytes = eightBS.toByteArray()
    assertEquals(eightBS, BitSet.valueOf(ByteBuffer.wrap(eightBytes)))

    val bbWithPosition = ByteBuffer.wrap(192.toByte +: eightBytes)
    assertEquals(192.toByte, bbWithPosition.get()) // extra byte
    assertEquals(1, bbWithPosition.position())
    assertEquals(eightBS, BitSet.valueOf(bbWithPosition))
    assertEquals(1, bbWithPosition.position())

    // ByteBuffer.allocate()ed
    assertEquals(emptyBS, BitSet.valueOf(ByteBuffer.allocate(0)))

    val allocateByteBuffer = ByteBuffer.allocate(eightBytes.length + 1)
    allocateByteBuffer.put(192.toByte) // extra byte
    allocateByteBuffer.put(eightBytes)
    allocateByteBuffer.rewind()
    assertEquals(192.toByte, allocateByteBuffer.get()) // extra byte
    assertEquals(1, allocateByteBuffer.position())
    assertEquals(eightBS, BitSet.valueOf(allocateByteBuffer))
    assertEquals(1, allocateByteBuffer.position())
  }

  @Test def valueOf_ByteBuffer_typedArrays(): Unit = {
    val eightBS = makeEightBS()
    val eightBytes = eightBS.toByteArray()

    // ByteBuffer.allocateDirect()ed
    assertEquals(new BitSet, BitSet.valueOf(ByteBuffer.allocateDirect(0)))

    val directByteBuffer = ByteBuffer.allocateDirect(eightBytes.length + 1)
    directByteBuffer.put(192.toByte) // extra byte
    directByteBuffer.put(eightBytes)
    directByteBuffer.rewind()
    assertEquals(192.toByte, directByteBuffer.get()) // extra byte
    assertEquals(1, directByteBuffer.position())
    assertEquals(eightBS, BitSet.valueOf(directByteBuffer))
    assertEquals(1, directByteBuffer.position())
  }

  @Test def valueOf_LongBuffer(): Unit = {
    // Array-wrapped LongBuffer
    val emptyBS = new BitSet
    val emptyBSArray = emptyBS.toLongArray()
    assertEquals(emptyBS, BitSet.valueOf(LongBuffer.wrap(emptyBSArray)))

    val eightBS = makeEightBS()
    val eightBSArray = eightBS.toLongArray()
    assertEquals(eightBS, BitSet.valueOf(LongBuffer.wrap(eightBSArray)))

    val lbWithPosition = LongBuffer.wrap(192L +: eightBSArray)
    assertEquals(192L, lbWithPosition.get()) // extra byte
    assertEquals(1, lbWithPosition.position())
    assertEquals(eightBS, BitSet.valueOf(lbWithPosition))
    assertEquals(1, lbWithPosition.position())

    // LongBuffer.allocate()ed
    assertEquals(emptyBS, BitSet.valueOf(LongBuffer.allocate(0)))

    val allocateLongBuffer = LongBuffer.allocate(eightBSArray.length + 1)
    allocateLongBuffer.put(192L) // extra byte
    allocateLongBuffer.put(eightBSArray)
    allocateLongBuffer.rewind()
    assertEquals(192L, allocateLongBuffer.get()) // extra byte
    assertEquals(1, allocateLongBuffer.position())
    assertEquals(eightBS, BitSet.valueOf(allocateLongBuffer))
    assertEquals(1, allocateLongBuffer.position())
  }

  private def makeEightBS(): BitSet = {
    val eightbs = new BitSet
    for (i <- 0 until 8) {
      eightbs.set(i)
    }
    eightbs
  }
}
