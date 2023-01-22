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

import java.{util => ju}
import java.util.function.{BiConsumer, BiFunction, Function}

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.javalib.util.concurrent.ConcurrentMapFactory
import org.scalajs.testsuite.utils.AssertThrows.{assertThrows, _}
import org.scalajs.testsuite.utils.Platform._

import scala.reflect.ClassTag

import Utils._

trait MapTest {
  import MapTest._

  def factory: MapFactory

  def testObj(i: Int): TestObj = TestObj(i)

  private def assumeNotIdentityHashMapOnJVM(): Unit =
    assumeFalse("JVM vs JS cache differences", executingInJVM && factory.isIdentityBased)

  @Test def testSizeGetPutWithStrings(): Unit = {
    assumeNotIdentityHashMapOnJVM()

    val mp = factory.empty[String, String]

    assertEquals(0, mp.size())
    mp.put("ONE", "one")
    assertEquals(1, mp.size())
    assertEquals("one", mp.get("ONE"))
    mp.put("TWO", "two")
    assertEquals(2, mp.size())
    assertEquals("two", mp.get("TWO"))
    mp.put("ONE", "three")
    assertEquals(2, mp.size())
    assertEquals("three", mp.get("ONE"))

    assertEquals(null, mp.get("THREE"))
    if (factory.allowsSupertypeKeyQueries) {
      assertEquals(null, mp.get(42))
      assertEquals(null, mp.get(testObj(42)))
    }
    if (factory.allowsNullKeysQueries)
      assertEquals(null, mp.get(null))
    else
      assertThrowsNPEIfCompliant(mp.get(null))
  }

  @Test def testSizeGetPutWithStringsLargeMap(): Unit = {
    assumeNotIdentityHashMapOnJVM()

    val largeMap = factory.empty[String, Int]
    for (i <- 0 until 1000)
      largeMap.put(i.toString(), i)
    val expectedSize = factory.withSizeLimit.fold(1000)(Math.min(_, 1000))
    assertEquals(expectedSize, largeMap.size())
    for (i <- (1000 - expectedSize) until 1000)
      assertEquals(i, largeMap.get(i.toString()))
    assertNull(largeMap.get("1000"))

    assertEquals(null, largeMap.get("THREE"))
    if (factory.allowsSupertypeKeyQueries) {
      assertEquals(null, largeMap.get(42))
      assertEquals(null, largeMap.get(testObj(42)))
    }
    if (factory.allowsNullKeysQueries)
      assertEquals(null, largeMap.get(null))
  }

  @Test def testSizeGetPutWithInts(): Unit = {
    assumeNotIdentityHashMapOnJVM()

    val mp = factory.empty[Int, Int]

    mp.put(100, 12345)
    assertEquals(1, mp.size())
    assertEquals(12345, mp.get(100))
    mp.put(150, 54321)
    assertEquals(2, mp.size())
    assertEquals(54321, mp.get(150))
    mp.put(100, 3)
    assertEquals(2, mp.size())
    assertEquals(3, mp.get(100))

    assertEquals(null, mp.get(42))
    if (factory.allowsSupertypeKeyQueries) {
      assertEquals(null, mp.get("THREE"))
      assertEquals(null, mp.get(testObj(42)))
    }
    if (factory.allowsNullKeysQueries)
      assertEquals(null, mp.get(null))
  }

  @Test def testSizeGetPutWithIntsLargeMap(): Unit = {
    assumeNotIdentityHashMapOnJVM()

    val largeMap = factory.empty[Int, Int]
    for (i <- 0 until 1000)
      largeMap.put(i, i * 2)
    val expectedSize = factory.withSizeLimit.fold(1000)(Math.min(_, 1000))
    assertEquals(expectedSize, largeMap.size())
    for (i <- (1000 - expectedSize) until 1000)
      assertEquals(i * 2, largeMap.get(i))
    assertNull(largeMap.get(1000))

    assertEquals(null, largeMap.get(-42))
    if (factory.allowsSupertypeKeyQueries) {
      assertEquals(null, largeMap.get("THREE"))
      assertEquals(null, largeMap.get(testObj(42)))
    }
    if (factory.allowsNullKeysQueries)
      assertEquals(null, largeMap.get(null))
  }

  @Test def testSizeGetPutWithCustomObjects(): Unit = {
    val mp = factory.empty[TestObj, TestObj]

    mp.put(testObj(100), TestObj(12345))
    assertEquals(1, mp.size())
    assertEquals(12345, mp.get(testObj(100)).num)
    mp.put(testObj(150), TestObj(54321))
    assertEquals(2, mp.size())
    assertEquals(54321, mp.get(testObj(150)).num)
    mp.put(testObj(100), TestObj(3))
    assertEquals(2, mp.size())
    assertEquals(3, mp.get(testObj(100)).num)

    assertEquals(null, mp.get(testObj(42)))
    if (factory.allowsSupertypeKeyQueries) {
      assertEquals(null, mp.get("THREE"))
      assertEquals(null, mp.get(42))
    }
    if (factory.allowsNullKeysQueries)
      assertEquals(null, mp.get(null))
  }

  @Test def testSizeGetPutWithCustomObjectsLargeMap(): Unit = {
    val largeMap = factory.empty[TestObj, Int]
    for (i <- 0 until 1000)
      largeMap.put(testObj(i), i * 2)
    val expectedSize = factory.withSizeLimit.fold(1000)(Math.min(_, 1000))
    assertEquals(expectedSize, largeMap.size())
    for (i <- (1000 - expectedSize) until 1000)
      assertEquals(i * 2, largeMap.get(testObj(i)))
    assertNull(largeMap.get(testObj(1000)))

    assertEquals(null, largeMap.get(testObj(-42)))
    if (factory.allowsSupertypeKeyQueries) {
      assertEquals(null, largeMap.get("THREE"))
      assertEquals(null, largeMap.get(42))
    }
    if (factory.allowsNullKeysQueries)
      assertEquals(null, largeMap.get(null))
  }

  @Test def testSizeGetPutWithDoublesCornerCasesOfEquals(): Unit = {
    assumeNotIdentityHashMapOnJVM()

    val mp = factory.empty[Double, Double]

    mp.put(1.2345, 11111.0)
    assertEquals(1, mp.size())
    val one = mp.get(1.2345)
    assertEquals(11111.0, one, 0.0)

    mp.put(Double.NaN, 22222.0)
    assertEquals(2, mp.size())
    val two = mp.get(Double.NaN)
    assertEquals(22222.0, two, 0.0)

    mp.put(+0.0, 33333.0)
    assertEquals(3, mp.size())
    val three = mp.get(+0.0)
    assertEquals(33333.0, three, 0.0)

    mp.put(-0.0, 44444.0)
    assertEquals(4, mp.size())
    val four = mp.get(-0.0)
    assertEquals(44444.0, four, 0.0)
  }

  @Test def testRemoveWithStrings(): Unit = {
    val mp = factory.empty[String, String]

    mp.put("ONE", "one")
    for (i <- 0 until 30)
      mp.put(s"key $i", s"value $i")
    assertEquals(31, mp.size())
    assertEquals("one", mp.remove("ONE"))
    assertNull(mp.get("ONE"))
    assertNull(mp.remove("ONE"))

    assertNull(mp.remove("foobar"))
    if (factory.allowsSupertypeKeyQueries) {
      assertNull(mp.remove(42))
      assertNull(mp.remove(testObj(42)))
    }
    if (factory.allowsNullKeys)
      assertNull(mp.remove(null))
    else
      assertThrowsNPEIfCompliant(mp.remove(null))
  }

  @Test def testRemoveWithInts(): Unit = {
    assumeNotIdentityHashMapOnJVM()

    val mp = factory.empty[Int, String]

    mp.put(543, "one")
    for (i <- 0 until 30)
      mp.put(i, s"value $i")
    assertEquals(31, mp.size())
    assertEquals("one", mp.remove(543))
    assertNull(mp.get(543))
    assertNull(mp.remove(543))

    assertNull(mp.remove(42))
    if (factory.allowsSupertypeKeyQueries) {
      assertNull(mp.remove("foobar"))
      assertNull(mp.remove(testObj(42)))
    }
    if (factory.allowsNullKeys)
      assertNull(mp.remove(null))
  }

  @Test def testRemoveWithCustomObjects(): Unit = {
    val mp = factory.empty[TestObj, String]

    mp.put(testObj(543), "one")
    for (i <- 0 until 30)
      mp.put(testObj(i), s"value $i")
    assertEquals(31, mp.size())
    assertEquals("one", mp.remove(testObj(543)))
    assertNull(mp.get(testObj(543)))
    assertNull(mp.remove(testObj(543)))

    assertNull(mp.remove(testObj(42)))
    if (factory.allowsSupertypeKeyQueries) {
      assertNull(mp.remove("foobar"))
      assertNull(mp.remove(42))
    }
    if (factory.allowsNullKeys)
      assertNull(mp.remove(null))
  }

  @Test def testRemoveWithDoublesCornerCasesOfEquals(): Unit = {
    assumeNotIdentityHashMapOnJVM()

    val mp = factory.empty[Double, String]

    mp.put(1.2345, "11111.0")
    mp.put(Double.NaN, "22222.0")
    mp.put(+0.0, "33333.0")
    mp.put(-0.0, "44444.0")

    assertEquals("11111.0", mp.get(1.2345))
    assertEquals("22222.0", mp.get(Double.NaN))
    assertEquals("33333.0", mp.get(+0.0))
    assertEquals("44444.0", mp.get(-0.0))

    assertEquals("44444.0", mp.remove(-0.0))
    assertNull(mp.get(-0.0))

    mp.put(-0.0, "55555.0")

    assertEquals("33333.0", mp.remove(+0.0))
    assertNull(mp.get(+0.0))

    mp.put(+0.0, "66666.0")

    assertEquals("22222.0", mp.remove(Double.NaN))
    assertNull(mp.get(Double.NaN))

    mp.put(Double.NaN, "77777.0")

    mp.clear()

    assertTrue(mp.isEmpty)
  }

  @Test def testGetPutRemoveNullKeys(): Unit = {
    val mp = factory.empty[String, String]
    for (i <- 0 until 30)
      mp.put(s"key $i", s"value $i")

    if (factory.allowsNullKeys) {
      mp.put(null, "one")
      assertEquals(31, mp.size())
      assertEquals("one", mp.get(null))
      assertEquals("one", mp.remove(null))
      assertNull(mp.get(null))
      assertNull(mp.remove(null))
    } else {
      assertThrowsNPEIfCompliant(mp.put(null, "one"))
    }
  }

  @Test def testGetPutRemoveNullValues(): Unit = {
    val mp = factory.empty[String, String]
    for (i <- 0 until 30)
      mp.put(s"key $i", s"value $i")

    if (factory.allowsNullValues) {
      mp.put("one", null)
      assertEquals(31, mp.size())
      assertNull(mp.get("one"))
      assertNull(mp.remove("one"))
      assertEquals(30, mp.size())
      assertNull(mp.get("one"))
    } else {
      assertThrowsNPEIfCompliant(mp.put("one", null))
    }
  }

  @Test def testClear(): Unit = {
    val mp = factory.empty[String, String]

    mp.put("ONE", "one")
    mp.put("TWO", "two")
    assertEquals(2, mp.size())
    mp.clear()

    // Test the content size
    assertEquals(0, mp.size())

    // Test the hash table
    assertNull(mp.get("ONE"))
    assertNull(mp.get("TWO"))

    // Test the iterators (different from the hash table for LinkedHashMap)
    assertFalse(mp.entrySet().iterator().hasNext())
    assertFalse(mp.keySet().iterator().hasNext())
    assertFalse(mp.values().iterator().hasNext())

    // can be reused after clear()
    mp.put("TWO", "value 2")
    mp.put("THREE", "value 3")
    assertEquals("value 2", mp.get("TWO"))
    assertEquals("value 3", mp.get("THREE"))
  }

  @Test def testContainsKey(): Unit = {
    val mp = factory.empty[String, String]

    mp.put("ONE", "one")
    assertTrue(mp.containsKey("ONE"))
    assertFalse(mp.containsKey("TWO"))
    if (factory.allowsNullKeysQueries)
      assertFalse(mp.containsKey(null))
    else
      assertThrowsNPEIfCompliant(mp.containsKey(null))
  }

  @Test def testContainsValue(): Unit = {
    val mp = factory.empty[String, String]

    mp.put("ONE", "one")
    assertTrue(mp.containsValue("one"))
    assertFalse(mp.containsValue("two"))
    if (factory.allowsNullValuesQueries)
      assertFalse(mp.containsValue(null))
    else
      assertThrowsNPEIfCompliant(mp.containsValue(null))
  }

  @Test def testPutAll(): Unit = {
    val mp = factory.empty[String, String]

    mp.put("ONE", "one")

    mp.putAll(TrivialImmutableMap("X" -> "y", "A" -> "b"))
    assertEquals(3, mp.size())
    assertEquals("one", mp.get("ONE"))
    assertEquals("y", mp.get("X"))
    assertEquals("b", mp.get("A"))

    val nullMap = TrivialImmutableMap((null: String) -> "y", "X" -> "z")
    if (factory.allowsNullKeys) {
      mp.putAll(nullMap)
      assertEquals("y", mp.get(null))
      assertEquals("z", mp.get("X"))
      assertEquals("one", mp.get("ONE"))
      assertEquals("b", mp.get("A"))
    } else {
      assertThrowsNPEIfCompliant(mp.putAll(nullMap))
    }
  }

  @Test def testValuesSizeIteratorBasic(): Unit = {
    val mp = factory.empty[String, String]
    mp.put("ONE", "one")

    val values = mp.values()
    assertEquals(1, values.size)
    val iter = values.iterator
    assertTrue(iter.hasNext)
    assertEquals("one", iter.next)
    assertFalse(iter.hasNext)
  }

  @Test def testEntrySetSizeIteratorBasic(): Unit = {
    val mp = factory.empty[String, String]
    mp.put("ONE", "one")

    val entrySet = mp.entrySet()
    assertEquals(1, entrySet.size)
    val iter = entrySet.iterator
    assertTrue(iter.hasNext)
    val next = iter.next
    assertFalse(iter.hasNext)
    assertEquals("ONE", next.getKey)
    assertEquals("one", next.getValue)
  }

  @Test def testKeySetSizeIteratorBasic(): Unit = {
    val mp = factory.empty[String, String]
    mp.put("ONE", "one")

    val keySet = mp.keySet()
    assertEquals(1, keySet.size)
    val iter = keySet.iterator
    assertTrue(iter.hasNext)
    assertEquals("ONE", iter.next)
    assertFalse(iter.hasNext)
  }

  @Test def testValuesIsViewForSize(): Unit = {
    val mp = factory.empty[String, String]
    mp.put("ONE", "one")
    mp.put("TWO", "two")
    val values = mp.values()

    assertEquals(2, values.size)
    mp.put("THREE", "three")
    assertEquals(3, values.size)
    mp.remove("ONE")
    assertEquals(2, values.size)
    assertFalse(values.isEmpty)
    mp.clear()
    assertEquals(0, values.size)
    assertTrue(values.isEmpty)
  }

  @Test def testValuesIsViewForQueriesWithStrings(): Unit = {
    val mp = factory.empty[String, String]
    mp.put("ONE", "one")
    mp.put("TWO", "two")
    val values = mp.values()

    assertTrue(values.contains("one"))
    assertTrue(values.contains("two"))
    assertFalse(values.contains("three"))
    if (factory.allowsNullValuesQueries)
      assertFalse(values.contains(null))
    else
      assertThrowsNPEIfCompliant(values.contains(null))

    mp.put("THREE", "three")

    assertTrue(values.contains("three"))

    val coll1 = TrivialImmutableCollection("one", "two", "three")
    assertTrue(values.containsAll(coll1))

    val coll2 = TrivialImmutableCollection("one", "two", "three", "four")
    assertFalse(values.containsAll(coll2))

    if (factory.allowsNullValuesQueries) {
      val coll3 = TrivialImmutableCollection("one", "two", "three", null)
      assertFalse(values.containsAll(coll3))
    }
  }

  @Test def testValuesIsViewForQueriesWithCustomObjects(): Unit = {
    val mp = factory.empty[TestObj, TestObj]
    mp.put(testObj(1), testObj(11))
    mp.put(testObj(2), testObj(22))
    val values = mp.values()

    assertTrue(values.contains(testObj(11)))
    assertTrue(values.contains(testObj(22)))
    assertFalse(values.contains(testObj(33)))
    if (factory.allowsNullValuesQueries)
      assertFalse(values.contains(null))
    else
      assertThrowsNPEIfCompliant(values.contains(null))

    mp.put(testObj(3), testObj(33))

    assertTrue(values.contains(testObj(33)))

    val coll1 = TrivialImmutableCollection(testObj(11), testObj(22),
        testObj(33))
    assertTrue(values.containsAll(coll1))

    val coll2 = TrivialImmutableCollection(testObj(11), testObj(22),
        testObj(33), testObj(44))
    assertFalse(values.containsAll(coll2))

    if (factory.allowsNullValuesQueries) {
      val coll3 = TrivialImmutableCollection(testObj(11), testObj(22),
          testObj(33), null)
      assertFalse(values.containsAll(coll3))
    }
  }

  @Test def testValuesIsViewForQueriesWithDoublesCornerCaseOfEquals(): Unit = {
    assumeNotIdentityHashMapOnJVM()

    val nummp = factory.empty[Double, Double]
    val numValues = nummp.values()

    nummp.put(1, +0.0)
    assertTrue(numValues.contains(+0.0))
    assertFalse(numValues.contains(-0.0))
    assertFalse(numValues.contains(Double.NaN))

    nummp.put(2, -0.0)
    assertTrue(numValues.contains(+0.0))
    assertTrue(numValues.contains(-0.0))
    assertFalse(numValues.contains(Double.NaN))

    nummp.put(3, Double.NaN)
    assertTrue(numValues.contains(+0.0))
    assertTrue(numValues.contains(-0.0))
    assertTrue(numValues.contains(Double.NaN))
  }

  @Test def testValuesIsViewForRemoveWithStrings(): Unit = {
    val mp = factory.empty[String, String]
    mp.put("ONE", "one")
    mp.put("TWO", "two")
    val values = mp.values()

    assertFalse(values.isEmpty)
    assertFalse(mp.isEmpty)

    values.clear()

    assertTrue(values.isEmpty)
    assertTrue(mp.isEmpty)

    mp.put("ONE", "one")
    mp.put("TWO", "two")

    assertTrue(mp.containsKey("ONE"))
    values.remove("one")
    assertFalse(mp.containsKey("ONE"))

    mp.put("ONE", "one")
    mp.put("THREE", "three")

    assertTrue(mp.containsKey("ONE"))
    assertTrue(mp.containsKey("TWO"))
    assertTrue(mp.containsKey("THREE"))

    values.removeAll(TrivialImmutableCollection("one", "two"))

    assertFalse(mp.containsKey("ONE"))
    assertFalse(mp.containsKey("TWO"))
    assertTrue(mp.containsKey("THREE"))

    mp.put("ONE", "one")
    mp.put("TWO", "two")
    mp.put("THREE", "three")

    assertTrue(mp.containsKey("ONE"))
    assertTrue(mp.containsKey("TWO"))
    assertTrue(mp.containsKey("THREE"))

    values.retainAll(TrivialImmutableCollection("one", "two"))

    assertTrue(mp.containsKey("ONE"))
    assertTrue(mp.containsKey("TWO"))
    assertFalse(mp.containsKey("THREE"))
  }

  @Test def testValuesIsViewForRemoveWithCustomObjects(): Unit = {
    val mp = factory.empty[TestObj, TestObj]
    mp.put(testObj(1), testObj(11))
    mp.put(testObj(2), testObj(22))
    val values = mp.values()

    assertFalse(values.isEmpty)
    assertFalse(mp.isEmpty)

    values.clear()

    assertTrue(values.isEmpty)
    assertTrue(mp.isEmpty)

    mp.put(testObj(1), testObj(11))
    mp.put(testObj(2), testObj(22))

    assertTrue(mp.containsKey(testObj(1)))
    values.remove(testObj(11))
    assertFalse(mp.containsKey(testObj(1)))

    mp.put(testObj(1), testObj(11))
    mp.put(testObj(3), testObj(33))

    assertTrue(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(testObj(2)))
    assertTrue(mp.containsKey(testObj(3)))

    values.removeAll(TrivialImmutableCollection(testObj(11), testObj(22)))

    assertFalse(mp.containsKey(testObj(1)))
    assertFalse(mp.containsKey(testObj(2)))
    assertTrue(mp.containsKey(testObj(3)))

    mp.put(testObj(1), testObj(11))
    mp.put(testObj(2), testObj(22))
    mp.put(testObj(3), testObj(33))

    assertTrue(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(testObj(2)))
    assertTrue(mp.containsKey(testObj(3)))

    values.retainAll(TrivialImmutableCollection(testObj(11), testObj(22)))

    assertTrue(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(testObj(2)))
    assertFalse(mp.containsKey(testObj(3)))
  }

  @Test def testKeySetIsViewForSize(): Unit = {
    val mp = factory.empty[String, String]
    mp.put("ONE", "one")
    mp.put("TWO", "two")
    val keySet = mp.keySet()

    assertEquals(2, keySet.size)
    mp.put("THREE", "three")
    assertEquals(3, keySet.size)
    mp.remove("ONE")
    assertEquals(2, keySet.size)
    assertFalse(keySet.isEmpty)
    mp.clear()
    assertEquals(0, keySet.size)
    assertTrue(keySet.isEmpty)
  }

  @Test def testKeySetIsViewForQueriesWithStrings(): Unit = {
    val mp = factory.empty[String, String]
    mp.put("ONE", "one")
    mp.put("TWO", "two")
    val keySet = mp.keySet()

    assertTrue(keySet.contains("ONE"))
    assertTrue(keySet.contains("TWO"))
    assertFalse(keySet.contains("THREE"))
    if (factory.allowsNullKeysQueries)
      assertFalse(keySet.contains(null))
    else
      assertThrowsNPEIfCompliant(keySet.contains(null))

    mp.put("THREE", "three")

    assertTrue(keySet.contains("THREE"))

    val coll1 = TrivialImmutableCollection("ONE", "TWO", "THREE")
    assertTrue(keySet.containsAll(coll1))

    val coll2 = TrivialImmutableCollection("ONE", "TWO", "THREE", "FOUR")
    assertFalse(keySet.containsAll(coll2))

    if (factory.allowsNullKeysQueries) {
      val coll3 = TrivialImmutableCollection("ONE", "TWO", "THREE", null)
      assertFalse(keySet.containsAll(coll3))
    }
  }

  @Test def testKeySetIsViewForQueriesWithCustomObjects(): Unit = {
    val mp = factory.empty[TestObj, TestObj]
    mp.put(testObj(1), TestObj(11))
    mp.put(testObj(2), TestObj(22))
    val keySet = mp.keySet()

    assertTrue(keySet.contains(testObj(1)))
    assertTrue(keySet.contains(testObj(2)))
    assertFalse(keySet.contains(testObj(3)))
    if (factory.allowsNullKeysQueries)
      assertFalse(keySet.contains(null))
    else
      assertThrowsNPEIfCompliant(keySet.contains(null))

    mp.put(testObj(3), TestObj(33))

    assertTrue(keySet.contains(testObj(3)))

    val coll1 = TrivialImmutableCollection(testObj(1), testObj(2), testObj(3))
    assertTrue(keySet.containsAll(coll1))

    val coll2 = TrivialImmutableCollection(testObj(1), testObj(2), testObj(4))
    assertFalse(keySet.containsAll(coll2))

    if (factory.allowsNullKeysQueries) {
      val coll3 = TrivialImmutableCollection(testObj(1), testObj(2), null)
      assertFalse(keySet.containsAll(coll3))
    }
  }

  @Test def testKeySetIsViewForQueriesWithDoublesCornerCaseOfEquals(): Unit = {
    assumeNotIdentityHashMapOnJVM()

    val nummp = factory.empty[Double, Double]
    val numkeySet = nummp.keySet()

    nummp.put(+0.0, 1)
    assertTrue(numkeySet.contains(+0.0))
    assertFalse(numkeySet.contains(-0.0))
    assertFalse(numkeySet.contains(Double.NaN))

    nummp.put(-0.0, 2)
    assertTrue(numkeySet.contains(+0.0))
    assertTrue(numkeySet.contains(-0.0))
    assertFalse(numkeySet.contains(Double.NaN))

    nummp.put(Double.NaN, 3)
    assertTrue(numkeySet.contains(+0.0))
    assertTrue(numkeySet.contains(-0.0))
    assertTrue(numkeySet.contains(Double.NaN))
  }

  @Test def testKeySetIsViewForRemoveWithStrings(): Unit = {
    val mp = factory.empty[String, String]
    mp.put("ONE", "one")
    mp.put("TWO", "two")
    val keySet = mp.keySet()

    assertFalse(keySet.isEmpty)
    assertFalse(mp.isEmpty)

    keySet.clear()

    assertTrue(keySet.isEmpty)

    assertTrue(mp.isEmpty)

    mp.put("ONE", "one")
    mp.put("TWO", "two")

    assertTrue(mp.containsKey("ONE"))
    keySet.remove("ONE")
    assertFalse(mp.containsKey("ONE"))

    mp.put("ONE", "one")
    mp.put("THREE", "three")

    assertTrue(mp.containsKey("ONE"))
    assertTrue(mp.containsKey("TWO"))
    assertTrue(mp.containsKey("THREE"))

    keySet.removeAll(TrivialImmutableCollection("ONE", "TWO", "FIVE"))

    assertFalse(mp.containsKey("ONE"))
    assertFalse(mp.containsKey("TWO"))
    assertTrue(mp.containsKey("THREE"))

    mp.put("ONE", "one")
    mp.put("TWO", "two")
    mp.put("THREE", "three")

    assertTrue(mp.containsKey("ONE"))
    assertTrue(mp.containsKey("TWO"))
    assertTrue(mp.containsKey("THREE"))

    keySet.retainAll(TrivialImmutableCollection("ONE", "TWO", "FIVE"))

    assertTrue(mp.containsKey("ONE"))
    assertTrue(mp.containsKey("TWO"))
    assertFalse(mp.containsKey("THREE"))

    if (factory.allowsNullKeys) {
      mp.put(null, "NULL")
      assertTrue(mp.containsKey(null))
      assertTrue(keySet.contains(null))
      assertTrue(keySet.remove(null))
      assertFalse(mp.containsKey(null))
    }

    if (factory.allowsNullValues) {
      mp.put("NULL", null)
      assertTrue(mp.containsKey("NULL"))
      assertTrue(keySet.contains("NULL"))
      assertTrue(keySet.remove("NULL"))
      assertFalse(mp.containsKey("NULL"))
    }
  }

  @Test def testKeySetIsViewForRemoveWithCustomObjects(): Unit = {
    val mp = factory.empty[TestObj, TestObj]
    mp.put(testObj(1), TestObj(11))
    mp.put(testObj(2), TestObj(22))
    val keySet = mp.keySet()

    assertFalse(keySet.isEmpty)
    assertFalse(mp.isEmpty)

    keySet.clear()

    assertTrue(keySet.isEmpty)

    assertTrue(mp.isEmpty)

    mp.put(testObj(1), TestObj(11))
    mp.put(testObj(2), TestObj(22))

    assertTrue(mp.containsKey(testObj(1)))
    keySet.remove(testObj(1))
    assertFalse(mp.containsKey(testObj(1)))

    mp.put(testObj(1), TestObj(11))
    mp.put(testObj(3), TestObj(33))

    assertTrue(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(testObj(2)))
    assertTrue(mp.containsKey(testObj(3)))

    keySet.removeAll(TrivialImmutableCollection(testObj(1), testObj(2),
        testObj(5)))

    assertFalse(mp.containsKey(testObj(1)))
    assertFalse(mp.containsKey(testObj(2)))
    assertTrue(mp.containsKey(testObj(3)))

    mp.put(testObj(1), TestObj(11))
    mp.put(testObj(2), TestObj(22))
    mp.put(testObj(3), TestObj(33))

    assertTrue(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(testObj(2)))
    assertTrue(mp.containsKey(testObj(3)))

    keySet.retainAll(TrivialImmutableCollection(testObj(1), testObj(2),
        testObj(5)))

    assertTrue(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(testObj(2)))
    assertFalse(mp.containsKey(testObj(3)))

    if (factory.allowsNullKeys) {
      mp.put(null, TestObj(111))
      assertTrue(mp.containsKey(null))
      assertTrue(keySet.contains(null))
      assertTrue(keySet.remove(null))
      assertFalse(mp.containsKey(null))
    }

    if (factory.allowsNullValues) {
      mp.put(testObj(4), null)
      assertTrue(mp.containsKey(testObj(4)))
      assertTrue(keySet.contains(testObj(4)))
      assertTrue(keySet.remove(testObj(4)))
      assertFalse(mp.containsKey(testObj(4)))
    }
  }

  @Test def testEntrySetIsViewForSize(): Unit = {
    val mp = factory.empty[String, String]
    mp.put("ONE", "one")
    mp.put("TWO", "two")
    val entrySet = mp.entrySet()

    assertEquals(2, entrySet.size)
    mp.put("THREE", "three")
    assertEquals(3, entrySet.size)
    mp.remove("ONE")
    assertEquals(2, entrySet.size)
    assertFalse(entrySet.isEmpty)
    mp.clear()
    assertEquals(0, entrySet.size)
    assertTrue(entrySet.isEmpty)
  }

  @Test def testEntrySetIsViewForQueriesWithStrings(): Unit = {
    val mp = factory.empty[String, String]
    mp.put("ONE", "one")
    mp.put("TWO", "two")
    if (factory.allowsNullKeys)
      mp.put(null, "NULL")
    if (factory.allowsNullValues)
      mp.put("NULL", null)
    val entrySet = mp.entrySet()

    assertTrue(entrySet.contains(SIE("ONE", "one")))
    assertTrue(entrySet.contains(SIE("TWO", "two")))
    assertFalse(entrySet.contains(SIE("THREE", "three")))
    assertFalse(entrySet.contains(SIE("ONE", "two")))
    assertFalse(entrySet.contains(SIE("THREE", "one")))

    if (factory.allowsNullKeysQueries)
      assertTrue(entrySet.contains(SIE(null, "NULL")))

    if (factory.allowsNullValuesQueries) {
      assertTrue(entrySet.contains(SIE("NULL", null)))
      assertFalse(entrySet.contains(SIE("NOTFOUND", null)))
    }

    mp.put("THREE", "three")

    assertTrue(entrySet.contains(SIE("THREE", "three")))

    val coll1 = TrivialImmutableCollection(SIE("ONE", "one"),
        SIE("TWO", "two"), SIE("THREE", "three"))
    assertTrue(entrySet.containsAll(coll1))

    val coll2 = TrivialImmutableCollection(SIE("ONE", "one"),
        SIE("TWO", "two"), SIE("THREE", "three"), SIE("FOUR", "four"))
    assertFalse(entrySet.containsAll(coll2))

    val coll3 = TrivialImmutableCollection(SIE("ONE", "one"),
        SIE("TWO", "four"), SIE("THREE", "three"))
    assertFalse(entrySet.containsAll(coll3))

    val coll4 = TrivialImmutableCollection(SIE("ONE", "one"),
        SIE("four", "two"), SIE("THREE", "three"))
    assertFalse(entrySet.containsAll(coll4))
  }

  @Test def testEntrySetIsViewForQueriesWithCustomObjects(): Unit = {
    val mp = factory.empty[TestObj, TestObj]
    mp.put(testObj(1), testObj(11))
    mp.put(testObj(2), testObj(22))
    if (factory.allowsNullValues)
      mp.put(testObj(5), null)
    if (factory.allowsNullKeys)
      mp.put(null, testObj(55))
    val entrySet = mp.entrySet()

    assertTrue(entrySet.contains(SIE(testObj(1), testObj(11))))
    assertTrue(entrySet.contains(SIE(testObj(2), testObj(22))))
    assertFalse(entrySet.contains(SIE(testObj(3), testObj(33))))
    assertFalse(entrySet.contains(SIE(testObj(1), testObj(22))))
    assertFalse(entrySet.contains(SIE(testObj(3), testObj(11))))
    if (factory.allowsNullValuesQueries) {
      assertTrue(entrySet.contains(SIE(testObj(5), null)))
      assertFalse(entrySet.contains(SIE(testObj(6), null)))
    }
    if (factory.allowsNullKeysQueries)
      assertTrue(entrySet.contains(SIE(null, testObj(55))))

    if (factory.allowsNullValuesQueries)
      assertFalse(entrySet.contains(SIE(testObj(7), null)))

    mp.put(testObj(3), testObj(33))

    assertTrue(entrySet.contains(SIE(testObj(3), testObj(33))))

    val coll1 = TrivialImmutableCollection(SIE(testObj(1), testObj(11)),
        SIE(testObj(2), testObj(22)), SIE(testObj(3), testObj(33)))
    assertTrue(entrySet.containsAll(coll1))

    val coll2 = TrivialImmutableCollection(SIE(testObj(1), testObj(11)),
        SIE(testObj(2), testObj(22)), SIE(testObj(3), testObj(33)),
        SIE(testObj(4), testObj(44)))
    assertFalse(entrySet.containsAll(coll2))

    val coll3 = TrivialImmutableCollection(SIE(testObj(1), testObj(11)),
        SIE(testObj(2), testObj(44)), SIE(testObj(3), testObj(33)))
    assertFalse(entrySet.containsAll(coll3))

    val coll4 = TrivialImmutableCollection(SIE(testObj(1), testObj(11)),
        SIE(testObj(4), testObj(22)), SIE(testObj(3), testObj(33)))
    assertFalse(entrySet.containsAll(coll4))
  }

  @Test def testEntrySetIsViewForRemoveWithStrings(): Unit = {
    val mp = factory.empty[String, String]
    mp.put("ONE", "one")
    mp.put("TWO", "two")
    val entrySet = mp.entrySet()

    assertFalse(entrySet.isEmpty)
    assertFalse(mp.isEmpty)

    entrySet.clear()
    assertTrue(entrySet.isEmpty)
    assertTrue(mp.isEmpty)

    mp.put("ONE", "one")
    mp.put("TWO", "two")
    if (factory.allowsNullKeys)
      mp.put(null, "NULL")
    if (factory.allowsNullValues)
      mp.put("NULL", null)

    assertTrue(mp.containsKey("ONE"))
    assertTrue(entrySet.remove(SIE("ONE", "one")))
    assertFalse(entrySet.remove(SIE("TWO", "four")))
    assertFalse(entrySet.remove("TWO"))
    assertFalse(mp.containsKey("ONE"))
    assertTrue(mp.containsKey("TWO"))
    if (factory.allowsNullKeysQueries)
      assertTrue(mp.containsKey(null))
    if (factory.allowsNullValuesQueries) {
      assertTrue(mp.containsValue(null))
      assertFalse(entrySet.remove(SIE("NOTFOUND", null)))
    }

    mp.put("ONE", "one")
    mp.put("THREE", "three")

    assertTrue(mp.containsKey("ONE"))
    assertTrue(mp.containsKey("TWO"))
    assertTrue(mp.containsKey("THREE"))

    entrySet.removeAll(TrivialImmutableCollection(SIE("ONE", "one"),
        SIE("TWO", "two"), SIE("THREE", "four"), "THREE", 42))

    assertFalse(mp.containsKey("ONE"))
    assertFalse(mp.containsKey("TWO"))
    assertTrue(mp.containsKey("THREE"))

    mp.put("ONE", "one")
    mp.put("TWO", "two")
    mp.put("THREE", "three")

    assertTrue(mp.containsKey("ONE"))
    assertTrue(mp.containsKey("TWO"))
    assertTrue(mp.containsKey("THREE"))

    entrySet.retainAll(TrivialImmutableCollection(SIE("ONE", "one"),
        SIE("TWO", "two"), SIE("THREE", "four"), "THREE", 42))

    assertTrue(mp.containsKey("ONE"))
    assertTrue(mp.containsKey("TWO"))
    assertFalse(mp.containsKey("THREE"))
  }

  @Test def testEntrySetIsViewForRemoveWithCustomObjects(): Unit = {
    val mp = factory.empty[TestObj, TestObj]
    mp.put(testObj(1), testObj(11))
    mp.put(testObj(2), testObj(22))
    val entrySet = mp.entrySet()

    assertFalse(entrySet.isEmpty)
    assertFalse(mp.isEmpty)

    entrySet.clear()
    assertTrue(entrySet.isEmpty)
    assertTrue(mp.isEmpty)

    mp.put(testObj(1), testObj(11))
    mp.put(testObj(2), testObj(22))
    if (factory.allowsNullKeys)
      mp.put(null, testObj(55))
    if (factory.allowsNullValues)
      mp.put(testObj(5), null)

    assertTrue(mp.containsKey(testObj(1)))
    assertTrue(entrySet.remove(SIE(testObj(1), testObj(11))))
    assertFalse(entrySet.remove(SIE(testObj(2), testObj(44))))
    assertFalse(entrySet.remove(testObj(2))) // remove should take Map.Entry
    assertFalse(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(testObj(2)))
    if (factory.allowsNullKeysQueries)
      assertTrue(mp.containsKey(null))
    if (factory.allowsNullValuesQueries) {
      assertTrue(mp.containsValue(null))
      assertFalse(entrySet.remove(SIE(testObj(6), null)))
    }

    mp.put(testObj(1), testObj(11))
    mp.put(testObj(3), testObj(33))

    assertTrue(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(testObj(2)))
    assertTrue(mp.containsKey(testObj(3)))

    entrySet.removeAll(TrivialImmutableCollection(SIE(testObj(1), testObj(11)),
        SIE(testObj(2), testObj(22)), SIE(testObj(3), testObj(44)),
        testObj(3), 42))

    assertFalse(mp.containsKey(testObj(1)))
    assertFalse(mp.containsKey(testObj(2)))
    assertTrue(mp.containsKey(testObj(3)))

    mp.put(testObj(1), testObj(11))
    mp.put(testObj(2), testObj(22))
    mp.put(testObj(3), testObj(33))

    assertTrue(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(testObj(2)))
    assertTrue(mp.containsKey(testObj(3)))

    entrySet.retainAll(TrivialImmutableCollection(SIE(testObj(1), testObj(11)),
        SIE(testObj(2), testObj(22)), SIE(testObj(3), testObj(44)),
        testObj(3), 42))

    assertTrue(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(testObj(2)))
    assertFalse(mp.containsKey(testObj(3)))
  }

  @Test def testEntrySetIsViewForSetValueWithStrings(): Unit = {
    val mp = factory.empty[String, String]
    mp.put("ONE", "one")
    mp.put("TWO", "two")
    val entrySet = mp.entrySet()

    val entry = entrySet.iterator().next()
    val key = entry.getKey()
    assertTrue(key == "ONE" || key == "TWO")
    val expectedValue = if (key == "ONE") "one" else "two"

    assertEquals(expectedValue, entry.getValue())
    assertEquals(expectedValue, entry.setValue("new value"))
    assertEquals("new value", entry.getValue())
    assertEquals("new value", mp.get(key))
  }

  @Test def testEntrySetIsViewForSetValueWithCustomObjects(): Unit = {
    val mp = factory.empty[TestObj, TestObj]
    mp.put(testObj(1), TestObj(11))
    mp.put(testObj(2), TestObj(22))
    val entrySet = mp.entrySet()

    val entry = entrySet.iterator().next()
    val key = entry.getKey()
    assertTrue(key.num == 1 || key.num == 2)
    val expectedValue = TestObj(if (key.num == 1) 11 else 22)

    assertEquals(expectedValue, entry.getValue())
    assertEquals(expectedValue, entry.setValue(TestObj(56)))
    assertEquals(TestObj(56), entry.getValue())
    assertEquals(TestObj(56), mp.get(key))
  }

  @Test def testGetOrDefault(): Unit = {
    val mp = factory.fromKeyValuePairs("ONE" -> "one", "TWO" -> "two", "THREE" -> "three")

    assertEquals("one", mp.getOrDefault("ONE", "def"))
    assertEquals("def", mp.getOrDefault("FOUR", "def"))
    assertNull(mp.getOrDefault("FIVE", null))

    if (factory.allowsNullValues) {
      mp.put("nullable", null)
      assertNull(mp.getOrDefault("nullable", "def"))
    }
  }

  @Test def testForEach(): Unit = {
    val mp = factory.fromKeyValuePairs("ONE" -> "one", "TWO" -> "two", "THREE" -> "three")

    val b = List.newBuilder[(String, String)]
    mp.forEach(new BiConsumer[String, String] {
      def accept(key: String, value: String): Unit =
        b += ((key, value))
    })
    val result = b.result()

    val expected = List("ONE" -> "one", "TWO" -> "two", "THREE" -> "three")

    if (factory.guaranteesInsertionOrder)
      assertEquals(expected, result)
    else
      assertEquals(expected.toSet, result.toSet)
  }

  @Test def testReplaceAll(): Unit = {
    val mp = factory.fromKeyValuePairs("ONE" -> "one", "TWO" -> "two", "THREE" -> "three")

    mp.replaceAll(new BiFunction[String, String, String] {
      def apply(key: String, value: String): String =
        s"$key -> $value"
    })

    assertEquals(3, mp.size())
    assertEquals("ONE -> one", mp.get("ONE"))
    assertEquals("TWO -> two", mp.get("TWO"))
    assertEquals("THREE -> three", mp.get("THREE"))

    if (factory.allowsNullValues) {
      mp.put("nullable", null)

      mp.replaceAll(new BiFunction[String, String, String] {
        def apply(key: String, value: String): String =
          if (key.startsWith("ONE")) null
          else if (value == null) "it was null"
          else value
      })

      assertTrue(mp.containsKey("ONE"))
      assertNull(mp.get("ONE"))
      assertEquals("it was null", mp.get("nullable"))
    } else {
      assertThrowsNPEIfCompliant(mp.replaceAll(new BiFunction[String, String, String] {
        def apply(key: String, value: String): String = null
      }))
    }
  }

  @Test def testPutIfAbsent(): Unit = {
    val mp = factory.empty[String, String]
    assertNull(mp.putIfAbsent("abc", "def"))
    assertEquals("def", mp.get("abc"))
    assertNull(mp.putIfAbsent("123", "456"))
    assertEquals("456", mp.get("123"))
    assertEquals("def", mp.putIfAbsent("abc", "def"))
    assertEquals("def", mp.putIfAbsent("abc", "ghi"))
    assertEquals("456", mp.putIfAbsent("123", "789"))
    assertEquals("def", mp.putIfAbsent("abc", "jkl"))

    if (factory.allowsNullValues) {
      mp.put("nullable", null)
      assertNull(mp.putIfAbsent("nullable", "non null"))
      assertEquals("non null", mp.get("nullable"))
    } else {
      assertThrowsNPEIfCompliant(mp.putIfAbsent("abc", null))
      assertThrowsNPEIfCompliant(mp.putIfAbsent("new key", null))
    }

    if (!factory.allowsNullKeys) {
      assertThrowsNPEIfCompliant(mp.putIfAbsent(null, "def"))
    }
  }

  @Test def testConditionalRemove(): Unit = {
    val mp = factory.fromKeyValuePairs("ONE" -> "one", "TWO" -> "two", "THREE" -> "three")

    assertFalse(mp.remove("non existing", "value"))
    assertFalse(mp.containsKey("non existing"))

    assertFalse(mp.remove("TWO", "one"))
    assertEquals("two", mp.get("TWO"))
    assertFalse(mp.remove("TWO", null))
    assertEquals("two", mp.get("TWO"))

    assertTrue(mp.remove("ONE", "one"))
    assertFalse(mp.containsKey("ONE"))

    if (factory.allowsNullKeys) {
      mp.put(null, "one")
      assertFalse(mp.remove(null, "not exist"))
      assertTrue(mp.containsKey(null))
      assertTrue(mp.remove(null, "one"))
      assertFalse(mp.containsKey(null))
    } else {
      assertThrowsNPEIfCompliant(mp.remove(null, "old value"))
    }

    if (factory.allowsNullValues) {
      mp.put("nullable", null)
      assertFalse(mp.remove("nullable", "value"))
      assertTrue(mp.containsKey("nullable"))
      assertTrue(mp.remove("nullable", null))
      assertFalse(mp.containsKey("nullable"))
    } else {
      // mp#(key, null) should not remove. https://bugs.java.com/bugdatabase/view_bug.do?bug_id=6272521
      assertFalse(mp.remove("THREE", null))
    }
  }

  @Test def testUnconditionalRemove(): Unit = {
    val mp = factory.fromKeyValuePairs("ONE" -> "one", "TWO" -> "two", "THREE" -> "three")

    assertEquals(null, mp.remove("non existing"))
    assertFalse(mp.containsKey("non existing"))

    assertEquals("two", mp.remove("TWO"))
    assertEquals(null, mp.get("TWO"))

    if (factory.allowsNullKeys) {
      mp.put(null, "one")
      assertTrue(mp.containsKey(null))
      assertEquals("one", mp.remove(null))
      assertFalse(mp.containsKey(null))
    } else {
      assertThrowsNPEIfCompliant(mp.remove(null))
    }
  }

  @Test def testConditionalReplace(): Unit = {
    val mp = factory.fromKeyValuePairs("ONE" -> "one", "TWO" -> "two", "THREE" -> "three")

    assertTrue(mp.replace("ONE", "one", "four"))
    assertEquals("four", mp.get("ONE"))
    assertFalse(mp.replace("TWO", "not two", "five"))
    assertEquals("two", mp.get("TWO"))
    assertFalse(mp.replace("non existing", "foo", "bar"))
    assertFalse(mp.containsKey("non existing"))

    if (factory.allowsNullValues) {
      assertFalse(mp.replace("ONE", null, "new value"))
      assertEquals("four", mp.get("ONE"))

      mp.put("nullable", null)
      assertFalse(mp.replace("nullable", "not null", "new value"))
      assertTrue(mp.replace("nullable", null, "nullable value"))
      assertEquals("nullable value", mp.get("nullable"))

      assertTrue(mp.replace("nullable", "nullable value", null))
      assertTrue(mp.containsKey("nullable"))
      assertNull(mp.get("nullable"))
    } else {
      assertThrowsNPEIfCompliant(mp.replace("ONE", null, "one"))
      assertThrowsNPEIfCompliant(mp.replace("ONE", "four", null))
    }

    if (factory.allowsNullKeys) {
      assertFalse(null, mp.replace(null, "value", "new value"))
      assertFalse(mp.containsKey(null))

      mp.put(null, "null value")
      assertTrue(mp.replace(null, "null value", "new value"))
      assertEquals("new value", mp.get(null))
    } else {
      assertThrowsNPEIfCompliant(mp.replace(null, "one", "two"))
    }
  }

  @Test def testUnconditionalReplace(): Unit = {
    val mp = factory.fromKeyValuePairs("ONE" -> "one", "TWO" -> "two", "THREE" -> "three")

    assertEquals("one", mp.replace("ONE", "four"))
    assertEquals("four", mp.get("ONE"))
    assertEquals("two", mp.get("TWO"))

    assertNull(mp.replace("non existing", "value"))
    assertFalse(mp.containsKey("non existing"))

    if (factory.allowsNullValues) {
      assertEquals("four", mp.replace("ONE", null))
      assertTrue(mp.containsKey("ONE"))
      assertNull(mp.get("ONE"))

      assertNull(mp.replace("ONE", "new one"))
      assertEquals("new one", mp.get("ONE"))
    } else {
      assertThrowsNPEIfCompliant(mp.replace("ONE", null))
      assertEquals("four", mp.get("ONE"))
    }

    if (factory.allowsNullKeys) {
      assertEquals(null, mp.replace(null, "value"))
      assertFalse(mp.containsKey(null))

      mp.put(null, "null value")
      assertEquals("null value", mp.replace(null, "new value"))
      assertEquals("new value", mp.get(null))
    } else {
      assertThrowsNPEIfCompliant(mp.replace(null, "one"))
    }
  }

  @Test def testComputeIfAbsent(): Unit = {
    val mp = factory.fromKeyValuePairs("ONE" -> "one", "TWO" -> "two", "THREE" -> "three")

    val notCalled = new Function[String, String] {
      def apply(key: String): String =
        throw new AssertionError(s"function should not have been called for $key")
    }

    val lengthAsString = new Function[String, String] {
      def apply(key: String): String = key.length().toString()
    }

    val returnsNull = new Function[String, String] {
      def apply(key: String): String = null
    }

    assertEquals("two", mp.computeIfAbsent("TWO", notCalled))
    assertEquals("5", mp.computeIfAbsent("SEVEN", lengthAsString))
    assertEquals("5", mp.get("SEVEN"))

    assertNull(mp.computeIfAbsent("non existing", returnsNull))
    assertFalse(mp.containsKey("non existing"))

    if (factory.allowsNullValues) {
      /* JDK 15 & 16 are affected by
       * https://bugs.openjdk.org/browse/JDK-8259622
       */
      assumeFalse("affected by JDK-8259622",
          executingInJVMOnLowerThanJDK17 && !executingInJVMOnLowerThanJDK15 &&
          mp.isInstanceOf[ju.TreeMap[_, _]])

      mp.put("nullable", null)
      assertEquals("8", mp.computeIfAbsent("nullable", lengthAsString))
      assertEquals("8", mp.get("nullable"))
    }
  }

  @Test def testComputeIfPresent(): Unit = {
    val mp = factory.fromKeyValuePairs("ONE" -> "one", "TWO" -> "two", "THREE" -> "three")

    val notCalled = new BiFunction[String, String, String] {
      def apply(key: String, value: String): String =
        throw new AssertionError(s"function should not have been called for $key")
    }

    val remappingFun = new BiFunction[String, String, String] {
      def apply(key: String, value: String): String = s"$key - $value"
    }

    val returnsNull = new BiFunction[String, String, String] {
      def apply(key: String, value: String): String = null
    }

    assertEquals("TWO - two", mp.computeIfPresent("TWO", remappingFun))
    assertEquals("TWO - two", mp.get("TWO"))

    assertNull(mp.computeIfPresent("ONE", returnsNull))
    assertFalse(mp.containsKey("ONE"))

    assertNull(mp.computeIfPresent("non existing", notCalled))
    assertFalse(mp.containsKey("non existing"))

    if (factory.allowsNullValues) {
      mp.put("nullable", null)
      assertNull(mp.computeIfPresent("nullable", notCalled))
      assertTrue(mp.containsKey("nullable"))
      assertNull(mp.get("nullable"))
    }
  }

  @Test def testCompute(): Unit = {
    val mp = factory.fromKeyValuePairs("ONE" -> "one", "TWO" -> "two", "THREE" -> "three")

    val remappingFun = new BiFunction[String, String, String] {
      def apply(key: String, value: String): String = s"$key - $value"
    }

    val returnsNull = new BiFunction[String, String, String] {
      def apply(key: String, value: String): String = null
    }

    assertEquals("TWO - two", mp.compute("TWO", remappingFun))
    assertEquals("TWO - two", mp.get("TWO"))

    assertEquals("SEVEN - null", mp.compute("SEVEN", remappingFun))
    assertEquals("SEVEN - null", mp.get("SEVEN"))

    assertNull(mp.compute("non existing", returnsNull))
    assertFalse(mp.containsKey("non existing"))

    assertNull(mp.compute("ONE", returnsNull))
    assertFalse(mp.containsKey("ONE"))

    if (factory.allowsNullValues) {
      mp.put("nullable", null)
      assertNull(mp.compute("nullable", returnsNull))
      assertFalse(mp.containsKey("nullable"))

      mp.put("nullable", null)
      assertEquals("nullable - null", mp.compute("nullable", remappingFun))
      assertEquals("nullable - null", mp.get("nullable"))
    }
  }

  @Test def testMerge(): Unit = {
    val mp = factory.fromKeyValuePairs("ONE" -> "one", "TWO" -> "two", "THREE" -> "three")

    val notCalled = new BiFunction[String, String, String] {
      def apply(prevValue: String, newValue: String): String =
        throw new AssertionError(s"function should not have been called for $prevValue")
    }

    val remappingFun = new BiFunction[String, String, String] {
      def apply(prevValue: String, newValue: String): String = s"$prevValue - $newValue"
    }

    val returnsNull = new BiFunction[String, String, String] {
      def apply(prevValue: String, newValue: String): String = null
    }

    assertEquals("two - def", mp.merge("TWO", "def", remappingFun))
    assertEquals("two - def", mp.get("TWO"))

    assertEquals("def", mp.merge("SEVEN", "def", notCalled))
    assertEquals("def", mp.get("SEVEN"))

    assertThrowsNPEIfCompliant(mp.merge("non existing", null, notCalled))
    assertThrowsNPEIfCompliant(mp.merge("ONE", null, notCalled))

    assertNull(mp.merge("ONE", "def", returnsNull))
    assertFalse(mp.containsKey("ONE"))

    if (factory.allowsNullValues) {
      mp.put("nullable", null)
      assertEquals("def", mp.merge("nullable", "def", notCalled))
      assertEquals("def", mp.get("nullable"))
    }
  }

  @Test def additionToKeySet(): Unit = {
    val set = factory.empty[String, String].keySet()

    assertThrows(classOf[UnsupportedOperationException], set.add("ONE"))
    assertThrows(classOf[UnsupportedOperationException], set.addAll(ju.Arrays.asList("ONE")))
    assertThrows(classOf[UnsupportedOperationException], set.addAll(ju.Arrays.asList(null)))
    assertThrows(classOf[UnsupportedOperationException], set.add(null))
  }
}

object MapTest {
  final case class TestObj(num: Int) extends Comparable[TestObj] {
    def compareTo(that: TestObj): Int = this.num - that.num
  }
}

trait MapFactory {
  def implementationName: String

  def empty[K: ClassTag, V: ClassTag]: ju.Map[K, V]

  def fromKeyValuePairs[K: ClassTag, V: ClassTag](pairs: (K, V)*): ju.Map[K, V] = {
    val result = empty[K, V]
    for ((key, value) <- pairs)
      result.put(key, value)
    result
  }

  def allowsNullKeys: Boolean

  def allowsNullValues: Boolean

  def allowsNullKeysQueries: Boolean = true

  def allowsNullValuesQueries: Boolean = true

  def allowsSupertypeKeyQueries: Boolean = false

  def withSizeLimit: Option[Int] = None

  def isIdentityBased: Boolean = false

  def guaranteesInsertionOrder: Boolean = false
}
