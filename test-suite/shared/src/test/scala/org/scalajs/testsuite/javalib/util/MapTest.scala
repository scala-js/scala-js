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

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.javalib.util.concurrent.ConcurrentMapFactory
import org.scalajs.testsuite.utils.AssertThrows._

import scala.reflect.ClassTag

trait MapTest {
  import MapTest._

  def factory: MapFactory

  @Test def testSizeGetPutWithStrings(): Unit = {
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
    assertEquals(null, mp.get(42))
    assertEquals(null, mp.get(TestObj(42)))
    if (factory.allowsNullKeysQueries)
      assertEquals(null, mp.get(null))
  }

  @Test def testSizeGetPutWithStringsLargeMap(): Unit = {
    val largeMap = factory.empty[String, Int]
    for (i <- 0 until 1000)
      largeMap.put(i.toString(), i)
    val expectedSize = factory.withSizeLimit.fold(1000)(Math.min(_, 1000))
    assertEquals(expectedSize, largeMap.size())
    for (i <- (1000 - expectedSize) until 1000)
      assertEquals(i, largeMap.get(i.toString()))
    assertNull(largeMap.get("1000"))

    assertEquals(null, largeMap.get("THREE"))
    assertEquals(null, largeMap.get(42))
    assertEquals(null, largeMap.get(TestObj(42)))
    if (factory.allowsNullKeysQueries)
      assertEquals(null, largeMap.get(null))
  }

  @Test def testSizeGetPutWithInts(): Unit = {
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
    assertEquals(null, mp.get("THREE"))
    assertEquals(null, mp.get(TestObj(42)))
    if (factory.allowsNullKeysQueries)
      assertEquals(null, mp.get(null))
  }

  @Test def testSizeGetPutWithIntsLargeMap(): Unit = {
    val largeMap = factory.empty[Int, Int]
    for (i <- 0 until 1000)
      largeMap.put(i, i * 2)
    val expectedSize = factory.withSizeLimit.fold(1000)(Math.min(_, 1000))
    assertEquals(expectedSize, largeMap.size())
    for (i <- (1000 - expectedSize) until 1000)
      assertEquals(i * 2, largeMap.get(i))
    assertNull(largeMap.get(1000))

    assertEquals(null, largeMap.get(-42))
    assertEquals(null, largeMap.get("THREE"))
    assertEquals(null, largeMap.get(TestObj(42)))
    if (factory.allowsNullKeysQueries)
      assertEquals(null, largeMap.get(null))
  }

  @Test def testSizeGetPutWithCustomObjects(): Unit = {
    val mp = factory.empty[TestObj, TestObj]

    mp.put(TestObj(100), TestObj(12345))
    assertEquals(1, mp.size())
    assertEquals(12345, mp.get(TestObj(100)).num)
    mp.put(TestObj(150), TestObj(54321))
    assertEquals(2, mp.size())
    assertEquals(54321, mp.get(TestObj(150)).num)
    mp.put(TestObj(100), TestObj(3))
    assertEquals(2, mp.size())
    assertEquals(3, mp.get(TestObj(100)).num)

    assertEquals(null, mp.get("THREE"))
    assertEquals(null, mp.get(42))
    assertEquals(null, mp.get(TestObj(42)))
    if (factory.allowsNullKeysQueries)
      assertEquals(null, mp.get(null))
  }

  @Test def testSizeGetPutWithCustomObjectsLargeMap(): Unit = {
    val largeMap = factory.empty[TestObj, Int]
    for (i <- 0 until 1000)
      largeMap.put(TestObj(i), i * 2)
    val expectedSize = factory.withSizeLimit.fold(1000)(Math.min(_, 1000))
    assertEquals(expectedSize, largeMap.size())
    for (i <- (1000 - expectedSize) until 1000)
      assertEquals(i * 2, largeMap.get(TestObj(i)))
    assertNull(largeMap.get(1000))

    assertEquals(null, largeMap.get(TestObj(-42)))
    assertEquals(null, largeMap.get("THREE"))
    assertEquals(null, largeMap.get(42))
    if (factory.allowsNullKeysQueries)
      assertEquals(null, largeMap.get(null))
  }

  @Test def testSizeGetPutWithDoublesCornerCasesOfEquals(): Unit = {
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
    assertNull(mp.remove(42))
    assertNull(mp.remove(TestObj(42)))
    if (factory.allowsNullKeys)
      assertNull(mp.remove(null))
  }

  @Test def testRemoveWithInts(): Unit = {
    val mp = factory.empty[Int, String]

    mp.put(543, "one")
    for (i <- 0 until 30)
      mp.put(i, s"value $i")
    assertEquals(31, mp.size())
    assertEquals("one", mp.remove(543))
    assertNull(mp.get(543))
    assertNull(mp.remove(543))

    assertNull(mp.remove("foobar"))
    assertNull(mp.remove(42))
    assertNull(mp.remove(TestObj(42)))
    if (factory.allowsNullKeys)
      assertNull(mp.remove(null))
  }

  @Test def testRemoveWithCustomObjects(): Unit = {
    val mp = factory.empty[TestObj, String]

    mp.put(TestObj(543), "one")
    for (i <- 0 until 30)
      mp.put(TestObj(i), s"value $i")
    assertEquals(31, mp.size())
    assertEquals("one", mp.remove(TestObj(543)))
    assertNull(mp.get(TestObj(543)))
    assertNull(mp.remove(TestObj(543)))

    assertNull(mp.remove(TestObj(42)))
    assertNull(mp.remove("foobar"))
    assertNull(mp.remove(42))
    if (factory.allowsNullKeys)
      assertNull(mp.remove(null))
  }

  @Test def testRemoveWithDoublesCornerCasesOfEquals(): Unit = {
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
      expectThrows(classOf[NullPointerException], mp.put(null, "one"))
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
      expectThrows(classOf[NullPointerException], mp.put("one", null))
    }
  }

  @Test def testClear(): Unit = {
    val mp = factory.empty[String, String]

    mp.put("ONE", "one")
    mp.put("TWO", "two")
    assertEquals(2, mp.size())
    mp.clear()
    assertEquals(0, mp.size())
    assertNull(mp.get("ONE"))
    assertNull(mp.get("TWO"))

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
      expectThrows(classOf[NullPointerException], mp.containsKey(null))
  }

  @Test def testContainsValue(): Unit = {
    val mp = factory.empty[String, String]

    mp.put("ONE", "one")
    assertTrue(mp.containsValue("one"))
    assertFalse(mp.containsValue("two"))
    if (factory.allowsNullValuesQueries)
      assertFalse(mp.containsValue(null))
    else
      expectThrows(classOf[NullPointerException], mp.containsValue(null))
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
      expectThrows(classOf[NullPointerException], mp.putAll(nullMap))
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
      expectThrows(classOf[NullPointerException], values.contains(null))

    mp.put("THREE", "three")

    assertTrue(values.contains("three"))

    val coll1 = ju.Arrays.asList("one", "two", "three")
    assertTrue(values.containsAll(coll1))

    val coll2 = ju.Arrays.asList("one", "two", "three", "four")
    assertFalse(values.containsAll(coll2))

    if (factory.allowsNullValuesQueries) {
      val coll3 = ju.Arrays.asList("one", "two", "three", null)
      assertFalse(values.containsAll(coll3))
    }
  }

  @Test def testValuesIsViewForQueriesWithCustomObjects(): Unit = {
    val mp = factory.empty[TestObj, TestObj]
    mp.put(TestObj(1), TestObj(11))
    mp.put(TestObj(2), TestObj(22))
    val values = mp.values()

    assertTrue(values.contains(TestObj(11)))
    assertTrue(values.contains(TestObj(22)))
    assertFalse(values.contains(TestObj(33)))
    if (factory.allowsNullValuesQueries)
      assertFalse(values.contains(null))
    else
      expectThrows(classOf[NullPointerException], values.contains(null))

    mp.put(TestObj(3), TestObj(33))

    assertTrue(values.contains(TestObj(33)))

    val coll1 = ju.Arrays.asList(TestObj(11), TestObj(22), TestObj(33))
    assertTrue(values.containsAll(coll1))

    val coll2 = ju.Arrays.asList(TestObj(11), TestObj(22), TestObj(33), TestObj(44))
    assertFalse(values.containsAll(coll2))

    if (factory.allowsNullValuesQueries) {
      val coll3 = ju.Arrays.asList(TestObj(11), TestObj(22), TestObj(33), null)
      assertFalse(values.containsAll(coll3))
    }
  }

  @Test def testValuesIsViewForQueriesWithDoublesCornerCaseOfEquals(): Unit = {
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

    values.removeAll(ju.Arrays.asList("one", "two"))

    assertFalse(mp.containsKey("ONE"))
    assertFalse(mp.containsKey("TWO"))
    assertTrue(mp.containsKey("THREE"))

    mp.put("ONE", "one")
    mp.put("TWO", "two")
    mp.put("THREE", "three")

    assertTrue(mp.containsKey("ONE"))
    assertTrue(mp.containsKey("TWO"))
    assertTrue(mp.containsKey("THREE"))

    values.retainAll(ju.Arrays.asList("one", "two"))

    assertTrue(mp.containsKey("ONE"))
    assertTrue(mp.containsKey("TWO"))
    assertFalse(mp.containsKey("THREE"))
  }

  @Test def testValuesIsViewForRemoveWithCustomObjects(): Unit = {
    val mp = factory.empty[TestObj, TestObj]
    mp.put(TestObj(1), TestObj(11))
    mp.put(TestObj(2), TestObj(22))
    val values = mp.values()

    assertFalse(values.isEmpty)
    assertFalse(mp.isEmpty)

    values.clear()

    assertTrue(values.isEmpty)
    assertTrue(mp.isEmpty)

    mp.put(TestObj(1), TestObj(11))
    mp.put(TestObj(2), TestObj(22))

    assertTrue(mp.containsKey(TestObj(1)))
    values.remove(TestObj(11))
    assertFalse(mp.containsKey(TestObj(1)))

    mp.put(TestObj(1), TestObj(11))
    mp.put(TestObj(3), TestObj(33))

    assertTrue(mp.containsKey(TestObj(1)))
    assertTrue(mp.containsKey(TestObj(2)))
    assertTrue(mp.containsKey(TestObj(3)))

    values.removeAll(ju.Arrays.asList(TestObj(11), TestObj(22)))

    assertFalse(mp.containsKey(TestObj(1)))
    assertFalse(mp.containsKey(TestObj(2)))
    assertTrue(mp.containsKey(TestObj(3)))

    mp.put(TestObj(1), TestObj(11))
    mp.put(TestObj(2), TestObj(22))
    mp.put(TestObj(3), TestObj(33))

    assertTrue(mp.containsKey(TestObj(1)))
    assertTrue(mp.containsKey(TestObj(2)))
    assertTrue(mp.containsKey(TestObj(3)))

    values.retainAll(ju.Arrays.asList(TestObj(11), TestObj(22)))

    assertTrue(mp.containsKey(TestObj(1)))
    assertTrue(mp.containsKey(TestObj(2)))
    assertFalse(mp.containsKey(TestObj(3)))
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
      expectThrows(classOf[NullPointerException], keySet.contains(null))

    mp.put("THREE", "three")

    assertTrue(keySet.contains("THREE"))

    val coll1 = ju.Arrays.asList("ONE", "TWO", "THREE")
    assertTrue(keySet.containsAll(coll1))

    val coll2 = ju.Arrays.asList("ONE", "TWO", "THREE", "FOUR")
    assertFalse(keySet.containsAll(coll2))

    if (factory.allowsNullKeysQueries) {
      val coll3 = ju.Arrays.asList("ONE", "TWO", "THREE", null)
      assertFalse(keySet.containsAll(coll3))
    }
  }

  @Test def testKeySetIsViewForQueriesWithCustomObjects(): Unit = {
    val mp = factory.empty[TestObj, TestObj]
    mp.put(TestObj(1), TestObj(11))
    mp.put(TestObj(2), TestObj(22))
    val keySet = mp.keySet()

    assertTrue(keySet.contains(TestObj(1)))
    assertTrue(keySet.contains(TestObj(2)))
    assertFalse(keySet.contains(TestObj(3)))
    if (factory.allowsNullKeysQueries)
      assertFalse(keySet.contains(null))
    else
      expectThrows(classOf[NullPointerException], keySet.contains(null))

    mp.put(TestObj(3), TestObj(33))

    assertTrue(keySet.contains(TestObj(3)))

    val coll1 = ju.Arrays.asList(TestObj(1), TestObj(2), TestObj(3))
    assertTrue(keySet.containsAll(coll1))

    val coll2 = ju.Arrays.asList(TestObj(1), TestObj(2), TestObj(4))
    assertFalse(keySet.containsAll(coll2))

    if (factory.allowsNullKeysQueries) {
      val coll3 = ju.Arrays.asList(TestObj(1), TestObj(2), null)
      assertFalse(keySet.containsAll(coll3))
    }
  }

  @Test def testKeySetIsViewForQueriesWithDoublesCornerCaseOfEquals(): Unit = {
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

    keySet.removeAll(ju.Arrays.asList("ONE", "TWO", "FIVE"))

    assertFalse(mp.containsKey("ONE"))
    assertFalse(mp.containsKey("TWO"))
    assertTrue(mp.containsKey("THREE"))

    mp.put("ONE", "one")
    mp.put("TWO", "two")
    mp.put("THREE", "three")

    assertTrue(mp.containsKey("ONE"))
    assertTrue(mp.containsKey("TWO"))
    assertTrue(mp.containsKey("THREE"))

    keySet.retainAll(ju.Arrays.asList("ONE", "TWO", "FIVE"))

    assertTrue(mp.containsKey("ONE"))
    assertTrue(mp.containsKey("TWO"))
    assertFalse(mp.containsKey("THREE"))
  }

  @Test def testKeySetIsViewForRemoveWithCustomObjects(): Unit = {
    val mp = factory.empty[TestObj, TestObj]
    mp.put(TestObj(1), TestObj(11))
    mp.put(TestObj(2), TestObj(22))
    val keySet = mp.keySet()

    assertFalse(keySet.isEmpty)
    assertFalse(mp.isEmpty)

    keySet.clear()

    assertTrue(keySet.isEmpty)

    assertTrue(mp.isEmpty)

    mp.put(TestObj(1), TestObj(11))
    mp.put(TestObj(2), TestObj(22))

    assertTrue(mp.containsKey(TestObj(1)))
    keySet.remove(TestObj(1))
    assertFalse(mp.containsKey(TestObj(1)))

    mp.put(TestObj(1), TestObj(11))
    mp.put(TestObj(3), TestObj(33))

    assertTrue(mp.containsKey(TestObj(1)))
    assertTrue(mp.containsKey(TestObj(2)))
    assertTrue(mp.containsKey(TestObj(3)))

    keySet.removeAll(ju.Arrays.asList(TestObj(1), TestObj(2), TestObj(5)))

    assertFalse(mp.containsKey(TestObj(1)))
    assertFalse(mp.containsKey(TestObj(2)))
    assertTrue(mp.containsKey(TestObj(3)))

    mp.put(TestObj(1), TestObj(11))
    mp.put(TestObj(2), TestObj(22))
    mp.put(TestObj(3), TestObj(33))

    assertTrue(mp.containsKey(TestObj(1)))
    assertTrue(mp.containsKey(TestObj(2)))
    assertTrue(mp.containsKey(TestObj(3)))

    keySet.retainAll(ju.Arrays.asList(TestObj(1), TestObj(2), TestObj(5)))

    assertTrue(mp.containsKey(TestObj(1)))
    assertTrue(mp.containsKey(TestObj(2)))
    assertFalse(mp.containsKey(TestObj(3)))
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
    val entrySet = mp.entrySet()

    assertTrue(entrySet.contains(SIE("ONE", "one")))
    assertTrue(entrySet.contains(SIE("TWO", "two")))
    assertFalse(entrySet.contains(SIE("THREE", "three")))
    assertFalse(entrySet.contains(SIE("ONE", "two")))
    assertFalse(entrySet.contains(SIE("THREE", "one")))

    mp.put("THREE", "three")

    assertTrue(entrySet.contains(SIE("THREE", "three")))

    val coll1 = ju.Arrays.asList(SIE("ONE", "one"), SIE("TWO", "two"),
        SIE("THREE", "three"))
    assertTrue(entrySet.containsAll(coll1))

    val coll2 = ju.Arrays.asList(SIE("ONE", "one"), SIE("TWO", "two"),
        SIE("THREE", "three"), SIE("FOUR", "four"))
    assertFalse(entrySet.containsAll(coll2))

    val coll3 = ju.Arrays.asList(SIE("ONE", "one"), SIE("TWO", "four"),
        SIE("THREE", "three"))
    assertFalse(entrySet.containsAll(coll3))

    val coll4 = ju.Arrays.asList(SIE("ONE", "one"), SIE("four", "two"),
        SIE("THREE", "three"))
    assertFalse(entrySet.containsAll(coll4))
  }

  @Test def testEntrySetIsViewForQueriesWithCustomObjects(): Unit = {
    val mp = factory.empty[TestObj, TestObj]
    mp.put(TestObj(1), TestObj(11))
    mp.put(TestObj(2), TestObj(22))
    val entrySet = mp.entrySet()

    assertTrue(entrySet.contains(SIE(TestObj(1), TestObj(11))))
    assertTrue(entrySet.contains(SIE(TestObj(2), TestObj(22))))
    assertFalse(entrySet.contains(SIE(TestObj(3), TestObj(33))))
    assertFalse(entrySet.contains(SIE(TestObj(1), TestObj(22))))
    assertFalse(entrySet.contains(SIE(TestObj(3), TestObj(11))))

    mp.put(TestObj(3), TestObj(33))

    assertTrue(entrySet.contains(SIE(TestObj(3), TestObj(33))))

    val coll1 = ju.Arrays.asList(SIE(TestObj(1), TestObj(11)),
        SIE(TestObj(2), TestObj(22)), SIE(TestObj(3), TestObj(33)))
    assertTrue(entrySet.containsAll(coll1))

    val coll2 = ju.Arrays.asList(SIE(TestObj(1), TestObj(11)),
        SIE(TestObj(2), TestObj(22)), SIE(TestObj(3), TestObj(33)),
        SIE(TestObj(4), TestObj(44)))
    assertFalse(entrySet.containsAll(coll2))

    val coll3 = ju.Arrays.asList(SIE(TestObj(1), TestObj(11)),
        SIE(TestObj(2), TestObj(44)), SIE(TestObj(3), TestObj(33)))
    assertFalse(entrySet.containsAll(coll3))

    val coll4 = ju.Arrays.asList(SIE(TestObj(1), TestObj(11)),
        SIE(TestObj(4), TestObj(22)), SIE(TestObj(3), TestObj(33)))
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

    assertTrue(mp.containsKey("ONE"))
    assertTrue(entrySet.remove(SIE("ONE", "one")))
    assertFalse(entrySet.remove(SIE("TWO", "four")))
    assertFalse(entrySet.remove("TWO"))
    assertFalse(mp.containsKey("ONE"))
    assertTrue(mp.containsKey("TWO"))

    mp.put("ONE", "one")
    mp.put("THREE", "three")

    assertTrue(mp.containsKey("ONE"))
    assertTrue(mp.containsKey("TWO"))
    assertTrue(mp.containsKey("THREE"))

    entrySet.removeAll(ju.Arrays.asList(SIE("ONE", "one"), SIE("TWO", "two"),
        SIE("THREE", "four"), "THREE", 42))

    assertFalse(mp.containsKey("ONE"))
    assertFalse(mp.containsKey("TWO"))
    assertTrue(mp.containsKey("THREE"))

    mp.put("ONE", "one")
    mp.put("TWO", "two")
    mp.put("THREE", "three")

    assertTrue(mp.containsKey("ONE"))
    assertTrue(mp.containsKey("TWO"))
    assertTrue(mp.containsKey("THREE"))

    entrySet.retainAll(ju.Arrays.asList(SIE("ONE", "one"), SIE("TWO", "two"),
        SIE("THREE", "four"), "THREE", 42))

    assertTrue(mp.containsKey("ONE"))
    assertTrue(mp.containsKey("TWO"))
    assertFalse(mp.containsKey("THREE"))
  }

  @Test def testEntrySetIsViewForRemoveWithCustomObjects(): Unit = {
    val mp = factory.empty[TestObj, TestObj]
    mp.put(TestObj(1), TestObj(11))
    mp.put(TestObj(2), TestObj(22))
    val entrySet = mp.entrySet()

    assertFalse(entrySet.isEmpty)
    assertFalse(mp.isEmpty)

    entrySet.clear()
    assertTrue(entrySet.isEmpty)
    assertTrue(mp.isEmpty)

    mp.put(TestObj(1), TestObj(11))
    mp.put(TestObj(2), TestObj(22))

    assertTrue(mp.containsKey(TestObj(1)))
    assertTrue(entrySet.remove(SIE(TestObj(1), TestObj(11))))
    assertFalse(entrySet.remove(SIE(TestObj(2), TestObj(44))))
    assertFalse(entrySet.remove(TestObj(2)))
    assertFalse(mp.containsKey(TestObj(1)))
    assertTrue(mp.containsKey(TestObj(2)))

    mp.put(TestObj(1), TestObj(11))
    mp.put(TestObj(3), TestObj(33))

    assertTrue(mp.containsKey(TestObj(1)))
    assertTrue(mp.containsKey(TestObj(2)))
    assertTrue(mp.containsKey(TestObj(3)))

    entrySet.removeAll(ju.Arrays.asList(SIE(TestObj(1), TestObj(11)),
        SIE(TestObj(2), TestObj(22)), SIE(TestObj(3), TestObj(44)),
        TestObj(3), 42))

    assertFalse(mp.containsKey(TestObj(1)))
    assertFalse(mp.containsKey(TestObj(2)))
    assertTrue(mp.containsKey(TestObj(3)))

    mp.put(TestObj(1), TestObj(11))
    mp.put(TestObj(2), TestObj(22))
    mp.put(TestObj(3), TestObj(33))

    assertTrue(mp.containsKey(TestObj(1)))
    assertTrue(mp.containsKey(TestObj(2)))
    assertTrue(mp.containsKey(TestObj(3)))

    entrySet.retainAll(ju.Arrays.asList(SIE(TestObj(1), TestObj(11)),
        SIE(TestObj(2), TestObj(22)), SIE(TestObj(3), TestObj(44)),
        TestObj(3), 42))

    assertTrue(mp.containsKey(TestObj(1)))
    assertTrue(mp.containsKey(TestObj(2)))
    assertFalse(mp.containsKey(TestObj(3)))
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
    mp.put(TestObj(1), TestObj(11))
    mp.put(TestObj(2), TestObj(22))
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

}

object MapTest {
  final case class TestObj(num: Int)

  def SIE[K, V](key: K, value: V): ju.Map.Entry[K, V] =
    new ju.AbstractMap.SimpleImmutableEntry(key, value)
}

trait MapFactory {
  def implementationName: String

  def empty[K: ClassTag, V: ClassTag]: ju.Map[K, V]

  def allowsNullKeys: Boolean

  def allowsNullValues: Boolean

  def allowsNullKeysQueries: Boolean = true

  def allowsNullValuesQueries: Boolean = true

  def withSizeLimit: Option[Int] = None
}
