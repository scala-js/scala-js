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
import scala.reflect.ClassTag
import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.Platform._

import Utils._

class IdentityHashMapTest extends MapTest {
  import MapTest._

  override def factory: IdentityHashMapFactory = new IdentityHashMapFactory

  private val testObjCache = new ju.HashMap[Int, TestObj]()

  override def testObj(i: Int): TestObj = {
    val existing = testObjCache.get(i)
    if (existing ne null) {
      existing
    } else {
      val newTestObj = TestObj(i)
      testObjCache.put(i, newTestObj)
      newTestObj
    }
  }

  // Tests that check by value rather than identity (false negative tests)
  // Similar to tests already in MapTest (copied and modified)

  @Test def testIdentityMapWithCustomObjects(): Unit = {
    val mp = factory.empty[TestObj, TestObj]
    val key100 = TestObj(100)
    val val12345 = TestObj(12345)
    mp.put(key100, val12345)
    mp.put(testObj(100), testObj(12345))

    assertEquals(2, mp.size())
    assertTrue(testObj(12345) eq mp.get(testObj(100)))
    assertTrue(val12345 eq mp.get(key100))
    assertFalse(testObj(12345) eq mp.get(key100))
    assertFalse(val12345 eq mp.get(testObj(100)))
    assertTrue(mp.containsKey(testObj(100)))
    assertTrue(mp.containsKey(key100))
    assertTrue(mp.containsValue(testObj(12345)))
    assertTrue(mp.containsValue(val12345))

    val rem1 = mp.remove(testObj(100))
    assertTrue(rem1 eq testObj(12345))
    assertFalse(rem1 eq val12345)
    assertTrue(mp.containsKey(key100))
    assertFalse(mp.containsKey(testObj(100)))
    assertTrue(mp.containsValue(val12345))
    assertFalse(mp.containsValue(testObj(12345)))

    val rem2 = mp.remove(key100)
    assertFalse(rem2 eq testObj(12345))
    assertTrue(rem2 eq val12345)
    assertFalse(mp.containsKey(key100))
    assertFalse(mp.containsKey(testObj(100)))
    assertFalse(mp.containsValue(val12345))
    assertFalse(mp.containsValue(testObj(12345)))
    assertTrue(mp.isEmpty())
  }

  @Test def testIdentityEntrySetIsViewForQueriesWithCustomObjects(): Unit = {
    val mp = factory.empty[TestObj, TestObj]
    mp.put(testObj(1), testObj(11))
    mp.put(testObj(2), testObj(22))
    mp.put(testObj(5), null)
    mp.put(null, testObj(55))
    val entrySet = mp.entrySet()

    assertTrue(entrySet.contains(SIE(testObj(1), testObj(11))))
    assertFalse(entrySet.contains(SIE(testObj(1), TestObj(11))))
    assertFalse(entrySet.contains(SIE(TestObj(1), testObj(11))))

    assertTrue(entrySet.contains(SIE(testObj(5), null)))
    assertFalse(entrySet.contains(SIE(TestObj(5), null)))
    assertFalse(entrySet.contains(SIE(testObj(6), null)))

    assertTrue(entrySet.contains(SIE(null, testObj(55))))
    assertFalse(entrySet.contains(SIE(null, TestObj(55))))

    mp.put(testObj(3), testObj(33))

    // containsAll calls contains tested above

    val coll1 = TrivialImmutableCollection(SIE(testObj(1), testObj(11)),
        SIE(testObj(2), testObj(22)), SIE(testObj(3), testObj(33)))
    val coll1a = TrivialImmutableCollection(SIE(testObj(1), TestObj(11)),
        SIE(testObj(2), testObj(22)), SIE(testObj(3), testObj(33)))
    assertTrue(entrySet.containsAll(coll1))
    assertFalse(entrySet.containsAll(coll1a))
  }

  @Test def testIdentityEntrySetIsViewForRemoveWithCustomObjects(): Unit = {
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
    mp.put(null, testObj(55))
    mp.put(testObj(5), null)

    assertTrue(mp.containsKey(testObj(1)))
    assertFalse(mp.containsKey(TestObj(1)))
    assertTrue(entrySet.remove(SIE(testObj(1), testObj(11))))
    assertFalse(entrySet.remove(SIE(testObj(2), TestObj(22))))
    assertFalse(entrySet.remove(SIE(testObj(2), testObj(44))))
    assertFalse(entrySet.remove(SIE(TestObj(5), null)))
    assertTrue(entrySet.remove(SIE(testObj(5), null)))

    mp.put(testObj(1), testObj(11))
    mp.put(testObj(3), testObj(33))

    assertTrue(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(testObj(2)))
    assertTrue(mp.containsKey(testObj(3)))

    //removeAll and retainAll include non Entry values

    entrySet.removeAll(TrivialImmutableCollection(SIE(testObj(1), TestObj(11)),
        SIE(testObj(2), testObj(22)), SIE(testObj(3), testObj(44)),
        TestObj(3), 33))

    assertTrue(mp.containsKey(testObj(1)))
    assertFalse(mp.containsKey(testObj(2)))
    assertTrue(mp.containsKey(testObj(3)))

    mp.put(testObj(2), testObj(22))

    assertTrue(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(testObj(2)))
    assertTrue(mp.containsKey(testObj(3)))

    entrySet.retainAll(TrivialImmutableCollection(SIE(testObj(1), testObj(11)),
        SIE(testObj(2), testObj(22)), SIE(testObj(3), TestObj(33)),
        testObj(3), 42))

    assertTrue(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(testObj(2)))
    assertFalse(mp.containsKey(testObj(3)))

    val key1 = TestObj(1)
    val val11 = TestObj(11)
    mp.put(key1, val11)

    assertTrue(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(key1))
    entrySet.remove(SIE(testObj(1), testObj(11)))
    assertFalse(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(key1))
    entrySet.remove(SIE(key1, TestObj(11)))
    assertFalse(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(key1))
    entrySet.remove(SIE(key1, val11))
    assertFalse(mp.containsKey(testObj(1)))
    assertFalse(mp.containsKey(key1))
  }

  @Test def testIdentityKeySetIsViewForQueriesWithCustomObjects(): Unit = {
    val mp = factory.empty[TestObj, TestObj]
    mp.put(testObj(1), TestObj(11))
    mp.put(testObj(2), TestObj(22))
    val keySet = mp.keySet()

    assertTrue(keySet.contains(testObj(1)))
    assertFalse(keySet.contains(TestObj(1)))
    assertTrue(keySet.contains(testObj(2)))
    assertFalse(keySet.contains(TestObj(2)))
    assertFalse(keySet.contains(testObj(3)))

    mp.put(testObj(3), TestObj(33))

    assertTrue(keySet.contains(testObj(3)))
    assertFalse(keySet.contains(TestObj(3)))

    val coll1 = TrivialImmutableCollection(testObj(1), testObj(2), testObj(3))
    val coll1a = TrivialImmutableCollection(testObj(1), testObj(2), TestObj(3))
    assertTrue(keySet.containsAll(coll1))
    assertFalse(keySet.containsAll(coll1a))
  }

  @Test def testIdentityKeySetIsViewForRemoveWithCustomObjects(): Unit = {
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
    assertFalse(mp.containsKey(TestObj(3)))

    val coll1 = TrivialImmutableCollection(
        testObj(1), TestObj(2), testObj(3), testObj(5))
    keySet.removeAll(coll1)

    assertFalse(mp.containsKey(testObj(1)))
    if (!executingInJVM)
      assertTrue(mp.containsKey(testObj(2)))
    assertFalse(mp.containsKey(testObj(3)))

    mp.put(testObj(1), TestObj(11))
    mp.put(testObj(2), TestObj(22))
    mp.put(testObj(3), TestObj(33))

    assertTrue(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(testObj(2)))
    assertTrue(mp.containsKey(testObj(3)))

    val coll2 = TrivialImmutableCollection(testObj(1), testObj(2), TestObj(3))
    keySet.retainAll(coll2)

    assertTrue(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(testObj(2)))
    if (!executingInJVM)
      assertFalse(mp.containsKey(testObj(3)))
    assertFalse(mp.containsKey(TestObj(3)))

    mp.put(null, TestObj(111))
    assertTrue(mp.containsKey(null))
    assertTrue(keySet.contains(null))
    assertTrue(keySet.remove(null))
    assertFalse(mp.containsKey(null))

    mp.put(testObj(4), null)
    assertTrue(mp.containsKey(testObj(4)))
    assertFalse(mp.containsKey(TestObj(4)))
    assertTrue(keySet.contains(testObj(4)))
    assertFalse(keySet.contains(TestObj(4)))
    assertFalse(keySet.remove(TestObj(4)))
    assertFalse(mp.containsKey(TestObj(4)))
    assertTrue(mp.containsKey(testObj(4)))
    assertTrue(keySet.contains(testObj(4)))
    assertTrue(keySet.remove(testObj(4)))
    assertFalse(mp.containsKey(testObj(4)))
    assertFalse(keySet.contains(testObj(4)))

    val key1 = TestObj(1)
    val val11 = TestObj(11)
    mp.put(key1, val11)
    mp.put(testObj(1), TestObj(11))

    assertTrue(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(key1))
    keySet.remove(testObj(1))
    assertFalse(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(key1))
    keySet.remove(key1)
    assertFalse(mp.containsKey(testObj(1)))
    assertFalse(mp.containsKey(key1))
  }

  @Test def testIdentityValuesIsViewForQueriesWithCustomObjects(): Unit = {
    val mp = factory.empty[TestObj, TestObj]
    mp.put(testObj(1), testObj(11))
    mp.put(testObj(2), testObj(22))
    val values = mp.values()

    assertTrue(values.contains(testObj(11)))
    assertFalse(values.contains(TestObj(11)))
    assertTrue(values.contains(testObj(22)))
    assertFalse(values.contains(testObj(33)))

    mp.put(testObj(3), testObj(33))

    assertTrue(values.contains(testObj(33)))

    val coll1 = TrivialImmutableCollection(testObj(11), TestObj(22), testObj(33))
    assertFalse(values.containsAll(coll1))

    val coll2 = TrivialImmutableCollection(
        testObj(11), testObj(22), testObj(33), testObj(44))
    assertFalse(values.containsAll(coll2))
  }

  @Test def testIdentityValuesIsViewForRemoveWithCustomObjects(): Unit = {
    val mp = factory.empty[TestObj, TestObj]
    mp.put(testObj(1), testObj(11))
    mp.put(testObj(2), testObj(22))
    mp.put(testObj(3), testObj(33))
    val values = mp.values()

    assertTrue(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(testObj(2)))
    assertTrue(mp.containsKey(testObj(3)))

    assertTrue(values.remove(testObj(11)))
    assertTrue(values.remove(testObj(22)))
    assertFalse(values.remove(TestObj(33)))

    assertFalse(mp.containsKey(testObj(1)))
    assertFalse(mp.containsKey(testObj(2)))
    assertTrue(mp.containsKey(testObj(3)))

    mp.put(testObj(1), testObj(11))
    mp.put(testObj(2), testObj(22))
    mp.put(testObj(3), testObj(33))

    val coll1 = TrivialImmutableCollection(testObj(11), testObj(22), TestObj(33))
    values.removeAll(coll1)

    assertFalse(mp.containsKey(testObj(1)))
    assertFalse(mp.containsKey(testObj(2)))
    if (!executingInJVM)
      assertTrue(mp.containsKey(testObj(3)))

    mp.put(testObj(1), testObj(11))
    mp.put(testObj(2), testObj(22))
    mp.put(testObj(3), testObj(33))

    assertTrue(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(testObj(2)))
    assertTrue(mp.containsKey(testObj(3)))

    values.retainAll(TrivialImmutableCollection(
        testObj(11), testObj(22), TestObj(33)))

    assertTrue(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(testObj(2)))
    if (!executingInJVM)
      assertFalse(mp.containsKey(testObj(3)))

    val key1 = TestObj(1)
    val val11 = TestObj(11)
    mp.put(key1, val11)

    assertTrue(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(key1))
    values.remove(testObj(11))
    assertFalse(mp.containsKey(testObj(1)))
    assertTrue(mp.containsKey(key1))
    values.remove(val11)
    assertFalse(mp.containsKey(testObj(1)))
    assertFalse(mp.containsKey(key1))
  }
}

class IdentityHashMapFactory extends MapFactory {
  override def implementationName: String =
    "java.util.IdentityHashMap"

  override def empty[K: ClassTag, V: ClassTag]: ju.IdentityHashMap[K, V] =
    new ju.IdentityHashMap[K, V]

  def allowsNullKeys: Boolean = true
  def allowsNullValues: Boolean = true
  override def isIdentityBased: Boolean = true

}
