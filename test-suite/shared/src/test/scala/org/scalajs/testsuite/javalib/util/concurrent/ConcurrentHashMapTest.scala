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

package org.scalajs.testsuite.javalib.util.concurrent

import scala.collection.mutable
import scala.reflect.ClassTag

import java.{util => ju}
import java.util.concurrent.{ConcurrentHashMap => CHM}

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.javalib.util.MapTest
import org.scalajs.testsuite.utils.AssertThrows.assertThrows

class ConcurrentHashMapTest extends MapTest {

  def factory: ConcurrentHashMapFactory = new ConcurrentHashMapFactory

  @Test def testEnumeration(): Unit = {
    val chm = factory.empty[String, String]

    chm.put("ONE", "one")
    val elements = chm.elements
    assertTrue(elements.hasMoreElements)
    assertEquals("one", elements.nextElement)
    assertFalse(elements.hasMoreElements)
  }

  @Test def testIteratorsAreWeaklyConsistent(): Unit = {
    /* The Javadoc says the following about weakly consistent iterators:
     * > they are guaranteed to traverse elements as they existed upon
     * > construction exactly once, and may (but are not guaranteed to) reflect
     * > any modifications subsequent to construction.
     *
     * The two subsentences seem to be contradictory, notably in terms of
     * removal. Experimentation shows that iterators *can* reflect removals
     * subsequent to construction.
     *
     * Therefore, we interpreted that the only actual guarantees are the
     * following:
     *
     * - If a key existed when the iterator was created, and it is not removed,
     *   then eventually the iterator will yield it.
     * - An iterator never yields the same key twice.
     * - If an iterator yields a given key, then the associated value can be
     *   any value associated with the key at some point since the iterator was
     *   created (but not another, arbitrary value).
     *
     * This test aims at testing those guarantees, and only those guarantees,
     * using random schedulings of concurrent `put` and `remove` operations
     * while the iterator is used.
     */

    // Creates a new map with the state before creating the iterator
    def createInitialMap(): CHM[String, String] = {
      val m = factory.empty[String, String]
      m.put("initial", "init")
      m.put("there forever", "always")
      m.put("mutated", "first value")
      for (i <- 0 until 30)
        m.put(i.toString(), s"value $i")
      m
    }

    /* A list of operations that will randomly scheduled concurrently with the
     * iteration.
     */
    val concurrentOperations = List[CHM[String, String] => Unit](
        _.put("foo", "bar"),
        _.put("babar", "baz"),
        _.put("babar", "bazbaz"),
        _.put("hello", "world"),
        _.put("mutated", "second value"),
        _.remove("initial"),
        _.remove("hello")
    )

    // Per key, the set of values that we can possibly observe
    val possibleValuesFor: Map[String, Set[String]] = {
      Map(
          "initial" -> Set("init"),
          "there forever" -> Set("always"),
          "mutated" -> Set("first value", "second value"),
          "foo" -> Set("bar"),
          "babar" -> Set("baz", "bazbaz"),
          "hello" -> Set("world")
      ) ++ (0 until 30).map(i => i.toString() -> Set(s"value $i"))
    }

    // The set of all the values that we can possibly observe (for values())
    val allPossibleValues: Set[String] = possibleValuesFor.flatMap(_._2).toSet

    /* The list of keys that we *must* observe at some point, because they
     * exist before the iterator is created, and they are never removed.
     */
    val mandatoryKeys: List[String] =
      List("there forever", "mutated") ++ (0 until 30).map(_.toString())

    /* The list of values that we *must* observe at some point, because they
     * are uniquely associated with a key that we must observe at some point.
     */
    val mandatoryValues: List[String] =
      mandatoryKeys.map(possibleValuesFor(_)).filter(_.size == 1).map(_.head)

    // The initial seed was generated at random
    val topLevelRandom = new java.util.Random(2972838770879131323L)

    // Test 30 different interleavings for entrySet()
    for (_ <- 0 until 30) {
      val random = new scala.util.Random(topLevelRandom.nextLong())
      var shuffledOps = random.shuffle(concurrentOperations)
      val m = createInitialMap()
      val encounteredKeys = mutable.Set.empty[String]
      val entryIter = m.entrySet().iterator()

      while (entryIter.hasNext()) {
        // Schedule 0-to-many concurrent operations before the next call to next()
        while (shuffledOps.nonEmpty && random.nextBoolean()) {
          shuffledOps.head(m)
          shuffledOps = shuffledOps.tail
        }

        val entry = entryIter.next()
        val key = entry.getKey()
        val value = entry.getValue()
        assertTrue(s"duplicate iteration of key '$key'", encounteredKeys.add(key))
        assertTrue(s"unexpected key '$key'", possibleValuesFor.contains(key))
        assertTrue(s"unexpected value '$value' for key '$key'",
            possibleValuesFor(key).contains(value))
      }

      for (key <- mandatoryKeys)
        assertTrue(s"missing key '$key'", encounteredKeys.contains(key))
    }

    // Test 30 different interleavings for keySet()
    for (_ <- 0 until 30) {
      val random = new scala.util.Random(topLevelRandom.nextLong())
      var shuffledOps = random.shuffle(concurrentOperations)
      val m = createInitialMap()
      val encounteredKeys = mutable.Set.empty[String]
      val keyIter = m.keySet().iterator()

      while (keyIter.hasNext()) {
        // Schedule 0-to-many concurrent operations before the next call to next()
        while (shuffledOps.nonEmpty && random.nextBoolean()) {
          shuffledOps.head(m)
          shuffledOps = shuffledOps.tail
        }

        val key = keyIter.next()
        assertTrue(s"duplicate iteration of key '$key'", encounteredKeys.add(key))
        assertTrue(s"unexpected key '$key'", possibleValuesFor.contains(key))
      }

      for (key <- mandatoryKeys)
        assertTrue(s"missing key '$key'", encounteredKeys.contains(key))
    }

    // Test 30 different interleavings for values()
    for (_ <- 0 until 30) {
      val random = new scala.util.Random(topLevelRandom.nextLong())
      var shuffledOps = random.shuffle(concurrentOperations)
      val m = createInitialMap()
      val encounteredValues = mutable.Set.empty[String]
      val valueIter = m.values().iterator()

      while (valueIter.hasNext()) {
        // Schedule 0-to-many concurrent operations before the next call to next()
        while (shuffledOps.nonEmpty && random.nextBoolean()) {
          shuffledOps.head(m)
          shuffledOps = shuffledOps.tail
        }

        val value = valueIter.next()
        encounteredValues.add(value)
        assertTrue(s"unexpected value '$value'",
            allPossibleValues.contains(value))
      }

      for (value <- mandatoryValues)
        assertTrue(s"missing value '$value'", encounteredValues.contains(value))
    }
  }

  @Test def keySetWithNullMappedValue(): Unit = {
    val map = factory.empty[String, String]
    assertThrows(classOf[NullPointerException], map.keySet(null))
  }

  @Test def addOnKeySetView(): Unit = {
    val map = factory.empty[String, Int]
    val keySet = map.keySet(0)
    assertNull(map.get("ONE"))
    assertTrue(keySet.add("ONE"))
    assertEquals("Adding a new key adds the default value.", 0, map.get("ONE"))

    assertFalse(keySet.add("ONE"))

    assertEquals(0, map.put("ONE", 42))
    assertEquals("Putting alters the value", 42, map.get("ONE"))

    assertFalse(keySet.add("ONE"))
    assertEquals("Adding an existing key does not alter the value", 42, map.get("ONE"))
  }

  @Test def toStringOnKeySetView(): Unit = {
    val map = factory.empty[String, Int]
    val keySet = map.keySet(0)

    map.put("a", 0)
    assertEquals("[a]", keySet.toString)

    map.put("b", 1)
    val str = keySet.toString
    assertTrue(s"toString should print keys, but actual: $str", str == "[a, b]" || str == "[b, a]")
  }
}

class ConcurrentHashMapFactory extends ConcurrentMapFactory {
  def implementationName: String =
    "java.util.concurrent.ConcurrentHashMap"

  override def empty[K: ClassTag, V: ClassTag]: ju.concurrent.ConcurrentHashMap[K, V] =
    new ju.concurrent.ConcurrentHashMap[K, V]

  def allowsNullKeys: Boolean = false

  def allowsNullValues: Boolean = false
}
