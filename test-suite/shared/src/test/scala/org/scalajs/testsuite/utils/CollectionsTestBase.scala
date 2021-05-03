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

package org.scalajs.testsuite.utils

import java.{lang => jl, util => ju}

import org.scalajs.testsuite.utils.AssertThrows._

import org.scalajs.testsuite.javalib.util.TrivialImmutableCollection
import org.scalajs.testsuite.javalib.util.TrivialImmutableMap

trait CollectionsTestBase {

  val range: Range = 0 to 30

  def rangeOfElems[A](toElem: Int => A): TrivialImmutableCollection[A] =
    TrivialImmutableCollection(range.map(toElem): _*)

  class A extends jl.Comparable[A] {
    def compareTo(o: A): Int = this.##.compareTo(o.##)
  }

  class B extends A

  class C extends B

  class CustomComparable(val v: Int) extends jl.Comparable[CustomComparable] {
    override def compareTo(o: CustomComparable): Int =
      (v % 8).compareTo(o.v % 8)

    override def toString(): String =
      s"CustomComparable($v)"
  }

  def testCollectionUnmodifiability[E](coll: ju.Collection[E], elem: E): Unit = {
    val empty = TrivialImmutableCollection[E]()
    assertThrows(classOf[UnsupportedOperationException], coll.add(elem))
    assertThrows(classOf[UnsupportedOperationException], coll.addAll(empty))
    assertThrows(classOf[UnsupportedOperationException], coll.clear())
    assertThrows(classOf[UnsupportedOperationException], coll.remove(elem))
    assertThrows(classOf[UnsupportedOperationException], coll.removeAll(empty))
    assertThrows(classOf[UnsupportedOperationException], coll.retainAll(empty))
    testIteratorsUnmodifiability(() => coll.iterator())
  }

  def testSetUnmodifiability[E](set: ju.Set[E], elem: E): Unit =
    testCollectionUnmodifiability(set, elem)

  def testSortedSetUnmodifiability[E](set: ju.SortedSet[E], elem: E,
      recursive: Boolean = false): Unit = {
    testSetUnmodifiability(set, elem)
    def testSubsets(ss: ju.SortedSet[E]) = {
      if (recursive) testSetUnmodifiability(ss, elem)
      else testSortedSetUnmodifiability(ss, elem, true)
    }
    testSubsets(set.headSet(elem))
    testSubsets(set.tailSet(elem))
    testSubsets(set.subSet(elem, elem))
  }

  def testListUnmodifiability[E](list: ju.List[E], elem: E,
      recursive: Boolean = false): Unit = {
    testCollectionUnmodifiability(list, elem)
    assertThrows(classOf[UnsupportedOperationException], list.add(0, elem))
    assertThrows(classOf[UnsupportedOperationException],
        list.addAll(0, TrivialImmutableCollection[E]()))
    assertThrows(classOf[UnsupportedOperationException], list.remove(0))
    assertThrows(classOf[UnsupportedOperationException], list.set(0, elem))
    def testSublist(sl: ju.List[E]): Unit = {
      if (recursive) testCollectionUnmodifiability(sl, elem)
      else testListUnmodifiability(sl, elem, true)
    }
    testSublist(list.subList(0, list.size / 2))
    testListIteratorsUnmodifiability(() => list.listIterator(), elem)
    testListIteratorsUnmodifiability(() => list.listIterator(0), elem)
  }

  def testOnFirstPositionOfIterator[Iter <: ju.Iterator[_]](
      newIter: () => Iter, action: Iter => Unit,
      expectedException: Option[Class[_ <: Throwable]]): Unit = {
    val it = newIter()
    if (it.hasNext) {
      it.next()
      expectedException match {
        case Some(exClass) => assertThrows(exClass, action(it))
        case None => action(it)
      }
    }
  }

  def testMapUnmodifiability[K, V](map: ju.Map[K, V], key: K, value: V): Unit = {
    assertThrows(classOf[UnsupportedOperationException], map.clear())
    assertThrows(classOf[UnsupportedOperationException], map.put(key, value))
    assertThrows(classOf[UnsupportedOperationException],
        map.putAll(TrivialImmutableMap[K, V]()))
    testSetUnmodifiability(map.entrySet(),
        new ju.AbstractMap.SimpleImmutableEntry(key, value))
    testSetUnmodifiability(map.keySet(), key)
    testCollectionUnmodifiability(map.values(), value)
  }

  def testSortedMapUnmodifiability[K, V](map: ju.SortedMap[K, V], key: K, value: V,
      recursive: Boolean = false): Unit = {
    testMapUnmodifiability(map, key, value)
    def testSubmap(sm: ju.SortedMap[K, V]) = {
      if (recursive) testMapUnmodifiability(sm, key, value)
      else testSortedMapUnmodifiability(sm, key, value, true)
    }
    testSubmap(map.headMap(key))
    testSubmap(map.tailMap(key))
    testSubmap(map.subMap(key, key))
  }

  def testIteratorsUnmodifiability[E](newIter: () => ju.Iterator[E]): Unit = {
    testOnFirstPositionOfIterator[ju.Iterator[E]](newIter, _.remove(),
        Some(classOf[UnsupportedOperationException]))
  }

  def testListIteratorsUnmodifiability[E](newIter: () => ju.ListIterator[E],
      elem: E): Unit = {
    testIteratorsUnmodifiability(newIter)
    testOnFirstPositionOfIterator[ju.ListIterator[E]](newIter, _.add(elem),
        Some(classOf[UnsupportedOperationException]))
    testOnFirstPositionOfIterator[ju.ListIterator[E]](newIter, _.set(elem),
        Some(classOf[UnsupportedOperationException]))
  }
}
