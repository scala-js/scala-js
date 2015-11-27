package org.scalajs.testsuite.utils

import java.{lang => jl, util => ju}

import org.scalajs.testsuite.utils.AssertThrows._

import scala.collection.JavaConversions._

trait CollectionsTestBase {

  val range: Range = 0 to 30

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

  def testCollectionImmutability[E](coll: ju.Collection[E], elem: E): Unit = {
    expectThrows(classOf[UnsupportedOperationException], coll.add(elem))
    expectThrows(classOf[UnsupportedOperationException],
        coll.addAll(Seq.empty[E]))
    expectThrows(classOf[UnsupportedOperationException], coll.clear())
    expectThrows(classOf[UnsupportedOperationException], coll.remove(elem))
    expectThrows(classOf[UnsupportedOperationException],
        coll.removeAll(Seq.empty[E]))
    expectThrows(classOf[UnsupportedOperationException],
        coll.retainAll(Seq.empty[E]))
    testIteratorsImmutability(() => coll.iterator())
  }

  def testSetImmutability[E](set: ju.Set[E], elem: E): Unit =
    testCollectionImmutability(set, elem)

  def testSortedSetImmutability[E](set: ju.SortedSet[E], elem: E,
      recursive: Boolean = false): Unit = {
    testSetImmutability(set, elem)
    def testSubsets(ss: ju.SortedSet[E]) = {
      if (recursive) testSetImmutability(ss, elem)
      else testSortedSetImmutability(ss, elem, true)
    }
    testSubsets(set.headSet(elem))
    testSubsets(set.tailSet(elem))
    testSubsets(set.subSet(elem, elem))
  }

  def testListImmutability[E](list: ju.List[E], elem: E,
      recursive: Boolean = false): Unit = {
    testCollectionImmutability(list, elem)
    expectThrows(classOf[UnsupportedOperationException], list.add(0, elem))
    expectThrows(classOf[UnsupportedOperationException],
        list.addAll(0, Seq.empty[E]))
    expectThrows(classOf[UnsupportedOperationException], list.remove(0))
    expectThrows(classOf[UnsupportedOperationException], list.set(0, elem))
    def testSublist(sl: ju.List[E]): Unit = {
      if (recursive) testCollectionImmutability(sl, elem)
      else testListImmutability(sl, elem, true)
    }
    testSublist(list.subList(0, list.size / 2))
    testListIteratorsImmutability(() => list.listIterator(), elem)
    testListIteratorsImmutability(() => list.listIterator(0), elem)
  }

  def testOnFirstPositionOfIterator[Iter <: ju.Iterator[_]](
      newIter: () => Iter, action: Iter => Unit,
      expectedException: Option[Class[_ <: Throwable]]): Unit = {
    val it = newIter()
    if (it.hasNext) {
      it.next()
      expectedException match {
        case Some(exClass) => expectThrows(exClass, action(it))
        case None => action(it)
      }
    }
  }

  def testMapImmutability[K, V](map: ju.Map[K, V], key: K, value: V): Unit = {
    expectThrows(classOf[UnsupportedOperationException], map.clear())
    expectThrows(classOf[UnsupportedOperationException], map.put(key, value))
    expectThrows(classOf[UnsupportedOperationException],
        map.putAll(Map.empty[K, V]))
    testSetImmutability(map.entrySet(),
        new ju.AbstractMap.SimpleImmutableEntry(key, value))
    testSetImmutability(map.keySet(), key)
    testCollectionImmutability(map.values(), value)
  }

  def testSortedMapImmutability[K, V](map: ju.SortedMap[K, V], key: K, value: V,
      recursive: Boolean = false): Unit = {
    testMapImmutability(map, key, value)
    def testSubmap(sm: ju.SortedMap[K, V]) = {
      if (recursive) testMapImmutability(sm, key, value)
      else testSortedMapImmutability(sm, key, value, true)
    }
    testSubmap(map.headMap(key))
    testSubmap(map.tailMap(key))
    testSubmap(map.subMap(key, key))
  }

  def testIteratorsImmutability[E](newIter: () => ju.Iterator[E]): Unit = {
    testOnFirstPositionOfIterator[ju.Iterator[E]](newIter, _.remove(),
        Some(classOf[UnsupportedOperationException]))
  }

  def testListIteratorsImmutability[E](newIter: () => ju.ListIterator[E],
      elem: E): Unit = {
    testIteratorsImmutability(newIter)
    testOnFirstPositionOfIterator[ju.ListIterator[E]](newIter, _.add(elem),
        Some(classOf[UnsupportedOperationException]))
    testOnFirstPositionOfIterator[ju.ListIterator[E]](newIter, _.set(elem),
        Some(classOf[UnsupportedOperationException]))
  }
}
