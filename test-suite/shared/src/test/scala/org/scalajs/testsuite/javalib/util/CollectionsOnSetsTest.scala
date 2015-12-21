package org.scalajs.testsuite.javalib.util

import java.{util => ju}

import org.junit.Test

import scala.collection.JavaConversions._

trait CollectionsOnSetsTest extends CollectionsOnCollectionsTest {
  def factory: SetFactory

  @Test def unmodifiableSet():Unit = {
    def test[E](toElem: Int => E): Unit = {
      val set = factory.empty[E]
      testSetImmutability(ju.Collections.unmodifiableSet(set), toElem(0))
      set.addAll(range.map(toElem))
      testSetImmutability(ju.Collections.unmodifiableSet(set), toElem(0))
    }

    test[Int](_.toInt)
    test[Long](_.toLong)
    test[Double](_.toDouble)
    test[String](_.toString)
  }
}

trait CollectionsOnSortedSetsTest extends CollectionsOnSetsTest {
  def factory: SortedSetFactory

  @Test def unmodifiableSortedSet():Unit = {
    def test[E](toElem: Int => E): Unit = {
      val sortedSet = factory.empty[E]
      testSortedSetImmutability(ju.Collections.unmodifiableSortedSet(sortedSet),
        toElem(0))
      sortedSet.addAll(range.map(toElem))
      testSortedSetImmutability(ju.Collections.unmodifiableSortedSet(sortedSet),
        toElem(0))
    }

    test[Int](_.toInt)
    test[Long](_.toLong)
    test[Double](_.toDouble)
    test[String](_.toString)
  }
}

class CollectionsOnHashSetFactoryTest extends CollectionsOnSetsTest {
  def factory: SetFactory = new HashSetFactory
}

class CollectionsOnLinkedHashSetFactoryTest extends CollectionsOnSetsTest {
  def factory: SetFactory = new LinkedHashSetFactory
}

class CollectionsOnConcurrentSkipListSetFactoryTest
    extends CollectionsOnSetsTest {
  def factory: SetFactory = new concurrent.ConcurrentSkipListSetFactory
}
