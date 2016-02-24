package org.scalajs.testsuite.javalib.util

import java.{util => ju, lang => jl}

import org.junit.Test

import scala.collection.JavaConversions._
import scala.reflect.ClassTag

trait CollectionsOnSetsTest extends CollectionsOnCollectionsTest {
  def factory: SetFactory

  @Test def unmodifiableSet():Unit = {
    def test[E: ClassTag](toElem: Int => E): Unit = {
      val set = factory.empty[E]
      testSetUnmodifiability(ju.Collections.unmodifiableSet(set), toElem(0))
      set.addAll(range.map(toElem))
      testSetUnmodifiability(ju.Collections.unmodifiableSet(set), toElem(0))
    }

    test[jl.Integer](_.toInt)
    test[jl.Long](_.toLong)
    test[jl.Double](_.toDouble)
    test[String](_.toString)
  }
}

trait CollectionsOnSortedSetsTest extends CollectionsOnSetsTest {
  def factory: SortedSetFactory

  @Test def unmodifiableSortedSet():Unit = {
    def test[E: ClassTag](toElem: Int => E): Unit = {
      val sortedSet = factory.empty[E]
      testSortedSetUnmodifiability(ju.Collections.unmodifiableSortedSet(sortedSet),
        toElem(0))
      sortedSet.addAll(range.map(toElem))
      testSortedSetUnmodifiability(ju.Collections.unmodifiableSortedSet(sortedSet),
        toElem(0))
    }

    test[jl.Integer](_.toInt)
    test[jl.Long](_.toLong)
    test[jl.Double](_.toDouble)
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
