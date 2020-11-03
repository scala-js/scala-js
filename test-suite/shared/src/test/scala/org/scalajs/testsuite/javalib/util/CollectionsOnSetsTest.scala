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

import java.{util => ju, lang => jl}

import org.junit.Test

import scala.reflect.ClassTag

trait CollectionsOnSetsTest extends CollectionsOnCollectionsTest {
  def factory: SetFactory

  @Test def unmodifiableSet(): Unit = {
    def test[E: ClassTag](toElem: Int => E): Unit = {
      val set = factory.empty[E]
      testSetUnmodifiability(ju.Collections.unmodifiableSet(set), toElem(0))
      set.addAll(rangeOfElems(toElem))
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

  @Test def unmodifiableSortedSet(): Unit = {
    def test[E: ClassTag](toElem: Int => E): Unit = {
      val sortedSet = factory.empty[E]
      testSortedSetUnmodifiability(ju.Collections.unmodifiableSortedSet(sortedSet), toElem(0))
      sortedSet.addAll(rangeOfElems(toElem))
      testSortedSetUnmodifiability(ju.Collections.unmodifiableSortedSet(sortedSet), toElem(0))
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

class CollectionsOnConcurrentSkipListSetFactoryTest extends CollectionsOnSetsTest {
  def factory: SetFactory = new concurrent.ConcurrentSkipListSetFactory
}
