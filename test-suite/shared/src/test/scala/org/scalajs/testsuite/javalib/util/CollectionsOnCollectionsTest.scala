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

import java.util.Comparator
import java.{lang => jl, util => ju}

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.CollectionsTestBase

import scala.reflect.ClassTag

trait CollectionsOnCollectionsTest extends CollectionsTestBase {

  def factory: CollectionFactory

  def testMinMax1[T <: AnyRef with Comparable[T]: ClassTag](
      factory: CollectionFactory, toElem: Int => T, isMin: Boolean): Unit = {
    val coll = factory.empty[T]
    coll.addAll(rangeOfElems(toElem))

    val minMax = if (isMin) range.head else range.last
    def getMinMax(): T =
      if (isMin) ju.Collections.min(coll)
      else ju.Collections.max(coll)

    assertEquals(0, getMinMax().compareTo(toElem(minMax)))

    coll match {
      case list: ju.List[_] =>
        ju.Collections.shuffle(list, new ju.Random(42))
        assertEquals(0, getMinMax().compareTo(toElem(minMax)))
        ju.Collections.shuffle(list, new ju.Random(100000))
        assertEquals(0, getMinMax().compareTo(toElem(minMax)))
      case _ =>
    }
  }

  def testMinMax2[T: ClassTag](factory: CollectionFactory, toElem: Int => T,
      isMin: Boolean, cmp: ju.Comparator[T]): Unit = {
    val coll = factory.empty[T]
    coll.addAll(rangeOfElems(toElem))

    val minMax = if (isMin) range.head else range.last
    def getMinMax: T =
      if (isMin) ju.Collections.min(coll, cmp)
      else ju.Collections.max(coll, cmp)

    assertEquals(0, cmp.compare(getMinMax, toElem(minMax)))

    coll match {
      case list: ju.List[_] =>
        ju.Collections.shuffle(list, new ju.Random(42))
        assertEquals(0, cmp.compare(getMinMax, toElem(minMax)))
        ju.Collections.shuffle(list, new ju.Random(100000))
        assertEquals(0, cmp.compare(getMinMax, toElem(minMax)))
      case _ =>
    }
  }

  @Test def minOnComparables(): Unit = {
    def test[T <: AnyRef with Comparable[T]: ClassTag](toElem: Int => T): Unit =
      testMinMax1(factory, toElem, true)

    test[jl.Integer](jl.Integer.valueOf)
    test[jl.Long](_.toLong)
    test[jl.Double](_.toDouble)
  }

  @Test def minWithComparator(): Unit = {
    def test[T: ClassTag](toElem: Int => T, cmp: Comparator[T]): Unit =
      testMinMax2(factory, toElem, true, cmp)

    test[jl.Integer](_.toInt, Comparator.naturalOrder[jl.Integer])
    test[jl.Long](_.toLong, Comparator.naturalOrder[jl.Long])
    test[jl.Double](_.toDouble, Comparator.naturalOrder[jl.Double])
  }

  @Test def maxOnComparables(): Unit = {
    def test[T <: AnyRef with Comparable[T]: ClassTag](toElem: Int => T): Unit =
      testMinMax1(factory, toElem, false)

    test[jl.Integer](jl.Integer.valueOf)
    test[jl.Long](_.toLong)
    test[jl.Double](_.toDouble)
  }

  @Test def maxWithComparator(): Unit = {
    def test[T: ClassTag](toElem: Int => T, cmp: Comparator[T]): Unit =
      testMinMax2(factory, toElem, false, cmp)

    test[jl.Integer](_.toInt, Comparator.naturalOrder[jl.Integer])
    test[jl.Long](_.toLong, Comparator.naturalOrder[jl.Long])
    test[jl.Double](_.toDouble, Comparator.naturalOrder[jl.Double])
  }

  @Test def frequency(): Unit = {
    def test[E: ClassTag](toElem: Int => E): Unit = {
      val coll = factory.empty[E]

      def expectAllFrequenciesToBe(n: Int): Unit = {
        for (i <- range)
          assertEquals(n, ju.Collections.frequency(coll, toElem(i)))
      }

      expectAllFrequenciesToBe(0)
      coll.addAll(rangeOfElems(toElem))
      expectAllFrequenciesToBe(1)
      coll.addAll(rangeOfElems(toElem))
      coll match {
        case _: ju.Set[_]  => expectAllFrequenciesToBe(1)
        case _: ju.List[_] => expectAllFrequenciesToBe(2)
        case _             => // Undefined behaviour
      }
    }

    test[jl.Integer](_.toInt)
    test[jl.Long](_.toLong)
    test[jl.Double](_.toDouble)
    test[String](_.toString)
  }

  @Test def addAll(): Unit = {
    def test[E: ClassTag](toElem: Int => E): Unit = {
      val coll = factory.empty[E]
      assertFalse(ju.Collections.addAll(coll))
      assertTrue(coll.isEmpty)
      assertTrue(ju.Collections.addAll(coll, toElem(0), toElem(1)))
      assertTrue(coll.contains(toElem(0)))
      assertTrue(coll.contains(toElem(1)))
    }

    test[jl.Integer](_.toInt)
    test[jl.Long](_.toLong)
    test[jl.Double](_.toDouble)
  }

  @Test def unmodifiableCollection(): Unit = {
    def test[E: ClassTag](toElem: Int => E): Unit = {
      val coll = factory.empty[E]
      testCollectionUnmodifiability(ju.Collections.unmodifiableCollection(coll),
          toElem(0))
      coll.addAll(rangeOfElems(toElem))
      testCollectionUnmodifiability(ju.Collections.unmodifiableCollection(coll),
          toElem(0))
    }

    test[jl.Integer](_.toInt)
    test[jl.Long](_.toLong)
    test[jl.Double](_.toDouble)
    test[String](_.toString)
  }
}
