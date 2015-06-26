package org.scalajs.testsuite.javalib

import java.{lang => jl, util => ju}
import java.util.Comparator

import scala.collection.JavaConversions._

import org.scalajs.jasminetest.JasmineTest

object CollectionsTest extends JasmineTest with ListTest with SortedSetTest
    with SortedMapTest with ExpectExceptions {

  @inline
  private def range: Range = 0 to 30

  def testCollectionImmutability[E](coll: ju.Collection[E], elem: E): Unit = {
    expectThrows[UnsupportedOperationException](coll.add(elem))
    expectThrows[UnsupportedOperationException](coll.addAll(Seq.empty[E]))
    expectThrows[UnsupportedOperationException](coll.clear())
    expectThrows[UnsupportedOperationException](coll.remove(elem))
    expectThrows[UnsupportedOperationException](coll.removeAll(Seq.empty[E]))
    expectThrows[UnsupportedOperationException](coll.retainAll(Seq.empty[E]))
    testIteratorsImmutability(() => coll.iterator())
  }

  def testSetImmutability[E](set: ju.Set[E], elem: E): Unit =
    testCollectionImmutability(set, elem)

  def testSortedSetImmutability[E](set: ju.SortedSet[E], elem: E, recursive: Boolean = false): Unit = {
    testSetImmutability(set, elem)
    def testSubsets(ss: ju.SortedSet[E]) = {
      if (recursive) testSetImmutability(ss, elem)
      else testSortedSetImmutability(ss, elem, true)
    }
    testSubsets(set.headSet(elem))
    testSubsets(set.tailSet(elem))
    testSubsets(set.subSet(elem, elem))
  }

  def testListImmutability[E](list: ju.List[E], elem: E, recursive: Boolean = false): Unit = {
    testCollectionImmutability(list, elem)
    expectThrows[UnsupportedOperationException](list.add(0, elem))
    expectThrows[UnsupportedOperationException](list.addAll(0, Seq.empty[E]))
    expectThrows[UnsupportedOperationException](list.remove(0))
    expectThrows[UnsupportedOperationException](list.set(0, elem))
    def testSublist(sl: ju.List[E]): Unit = {
      if (recursive) testCollectionImmutability(sl, elem)
      else testListImmutability(sl, elem, true)
    }
    testSublist(list.subList(0, list.size / 2))
    testListIteratorsImmutability(() => list.listIterator(), elem)
    testListIteratorsImmutability(() => list.listIterator(0), elem)
  }

  def testOnFirstPositionOfIterator[Iter <: ju.Iterator[_]](newIter: () => Iter,
      action: Iter => Unit, expectedException: (=> Any) => Unit): Unit = {
    val it = newIter()
    if (it.hasNext) {
      it.next()
      expectedException(action(it))
    }
  }

  def testMapImmutability[K, V](map: ju.Map[K, V], key: K, value: V): Unit = {
    expectThrows[UnsupportedOperationException](map.clear())
    expectThrows[UnsupportedOperationException](map.put(key, value))
    expectThrows[UnsupportedOperationException](map.putAll(Map.empty[K, V]))
    testSetImmutability(map.entrySet(), new ju.AbstractMap.SimpleImmutableEntry(key, value))
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
        expectThrows[UnsupportedOperationException])
  }

  def testListIteratorsImmutability[E](newIter: () => ju.ListIterator[E], elem: E): Unit = {
    testIteratorsImmutability(newIter)
    testOnFirstPositionOfIterator[ju.ListIterator[E]](newIter, _.add(elem),
        expectThrows[UnsupportedOperationException])
    testOnFirstPositionOfIterator[ju.ListIterator[E]](newIter, _.set(elem),
        expectThrows[UnsupportedOperationException])
  }

  def testCheckedCollection[D, E <: D](newColl: () => ju.Collection[E], elem: E, wrongElem: D): Unit = {
    def superColl(): ju.Collection[D] = newColl().asInstanceOf[ju.Collection[D]]
    it("should add instances of the correct type (Collection methods)") {
      expect(superColl().add(elem)).toBeTruthy
      expect(superColl().addAll(Seq(elem))).toBeTruthy
    }

    when("compliant-asinstanceofs").
    it("shouldn't add instances of a wrong type (Collection methods)") {
      expectThrows[ClassCastException](superColl().add(wrongElem))
      expectThrows[ClassCastException](superColl().addAll(Seq(wrongElem)))
    }
  }

  def testCheckedSet[D, E <: D](newSet: () => ju.Set[E], elem: E, wrongElem: D): Unit =
    testCheckedCollection(newSet, elem, wrongElem)

  def testCheckedSortedSet[D, E <: D](newSortedSet: () => ju.SortedSet[E],
      elem: E, wrongElem: D): Unit = {
    testCheckedSet(newSortedSet, elem, wrongElem)
  }

  def testCheckedList[D, E <: D](newList: () => ju.List[E], elem: E, wrongElem: D): Unit = {
    def superList(): ju.List[D] =
      newList().asInstanceOf[ju.List[D]]

    testCheckedCollection(newList, elem, wrongElem)

    it("should add instances of the correct type (List methods)") {
      expect(superList().add(0, elem)).toBeUndefined
      expect(superList().addAll(0, Seq(elem))).toBeTruthy
      testOnFirstPositionOfIterator[ju.ListIterator[D]](superList().listIterator,
          _.add(elem), expectNoException)
      testOnFirstPositionOfIterator[ju.ListIterator[D]](superList().listIterator,
          _.set(elem), expectNoException)
    }

    when("compliant-asinstanceofs").
    it("shouldn't add instances of a wrong type (List methods)") {
      expectThrows[ClassCastException](superList().add(0, wrongElem))
      expectThrows[ClassCastException](superList().addAll(0, Seq(wrongElem)))
      testOnFirstPositionOfIterator[ju.ListIterator[D]](superList().listIterator,
          _.add(wrongElem), expectThrows[ClassCastException])
      testOnFirstPositionOfIterator[ju.ListIterator[D]](superList().listIterator,
          _.set(wrongElem), expectThrows[ClassCastException])
    }
  }

  def testCheckedMap[J, K <: J, U, V <: U](newMap: () => ju.Map[K, V],
      key: K, wrongKey: J, value: V, wrongValue: U): Unit = {
    def superMap(): ju.Map[J, U] = newMap().asInstanceOf[ju.Map[J, U]]
    it("should add instances of the correct type (Map methods)") {
      expect(superMap().put(key, value) == null).toBeTruthy
    }

    when("compliant-asinstanceofs").
    it("shouldn't add instances of a wrong type (Map methods)") {
      expectThrows[ClassCastException](superMap().put(wrongKey, value))
      expectThrows[ClassCastException](superMap().put(key, wrongValue))
      expectThrows[ClassCastException](superMap().put(wrongKey, wrongValue))
      def singletonMap(): ju.Map[J, U] = {
        val m = superMap()
        m.put(key, value)
        m
      }
      expectThrows[ClassCastException](singletonMap().entrySet().head.setValue(wrongValue))
    }
  }

  def testCheckedSortedMap[J, K <: J, U, V <: U](newSortedMap: () => ju.SortedMap[K, V],
      key: K, wrongKey: J, value: V, wrongValue: U, recursive: Boolean = false): Unit = {
    def superSortedMap(): ju.SortedMap[J, U] = newSortedMap().asInstanceOf[ju.SortedMap[J, U]]
    def testSubsortedmap(fun: ju.SortedMap[K, V] => ju.SortedMap[K, V]): Unit = {
      if (recursive) testCheckedMap(() => fun(newSortedMap()), key, wrongKey, value, wrongValue)
      else testCheckedSortedMap(() => fun(newSortedMap()), key, wrongKey, value, wrongValue, true)
    }

    testCheckedMap(newSortedMap, key, wrongKey, value, wrongValue)

    testSubsortedmap(_.headMap(key))
    testSubsortedmap(_.tailMap(key))
    testSubsortedmap(_.subMap(key, key))
  }

  describe("java.util.Collections.sort") {
    // Test: sort[T<:Comparable[T]](List[T])
    for (factory <- ListFactory.allFactories) {
      it("should implement sort[T<:Comparable[T]](List[T]) and behave correctly" +
          s" with ${factory.implementationName}") {
        def test[T <: AnyRef with Comparable[T]](toElem: Int => T,
            absoluteOrder: Boolean = true): Unit = {

          val list = factory.empty[T]

          def testIfSorted(rangeValues: Boolean): Unit = {
            for (i <- range.init)
              expect(list(i).compareTo(list(i + 1)) <= 0).toBeTruthy
            if (absoluteOrder && rangeValues) {
              for (i <- range)
                expect(list(i).compareTo(toElem(i))).toEqual(0)
            }
          }

          list.addAll(range.map(toElem))
          ju.Collections.sort(list)
          testIfSorted(true)

          list.clear()
          list.addAll(range.reverse.map(toElem))
          ju.Collections.sort(list)
          testIfSorted(true)

          for (seed <- List(0, 1, 42, -5432, 2341242)) {
            val rnd = new scala.util.Random(seed)
            list.clear()
            list.addAll(range.map(_ => toElem(rnd.nextInt())))
            ju.Collections.sort(list)
            testIfSorted(false)
          }
        }

        test[CustomComparable](new CustomComparable(_), false)
        test[jl.Integer](jl.Integer.valueOf)
        test[jl.Long](_.toLong)
        test[jl.Double](_.toDouble)
      }
    }

    // Test: sort[T](List[T], Comparator[T])
    for (factory <- ListFactory.allFactories) {
      it("should implement sort[T](List[T], Comparator[T]) and behave correctly" +
          s" with ${factory.implementationName}") {
        def test[T](toElem: Int => T, cmpFun: (T, T) => Int, absoluteOrder: Boolean = true): Unit = {
          val list = factory.empty[T]

          def testIfSorted(rangeValues: Boolean): Unit = {
            for (i <- range.init)
              expect(cmpFun(list(i), list(i + 1)) <= 0).toBeTruthy
            if (absoluteOrder && rangeValues) {
              for (i <- range)
                expect(cmpFun(list(i), toElem(i))).toEqual(0)
            }
          }

          val cmp = new ju.Comparator[T] {
            override def compare(o1: T, o2: T): Int = cmpFun(o1, o2)
          }

          list.addAll(range.map(toElem))
          ju.Collections.sort(list, cmp)
          testIfSorted(true)

          list.clear()
          list.addAll(range.reverse.map(toElem))
          ju.Collections.sort(list, cmp)
          testIfSorted(true)

          for (seed <- List(0, 1, 42, -5432, 2341242)) {
            val rnd = new scala.util.Random(seed)
            list.clear()
            list.addAll(range.map(_ => toElem(rnd.nextInt())))
            ju.Collections.sort(list, cmp)
            testIfSorted(false)
          }
        }

        test[CustomComparable](new CustomComparable(_), (x, y) => x.compareTo(y), false)
        test[Int](_.toInt, (x, y) => x.compareTo(y))
        test[Long](_.toLong, (x, y) => x.compareTo(y))
        test[Double](_.toDouble, (x, y) => x.compareTo(y))
      }
    }
  }

  describe("java.util.Collections.binarySearch") {
    // Test: binarySearch[T](list: List[Comparable[T]], T)
    for (factory <- ListFactory.allFactories) {
      it("should implement binarySearch[T](List[Comparable[T]]) and behave correctly" +
          s" with ${factory.implementationName}") {
        def test[T <: AnyRef with Comparable[T]](toElem: Int => T): Unit = {
          val list = factory.empty[T]

          list.addAll(range.map(toElem))
          ju.Collections.sort(list)

          for (i <- Seq(range.head, range.last, range(range.size/3), range(range.size/2), range(3*range.size/5)))
            expect(ju.Collections.binarySearch(list, toElem(i))).toEqual(i)

          // If not found it should return: -(insertion point) - 1
          expect(ju.Collections.binarySearch(list, toElem(-1))).toEqual(-1)
          expect(ju.Collections.binarySearch(list, toElem(-42))).toEqual(-1)
          expect(ju.Collections.binarySearch(list, toElem(range.last + 1))).toEqual(-range.size - 1)
          expect(ju.Collections.binarySearch(list, toElem(range.last + 42))).toEqual(-range.size - 1)
          list.remove(range.last / 2)
          expect(ju.Collections.binarySearch(list, toElem(range.last / 2))).toEqual(-(range.last / 2) - 1)
        }

        test[jl.Integer](jl.Integer.valueOf)
        test[jl.Long](_.toLong)
        test[jl.Double](_.toDouble)
      }
    }

    // Test: binarySearch[T](List[T], key: T, Comparator[T]))
    for (factory <- ListFactory.allFactories) {
      it("should implement binarySearch[T](List[T], T, Comparator[T])) and behave" +
        s" correctly with ${factory.implementationName}") {
        def test[T](toElem: Int => T, cmpFun: (T, T) => Int): Unit = {
          val list = factory.empty[T]
          val cmp = new ju.Comparator[T] {
            override def compare(o1: T, o2: T): Int = cmpFun(o1, o2)
          }

          list.addAll(range.map(toElem))
          ju.Collections.sort(list, cmp)

          for (i <- Seq(range.head, range.last, range(range.size/3), range(range.size/2), range(3*range.size/5)))
            expect(ju.Collections.binarySearch(list, toElem(i), cmp)).toEqual(i)

          // If not found it should return: -(insertion point) - 1
          expect(ju.Collections.binarySearch(list, toElem(-1), cmp)).toEqual(-1)
          expect(ju.Collections.binarySearch(list, toElem(-42), cmp)).toEqual(-1)
          expect(ju.Collections.binarySearch(list, toElem(range.last + 1), cmp)).toEqual(-range.size - 1)
          expect(ju.Collections.binarySearch(list, toElem(range.last + 42), cmp)).toEqual(-range.size - 1)
          list.remove(range.last / 2)
          expect(ju.Collections.binarySearch(list, toElem(range.last / 2), cmp)).toEqual(-(range.last / 2) - 1)
        }

        test[Int](_.toInt, (x, y) => x.compareTo(y))
        test[Long](_.toLong, (x, y) => x.compareTo(y))
        test[Double](_.toDouble, (x, y) => x.compareTo(y))
      }
    }
  }

  describe("java.util.Collections.reverse") {
    // Test: reverse(list: List[_])
    for (factory <- ListFactory.allFactories) {
      it(s"should implement reverse(List[_]) and behave correctly with ${factory.implementationName}") {
        def test[T](toElem: Int => T): Unit = {
          val list = factory.empty[T]
          list.addAll(range.map(toElem))

          def testIfInOrder(reversed: Boolean): Unit = {
            for (i <- range) {
              val expected =
                if (reversed) range.last - i
                else i
              expect(list(i) == toElem(expected)).toBeTruthy
            }
          }

          ju.Collections.reverse(list)
          testIfInOrder(true)

          ju.Collections.reverse(list)
          testIfInOrder(false)
        }

        test[Int](_.toInt)
        test[Long](_.toLong)
        test[Double](_.toDouble)
        test[String](_.toString)
      }
    }
  }

  describe("java.util.Collections.shuffle") {
    def testShuffle(factory: ListFactory, shuffle: ju.List[_] => Unit): Unit = {
      def test[E](toElem: Int => E): Unit = {
        val list = factory.empty[E]
        ju.Collections.shuffle(list)
        expect(list.size).toEqual(0)
        list.addAll(range.map(toElem))
        shuffle(list)
        expect(list.size).toEqual(range.size)
        expect(list.containsAll(range.map(toElem))).toBeTruthy
      }
      test[Int](_.toInt)
      test[Long](_.toLong)
      test[Double](_.toDouble)
      test[String](_.toString)
    }

    // Test: shuffle(list: List[_])
    // Relies on the correctness of shuffle(list: List[_], rnd: Random)
    // Tests for this version are ommitted because they are not reproducible

    // Test: shuffle(list: List[_], rnd: Random)
    for (factory <- ListFactory.allFactories) {
      it(s"should implement shuffle(list: List[_], rnd: Random): Unit with ${factory.implementationName}") {
        testShuffle(factory, ju.Collections.shuffle(_, new ju.Random(0)))
        testShuffle(factory, ju.Collections.shuffle(_, new ju.Random(42)))
        testShuffle(factory, ju.Collections.shuffle(_, new ju.Random(-1243)))
        testShuffle(factory, ju.Collections.shuffle(_, new ju.Random(94325)))
      }
    }
  }

  describe("java.util.Collections.swap") {
    // Test: swap(List[_], Int, Int)
    for (factory <- ListFactory.allFactories) {
      it(s"should implement swap(List[_], Int, Int) and behave correctly with ${factory.implementationName} ") {
        def test[T](toElem: Int => T): Unit = {
          val list = factory.empty[T]
          list.addAll(range.map(toElem(_)))

          ju.Collections.swap(list, 0, 1)
          expect(list.get(0) == toElem(1)).toBeTruthy
          expect(list.get(1) == toElem(0)).toBeTruthy
          for (i <- range.drop(2))
            expect(list.get(i) == toElem(i)).toBeTruthy

          ju.Collections.swap(list, 0, range.last)
          expect(list.get(0) == toElem(range.last)).toBeTruthy
          expect(list.get(1) == toElem(0)).toBeTruthy
          for (i <- range.drop(2).init)
            expect(list.get(i) == toElem(i)).toBeTruthy
          expect(list.get(range.last) == toElem(1)).toBeTruthy

          ju.Collections.swap(list, 0, range.last)
          expect(list.get(0) == toElem(1)).toBeTruthy
          expect(list.get(1) == toElem(0)).toBeTruthy
          for (i <- range.drop(2))
            expect(list.get(i) == toElem(i)).toBeTruthy
        }

        test[Int](_.toInt)
        test[Long](_.toLong)
        test[Double](_.toDouble)
        test[String](_.toString)
      }
    }
  }

  describe("java.util.Collections.fill") {
    // Test: fill[T](List[T], T)
    for (factory <- ListFactory.allFactories) {
      it(s"should implement fill[T](List[T], T) and behave correctly with ${factory.implementationName}") {
        def test[T](toElem: Int => T): Unit = {
          val list = factory.empty[T]
          list.addAll(range.map(toElem(_)))

          ju.Collections.fill(list, toElem(0))
          for (i <- range)
            expect(list.get(i) == toElem(0)).toBeTruthy

          ju.Collections.fill(list, toElem(42))
          for (i <- range)
            expect(list.get(i) == toElem(42)).toBeTruthy
        }

        test[Int](_.toInt)
        test[Long](_.toLong)
        test[Double](_.toDouble)
        test[String](_.toString)
      }
    }
  }

  describe("java.util.Collections.copy") {
    // Test: copy[T](List[T], List[T])
    for (factory <- ListFactory.allFactories) {
      it(s"should implement copy[T](List[T], List[T]) and behave correctly with ${factory.implementationName}") {
        def test[T](toElem: Int => T): Unit = {
          val source = factory.empty[T]
          val dest = factory.empty[T]

          // Lists of same size
          range.foreach(i => source.add(toElem(i)))
          range.foreach(i => dest.add(toElem(-i)))
          ju.Collections.copy(dest, source)
          for (i <- range)
            expect(dest(i) == toElem(i)).toBeTruthy

          // source.size < dest.size
          source.clear()
          dest.clear()
          range.take(range.size / 2).foreach(i => source.add(toElem(i)))
          range.foreach(i => dest.add(toElem(-i)))
          ju.Collections.copy(dest, source)
          for (i <- range.take(range.size / 2))
            expect(dest(i) == toElem(i)).toBeTruthy
          for (i <- range.drop(range.size / 2))
            expect(dest(i) == toElem(-i)).toBeTruthy

          // source.size > dest.size
          source.clear()
          dest.clear()
          range.foreach(i => source.add(toElem(i)))
          range.take(range.size / 2).foreach(i => dest.add(toElem(-i)))
          expectThrows[IndexOutOfBoundsException](ju.Collections.copy(dest, source))
        }

        test[Int](_.toInt)
        test[Long](_.toLong)
        test[Double](_.toDouble)
        test[String](_.toString)
      }
    }
  }

  describe("java.util.Collections.{min, max}") {
    def testMinMax1[T <: AnyRef with Comparable[T]](factory: CollectionFactory,
        toElem: Int => T, isMin: Boolean): Unit = {
      val coll = factory.empty[T]
      coll.addAll(range.map(toElem))

      val minMax = if (isMin) range.head else range.last
      def getMinMax(): T =
        if (isMin) ju.Collections.min(coll)
        else ju.Collections.max(coll)

      expect(getMinMax().compareTo(toElem(minMax))).toEqual(0)

      coll match {
        case list: List[_] =>
          ju.Collections.shuffle(list, new ju.Random(42))
          expect(getMinMax().compareTo(toElem(minMax))).toEqual(0)
          ju.Collections.shuffle(list, new ju.Random(100000))
          expect(getMinMax().compareTo(toElem(minMax))).toEqual(0)
        case _ =>
      }
    }

    def testMinMax2[T](factory: CollectionFactory, toElem: Int => T,
        isMin: Boolean, cmp: ju.Comparator[T]): Unit = {
      val coll = factory.empty[T]
      coll.addAll(range.map(toElem))

      val minMax = if (isMin) range.head else range.last
      def getMinMax: T =
        if (isMin) ju.Collections.min(coll, cmp)
        else ju.Collections.max(coll, cmp)

      expect(cmp.compare(getMinMax, toElem(minMax))).toEqual(0)

      coll match {
        case list: List[_] =>
          ju.Collections.shuffle(list, new ju.Random(42))
          expect(cmp.compare(getMinMax, toElem(minMax))).toEqual(0)

          ju.Collections.shuffle(list, new ju.Random(100000))
          expect(cmp.compare(getMinMax, toElem(minMax))).toEqual(0)
        case _ =>
      }
    }

    for (factory <- CollectionFactory.allFactories) {
      it("should implement min[T <: AnyRef with jl.Comparable[T]](coll: Collection[T]): T" +
          s" with ${factory.implementationName}") {
        def test[T <: AnyRef with Comparable[T]](toElem: Int => T): Unit =
          testMinMax1(factory, toElem, true)

        test[jl.Integer](jl.Integer.valueOf)
        test[jl.Long](_.toLong)
        test[jl.Double](_.toDouble)
      }
    }

    for (factory <- CollectionFactory.allFactories) {
      it("should implement min[T](coll: Collection[T], comp: Comparator[T]): T" +
          s" with ${factory.implementationName}") {
        def test[T](toElem: Int => T, cmpFun: (T, T) => Int): Unit = {
          testMinMax2(factory, toElem, true, new Comparator[T] {
            override def compare(o1: T, o2: T): Int = cmpFun(o1, o2)
          })
        }

        test[Int](_.toInt, (x: Int, y: Int) => x.compareTo(y))
        test[Long](_.toLong, (x: Long, y: Long) => x.compareTo(y))
        test[Double](_.toDouble, (x: Double, y: Double) => x.compareTo(y))
      }
    }

    for (factory <- ListFactory.allFactories) {
      it("should implement max[T <: AnyRef with jl.Comparable[T]](coll: Collection[T]): T" +
          s" with ${factory.implementationName}") {
        def test[T <: AnyRef with Comparable[T]](toElem: Int => T): Unit =
          testMinMax1(factory, toElem, false)

        test[jl.Integer](jl.Integer.valueOf)
        test[jl.Long](_.toLong)
        test[jl.Double](_.toDouble)
      }
    }

    for (factory <- ListFactory.allFactories) {
      it("should implement max[T](coll: Collection[T], comp: Comparator[T]): T" +
          s" with ${factory.implementationName}") {
        def test[T](toElem: Int => T, cmpFun: (T, T) => Int): Unit = {
          testMinMax2(factory, toElem, false, new Comparator[T] {
            override def compare(o1: T, o2: T): Int = cmpFun(o1, o2)
          })
        }

        test[Int](_.toInt, (x: Int, y: Int) => x.compareTo(y))
        test[Long](_.toLong, (x: Long, y: Long) => x.compareTo(y))
        test[Double](_.toDouble, (x: Double, y: Double) => x.compareTo(y))
      }
    }
  }

  describe("java.util.Collections.rotate") {
    def modulo(a: Int, b: Int): Int = ((a % b) + b) % b
    for (factory <- ListFactory.allFactories) {
      it(s"should implement rotate(list: List[_], distance: Int): Unit with ${factory.implementationName}") {
        def test[E](toElem: Int => E): Unit = {
          val list = factory.empty[E]
          list.addAll(range.map(toElem))

          ju.Collections.rotate(list, 0)
          for (i <- range)
            expect(list(i) == toElem(i)).toBeTruthy

          ju.Collections.rotate(list, list.size)
          for (i <- range)
            expect(list(i) == toElem(i)).toBeTruthy

          ju.Collections.rotate(list, 1)
          for (i <- range)
            expect(list(i) == toElem(modulo(i - 1, range.size))).toBeTruthy

          ju.Collections.rotate(list, 1)
          for (i <- range)
            expect(list(i) == toElem(modulo(i - 2, range.size))).toBeTruthy

          ju.Collections.rotate(list, -5)
          for (i <- range)
            expect(list(i) == toElem(modulo(i + 3, range.size))).toBeTruthy

          list.clear()
          list.addAll((0 until 6).map(toElem))
          ju.Collections.rotate(list, 2)
          for (i <- 0 until 6)
            expect(list(i) == toElem(modulo(i - 2, 6))).toBeTruthy
        }

        test[Int](_.toInt)
        test[Long](_.toLong)
        test[Double](_.toDouble)
        test[String](_.toString)
      }
    }
  }

  describe("java.util.Collections.replaceAll") {
    for (factory <- ListFactory.allFactories) {
      it("should implement replaceAll[T](list: List[T], oldVal: T, newVal: T): Boolean" +
          s" with ${factory.implementationName}") {
        def test[E](toElem: Int => E): Unit = {
          val list = factory.empty[E]
          list.addAll(range.map(toElem))

          ju.Collections.replaceAll(list, toElem(range.last), toElem(0))
          for (i <- range.init)
            expect(list(i) == toElem(i)).toBeTruthy
          expect(list.last == toElem(0)).toBeTruthy

          ju.Collections.replaceAll(list, toElem(range(range.size - 2)), toElem(0))
          for (i <- range.dropRight(2))
            expect(list(i) == toElem(i)).toBeTruthy
          expect(list.dropRight(1).last == toElem(0)).toBeTruthy
          expect(list.last == toElem(0)).toBeTruthy

          ju.Collections.replaceAll(list, toElem(0), toElem(-1))
          for (i <- range.tail.dropRight(2))
            expect(list(i) == toElem(i)).toBeTruthy
          expect(list.head == toElem(-1)).toBeTruthy
          expect(list.dropRight(1).last == toElem(-1)).toBeTruthy
          expect(list.last == toElem(-1)).toBeTruthy
        }

        test[Int](_.toInt)
        test[Long](_.toLong)
        test[Double](_.toDouble)
        test[String](_.toString)
      }
    }
  }

  describe("java.util.Collections.{indexOfSubList, lastIndexOfSubList}") {
    for (factory <- ListFactory.allFactories) {
      it(s"should implement indexOfSubList(List[_], target: List[_]): Int with ${factory.implementationName}") {
        def test[E](toElem: Int => E): Unit = {
          val source = factory.empty[E]
          val target = factory.empty[E]

          expect(ju.Collections.indexOfSubList(source, target)).toEqual(0)

          source.addAll(range.map(toElem))
          expect(ju.Collections.indexOfSubList(source, target)).toEqual(0)

          target.addAll(range.map(toElem))
          expect(ju.Collections.indexOfSubList(source, target)).toEqual(0)

          source.addAll(range.map(toElem))
          expect(ju.Collections.indexOfSubList(source, target)).toEqual(0)

          source.addAll(range.map(toElem))
          expect(ju.Collections.indexOfSubList(source, target)).toEqual(0)

          source.remove(0)
          expect(ju.Collections.indexOfSubList(source, target)).toEqual(range.size - 1)

          target.add(0, toElem(-5))
          expect(ju.Collections.indexOfSubList(source, target)).toEqual(-1)
        }

        test[Int](_.toInt)
        test[Long](_.toLong)
        test[Double](_.toDouble)
        test[String](_.toString)
      }
    }

    for (factory <- ListFactory.allFactories) {
      it("should implement lastIndexOfSubList(source: List[_], target: List[_]): Int" +
          s" with ${factory.implementationName}") {
        def test[E](toElem: Int => E): Unit = {
          val source = factory.empty[E]
          val target = factory.empty[E]

          expect(ju.Collections.lastIndexOfSubList(source, target)).toEqual(0)

          source.addAll(range.map(toElem))
          expect(ju.Collections.lastIndexOfSubList(source, target)).toEqual(0)

          target.addAll(range.map(toElem))
          expect(ju.Collections.lastIndexOfSubList(source, target)).toEqual(0)

          source.addAll(range.map(toElem))
          expect(ju.Collections.lastIndexOfSubList(source, target)).toEqual(range.size)

          source.addAll(range.map(toElem))
          expect(ju.Collections.lastIndexOfSubList(source, target)).toEqual(2 * range.size)

          source.remove(source.size - 1)
          expect(ju.Collections.lastIndexOfSubList(source, target)).toEqual(range.size)

          target.add(0, toElem(-5))
          expect(ju.Collections.lastIndexOfSubList(source, target)).toEqual(-1)
        }

        test[Int](_.toInt)
        test[Long](_.toLong)
        test[Double](_.toDouble)
        test[String](_.toString)
      }
    }
  }

  describe("java.util.Collections.unmodifiable*") {
    for (factory <- CollectionFactory.allFactories) {
      it("should implement unmodifiableCollection[T](c: Collection[T]): Collection[T]" +
          s" with ${factory.implementationName}") {
        def test[E](toElem: Int => E): Unit = {
          val coll = factory.empty[E]
          testCollectionImmutability(ju.Collections.unmodifiableCollection(coll), toElem(0))
          coll.addAll(range.map(toElem))
          testCollectionImmutability(ju.Collections.unmodifiableCollection(coll), toElem(0))
        }

        test[Int](_.toInt)
        test[Long](_.toLong)
        test[Double](_.toDouble)
        test[String](_.toString)
      }
    }

    for (factory <- SetFactory.allFactories) {
      it(s"should implement unmodifiableSet[T](a: Set[T]): Set[T] with ${factory.implementationName}") {
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

    for (factory <- SortedSetFactory.allFactories) {
      it("should implement unmodifiableSortedSet[T](s: SortedSet[T]): SortedSet[T]" +
          s" with ${factory.implementationName}") {
        def test[E](toElem: Int => E): Unit = {
          val sortedSet = factory.empty[E]
          testSortedSetImmutability(ju.Collections.unmodifiableSortedSet(sortedSet), toElem(0))
          sortedSet.addAll(range.map(toElem))
          testSortedSetImmutability(ju.Collections.unmodifiableSortedSet(sortedSet), toElem(0))
        }

        test[Int](_.toInt)
        test[Long](_.toLong)
        test[Double](_.toDouble)
        test[String](_.toString)
      }
    }

    for (factory <- ListFactory.allFactories) {
      it("should implement unmodifiableList[T](list: List[T]): List[T]" +
          s" with ${factory.implementationName}") {
        def test[E](toElem: Int => E): Unit = {
          val immuList = ju.Collections.unmodifiableList(factory.empty[E])
          testListImmutability(immuList, toElem(0))
        }

        test[Int](_.toInt)
        test[Long](_.toLong)
        test[Double](_.toDouble)
        test[String](_.toString)
      }
    }

    for (factory <- MapFactory.allFactories) {
      it("should implement unmodifiableMap[K, V](m: Map[K, V]): Map[K, V]" +
          s" with ${factory.implementationName}") {
        def test[K, V](toKey: Int => K, toValue: Int => V): Unit = {
          val map = factory.empty[K, V]
          testMapImmutability(ju.Collections.unmodifiableMap[K, V](map), toKey(0), toValue(0))
          for (i <- range)
            map.put(toKey(i), toValue(i))
          testMapImmutability(ju.Collections.unmodifiableMap[K, V](map), toKey(0), toValue(0))
        }

        test[Int, Int](_.toInt, _.toInt)
        test[Long, String](_.toLong, _.toString)
        test[String, String](_.toString, _.toString)
        test[Double, Double](_.toDouble, _.toDouble)
      }
    }

    for (factory <- SortedMapFactory.allFactories) {
      it("should implement unmodifiableSortedMap[K, V](m: SortedMap[K, V]): SortedMap[K, V]" +
          s" with ${factory.implementationName}") {
        def test[K, V](toKey: Int => K, toValue: Int => V): Unit = {
          val sortedMap = factory.empty[K, V]
          testMapImmutability(ju.Collections.unmodifiableSortedMap[K, V](sortedMap), toKey(0), toValue(0))
          for (i <- range)
            sortedMap.put(toKey(i), toValue(i))
          testMapImmutability(ju.Collections.unmodifiableSortedMap[K, V](sortedMap), toKey(0), toValue(0))
        }

        test[Int, Int](_.toInt, _.toInt)
        test[Long, String](_.toLong, _.toString)
        test[String, String](_.toString, _.toString)
        test[Double, Double](_.toDouble, _.toDouble)
      }
    }
  }

  for (factory <- CollectionFactory.allFactories) {
    describe("java.util.Collections.synchronizedCollection[T]" +
        s"(${factory.implementationName}[T]): Collection[T]") {
      testCollectionApi(
        new CollectionFactory {
          override def implementationName: String =
            s"synchronizedCollection(${factory.implementationName})"

          override def empty[E]: ju.Collection[E] =
            ju.Collections.synchronizedCollection(factory.empty[E])
        }
      )
    }
  }

  for (factory <- SetFactory.allFactories) {
    describe(s"java.util.Collections.synchronizedSet[T](${factory.implementationName}[T]): Set[T]") {
      testSetApi(
        new SetFactory {
          def implementationName: String =
            s"synchronizedSet(${factory.implementationName})"

          def empty[E]: ju.Set[E] =
            ju.Collections.synchronizedSet(factory.empty[E])

          def allowsNullElement: Boolean =
            factory.allowsNullElement
        }
      )
    }
  }

  for (factory <- SortedSetFactory.allFactories) {
    describe("java.util.Collections.synchronizedSortedSet[T]" +
        s"(${factory.implementationName}[T]): SortedSet[T]") {
      testSortedSetApi(
        new SortedSetFactory {
          def implementationName: String =
            s"synchronizedSortedSet(${factory.implementationName})"

          def empty[E]: ju.SortedSet[E] =
            ju.Collections.synchronizedSortedSet(factory.empty[E])

          def allowsNullElement: Boolean =
            factory.allowsNullElement
        }
      )
    }
  }

  for (factory <- ListFactory.allFactories) {
    describe("java.util.Collections.synchronizedList[T]" +
        s"(${factory.implementationName}[T]): List[T]") {
      testListApi(
        new ListFactory {
          def implementationName: String =
            s"synchronizedList(${factory.implementationName})"

          def empty[E]: ju.List[E] =
            ju.Collections.synchronizedList(factory.empty[E])
        }
      )
    }
  }

  for (factory <- MapFactory.allFactories) {
    describe("java.util.Collections.synchronizedMap[K, V]" +
        s"(${factory.implementationName}[K, V]): Map[K, V]") {
      testMapApi(
        new MapFactory {
          def implementationName: String =
            s"synchronizedMap(${factory.implementationName})"

          def empty[K, V]: ju.Map[K, V] =
            ju.Collections.synchronizedMap(factory.empty[K, V])

          override def allowsNullKeys: Boolean =
            factory.allowsNullKeys

          override def allowsNullValues: Boolean =
            factory.allowsNullValues
        }
      )
    }
  }

  for (factory <- SortedMapFactory.allFactories) {
    describe("java.util.Collections.synchronizedSortedMap[K, V]" +
        s"(${factory.implementationName}[K, V]): SortedMap[K, V]") {
      testSortedMapApi(
        new SortedMapFactory {
          def implementationName: String =
            s"synchronizedSortedMap(${factory.implementationName})"

          def empty[K, V]: ju.SortedMap[K, V] =
            ju.Collections.synchronizedSortedMap(factory.empty[K, V])

          override def allowsNullKeys: Boolean =
            factory.allowsNullKeys

          override def allowsNullValues: Boolean =
            factory.allowsNullValues
        }
      )
    }
  }

  class A extends jl.Comparable[A] {
    def compareTo(o: A): Int = this.##.compareTo(o.##)
  }

  class B extends A

  class C extends B

  for (factory <- CollectionFactory.allFactories) {
    describe("java.util.Collections.checkedCollection[E]" +
        s"(${factory.implementationName}[E], Class[E]): Collection[E]") {
      testCheckedCollection(() => ju.Collections.checkedCollection(factory.empty[B], classOf[B]), new C, new A)
    }
  }

  for (factory <- SetFactory.allFactories) {
    describe("java.util.Collections.checkedSet[E]" +
        s"(${factory.implementationName}[E], Class[E]): Set[E]") {
      testCheckedSet(() => ju.Collections.checkedSet(factory.empty[B], classOf[B]), new C, new A)
    }
  }

  for (factory <- SortedSetFactory.allFactories) {
    describe("java.util.Collections.checkedSortedSet[E]" +
        s"(${factory.implementationName}[E], Class[E]): SortedSet[E]") {
      testCheckedSortedSet(() => ju.Collections.checkedSortedSet(factory.empty[B], classOf[B]), new C, new A)
    }
  }

  for (factory <- ListFactory.allFactories) {
    describe("java.util.Collections.checkedList[E]" +
        s"(${factory.implementationName}[E], Class[E]): List[E]") {
      testCheckedList(() => ju.Collections.checkedList(factory.empty[B], classOf[B]), new C, new A)
    }
  }

  for (factory <- MapFactory.allFactories) {
    describe("java.util.Collections.checkedMap[K, V]" +
        s"(${factory.implementationName}[K, V], Class[K], Class[V]): Map[K, V]") {
      testCheckedMap(() => ju.Collections.checkedMap(factory.empty[B, B], classOf[B], classOf[B]),
          new C, new A, new C, new A)
    }
  }

  for (factory <- SortedMapFactory.allFactories) {
    describe("should implement checkedSortedMap[K, V]" +
        s"(${factory.implementationName}[K, V], Class[K], Class[V]): SortedMap[K, V]") {
      testCheckedSortedMap(() => ju.Collections.checkedSortedMap(factory.empty[B, B], classOf[B], classOf[B]),
          new C, new A, new C, new A)
    }
  }

  describe("java.util.Collections.empty*") {

    /* emptyIterator, emptyListIterator and emptyEnumeration are not supported
     * in JDK6 and therefore are not tested here. They are located in:
     * test-suite/src/tests/require-jdk7/org/scalajs/testsuite/javalib/CollectionsTestOnJDK7.scala
     */

    it(s"should implement emptySet[T](): Set[T]") {
      def test[E](toElem: Int => E): Unit = {
        val emptySet = ju.Collections.emptySet[E]
        expect(emptySet.isEmpty).toBeTruthy
        expect(emptySet.size).toEqual(0)
        expect(emptySet.iterator.size).toEqual(0)
        testSetImmutability(emptySet, toElem(0))
      }

      test[Int](_.toInt)
      test[Long](_.toLong)
      test[Double](_.toDouble)
    }

    it(s"should implement emptyList[T](): List[T]") {
      def test[E](toElem: Int => E): Unit = {
        val emptyList = ju.Collections.emptyList[E]
        expect(emptyList.isEmpty).toBeTruthy
        expect(emptyList.size).toEqual(0)
        expect(emptyList.iterator.size).toEqual(0)
        testListImmutability(emptyList, toElem(0))
      }

      test[Int](_.toInt)
      test[Long](_.toLong)
      test[Double](_.toDouble)
    }

    it(s"should implement emptyMap[K, V](): Map[K, V]") {
      def test[K, V](toKey: Int => K, toValue: Int => V): Unit = {
        val emptyMap = ju.Collections.emptyMap[K, V]
        expect(emptyMap.isEmpty).toBeTruthy
        expect(emptyMap.size).toEqual(0)
        expect(emptyMap.entrySet.size).toEqual(0)
        expect(emptyMap.keySet.size).toEqual(0)
        expect(emptyMap.values.size).toEqual(0)
        testMapImmutability(emptyMap, toKey(0), toValue(0))
      }

      test[Int, Int](_.toInt, _.toInt)
      test[Long, String](_.toLong, _.toString)
      test[Double, Double](_.toDouble, _.toDouble)
    }
  }

  describe("java.util.Collections.singleton*") {
    it(s"should implement singleton[T](o: T): Set[T]") {
      def test[E](toElem: Int => E): Unit = {
        val singletonSet = ju.Collections.singleton[E](toElem(0))
        expect(singletonSet.contains(toElem(0))).toBeTruthy
        expect(singletonSet.size).toEqual(1)
        expect(singletonSet.iterator.size).toEqual(1)
        testSetImmutability(singletonSet, toElem(0))
      }

      test[Int](_.toInt)
      test[Long](_.toLong)
      test[Double](_.toDouble)
    }

    it(s"should implement singletonList[T](o: T): List[T]") {
      def test[E](toElem: Int => E): Unit = {
        val singletonList = ju.Collections.singletonList[E](toElem(0))
        expect(singletonList.contains(toElem(0))).toBeTruthy
        expect(singletonList.size).toEqual(1)
        expect(singletonList.iterator.size).toEqual(1)
        testListImmutability(singletonList, toElem(0))
      }

      test[Int](_.toInt)
      test[Long](_.toLong)
      test[Double](_.toDouble)
    }

    it(s"should implement singletonMap[K, V](key: K, value: V): Map[K, V]") {
      def test[K, V](toKey: Int => K, toValue: Int => V): Unit = {
        val singletonMap = ju.Collections.singletonMap[K, V](toKey(0), toValue(1))
        expect(singletonMap.get(toKey(0)) == toValue(1)).toBeTruthy
        expect(singletonMap.size).toEqual(1)
        expect(singletonMap.iterator.size).toEqual(1)
        testMapImmutability(singletonMap, toKey(0), toValue(0))
      }

      test[Int, Int](_.toInt, _.toInt)
      test[Long, String](_.toLong, _.toString)
      test[Double, Double](_.toDouble, _.toDouble)
    }
  }

  describe("java.util.Collections.nCopies") {
    it(s"should implement nCopies[T](n: Int, o: T): List[T]") {
      def test[E](toElem: Int => E): Unit = {
        for (n <- Seq(1, 4, 543)) {
          val nCopies = ju.Collections.nCopies(n, toElem(0))
          expect(nCopies.contains(toElem(0))).toBeTruthy
          nCopies.forall(_ == toElem(0))
          expect(nCopies.size).toEqual(n)
          expect(nCopies.iterator.size).toEqual(n)
          testListImmutability(nCopies, toElem(0))
        }

        val zeroCopies = ju.Collections.nCopies(0, toElem(0))
        expect(zeroCopies.contains(toElem(0))).toBeFalsy
        expect(zeroCopies.size).toEqual(0)
        expect(zeroCopies.iterator.size).toEqual(0)
        testListImmutability(zeroCopies, toElem(0))

        for (n <- Seq(-1, -4, -543))
          expectThrows[IllegalArgumentException](ju.Collections.nCopies(n, toElem(0)))
      }

      test[Int](_.toInt)
      test[Long](_.toLong)
      test[Double](_.toDouble)
    }
  }

  describe("java.util.Collections.reverseOrder") {
    it(s"should implement reverseOrder[T](): Comparator[T]") {

      def testNumerical[E](toElem: Int => E): Unit = {
        val rCmp = ju.Collections.reverseOrder[E]
        for (i <- range) {
          expect(rCmp.compare(toElem(i), toElem(i))).toEqual(0)
          expect(rCmp.compare(toElem(i), toElem(i - 1)) < 0).toBeTruthy
          expect(rCmp.compare(toElem(i), toElem(i + 1)) > 0).toBeTruthy
        }
      }

      testNumerical[Int](_.toInt)
      testNumerical[Long](_.toLong)
      testNumerical[Double](_.toDouble)

      val rCmp = ju.Collections.reverseOrder[String]

      expect(rCmp.compare("", "")).toEqual(0)
      expect(rCmp.compare("a", "a")).toEqual(0)
      expect(rCmp.compare("123", "123")).toEqual(0)
      expect(rCmp.compare("hello world", "hello world")).toEqual(0)

      expect(rCmp.compare("a", "b") > 0).toBeTruthy
      expect(rCmp.compare("a", "ba") > 0).toBeTruthy
      expect(rCmp.compare("a", "aa") > 0).toBeTruthy
      expect(rCmp.compare("aa", "aaa") > 0).toBeTruthy

      expect(rCmp.compare("b", "a") < 0).toBeTruthy
      expect(rCmp.compare("ba", "a") < 0).toBeTruthy
      expect(rCmp.compare("aa", "a") < 0).toBeTruthy
      expect(rCmp.compare("aaa", "aa") < 0).toBeTruthy
    }

    it(s"should implement reverseOrder[T](Comparator[T]): Comparator[T]") {
      val rCmp1 = new ju.Comparator[Int] {
        override def compare(o1: Int, o2: Int): Int = o2 - o1
      }
      val rCmp2 = ju.Collections.reverseOrder(new ju.Comparator[Int] {
        override def compare(o1: Int, o2: Int): Int = o1 - o2
      })

      scala.util.Random.setSeed(42)
      for (_ <- 0 to 50) {
        val num = scala.util.Random.nextInt(10000)
        expect(rCmp1.compare(num, num)).toEqual(0)
        expect(rCmp2.compare(num, num)).toEqual(0)
      }

      for (i <- range) {
        for (_ <- 1 to 10) {
          val num = scala.util.Random.nextInt(10000) + 1
          expect(rCmp1.compare(i, i + num) > 0).toBeTruthy
          expect(rCmp2.compare(i, i + num) > 0).toBeTruthy
          expect(rCmp1.compare(i, i - num) < 0).toBeTruthy
          expect(rCmp2.compare(i, i - num) < 0).toBeTruthy
        }
      }

      for (_ <- 1 to 100) {
        val num1 = scala.util.Random.nextInt(10000)
        val num2 = scala.util.Random.nextInt(10000)
        expect(rCmp1.compare(num1, num2)).toEqual(rCmp2.compare(num1, num2))
      }
    }
  }

  describe("java.util.Collections.{enumeration, list}") {
    it(s"should implement enumeration[T](c: Collection[T]): Enumeration[T]") {
      val coll = asJavaCollection(range)
      val enum = ju.Collections.enumeration(coll)
      for (elem <- coll) {
        expect(enum.hasMoreElements).toBeTruthy
        expect(enum.nextElement()).toEqual(elem)
      }
      expect(enum.hasMoreElements).toBeFalsy
    }

    it(s"should implement list[T](e: Enumeration[T]): ArrayList[T]") {
      val enum = asJavaEnumeration(range.iterator)
      val list = ju.Collections.list(enum)
      expect(list.size).toEqual(range.size)
      for (i <- range)
        expect(list.get(i)).toEqual(i)
    }
  }

  describe("java.util.Collections.frequency") {
    for (factory <- CollectionFactory.allFactories) {
      it("should implement frequency(Collection[_], AnyRef): Int" +
          s" with ${factory.implementationName}") {
        def test[E](toElem: Int => E): Unit = {
          val coll = factory.empty[E]

          def expectAllFrequenciesToBe(n: Int): Unit = {
            for (i <- range)
              expect(ju.Collections.frequency(coll, toElem(i))).toEqual(n)
          }

          expectAllFrequenciesToBe(0)
          coll.addAll(range.map(toElem))
          expectAllFrequenciesToBe(1)
          coll.addAll(range.map(toElem))
          coll match {
            case _: ju.Set[_]  => expectAllFrequenciesToBe(1)
            case _: ju.List[_] => expectAllFrequenciesToBe(2)
            case _             => // Undefined behaviour
          }
        }

        test[Int](_.toInt)
        test[Long](_.toLong)
        test[Double](_.toDouble)
        test[String](_.toString)
      }
    }
  }

  describe("java.util.Collections.disjoint") {
    it(s"should implement disjoint(Collection[_], Collection[_]): Boolean") {
      expect(ju.Collections.disjoint(0 to 3, 0 to 3)).toBeFalsy
      expect(ju.Collections.disjoint(0 to 3, 3 to 5)).toBeFalsy
      expect(ju.Collections.disjoint(0 to 3, 6 to 9)).toBeTruthy
      expect(ju.Collections.disjoint(0 to -1, 0 to 3)).toBeTruthy
      expect(ju.Collections.disjoint(0 to 3, 0 to -1)).toBeTruthy
      expect(ju.Collections.disjoint(0 to -1, 0 to -1)).toBeTruthy
    }
  }

  describe("java.util.Collections.addAll") {
    for (factory <- CollectionFactory.allFactories) {
      it(s"should implement addAll[T](Collection[T], T*): Boolean with ${factory.implementationName}") {
        def test[E](toElem: Int => E): Unit = {
          val coll = factory.empty[E]
          expect(ju.Collections.addAll(coll)).toBeFalsy
          expect(coll.isEmpty).toBeTruthy
          expect(ju.Collections.addAll(coll, toElem(0), toElem(1))).toBeTruthy
          expect(coll.contains(toElem(0))).toBeTruthy
          expect(coll.contains(toElem(1))).toBeTruthy
        }

        test[Int](_.toInt)
        test[Long](_.toLong)
        test[Double](_.toDouble)
      }
    }
  }

  for (factory <- MapFactory.allFactories) {
    describe(s"java.util.Collections.newSetFromMap[E](${factory.implementationName}[E, Boolean]): Set[E]") {
      def test[E](toElem: Int => E): Unit = {
        testSetApi(new SetFactory {
          def implementationName: String =
            s"newSetFromMap(${factory.implementationName})"

          def empty[E]: ju.Set[E] =
            ju.Collections.newSetFromMap[E](factory.empty[E, jl.Boolean])

          def allowsNullElement: Boolean =
            factory.allowsNullKeys
        })
      }

      test[Int](_.toInt)
      test[Long](_.toLong)
      test[Double](_.toDouble)
      test[String](_.toString)
    }
  }

  class CustomComparable(val value: Int) extends jl.Comparable[CustomComparable] {
    override def compareTo(o: CustomComparable): Int =
      (value % 8).compareTo(o.value % 8)

    override def toString(): String =
      s"CustomComparable($value)"
  }
}
