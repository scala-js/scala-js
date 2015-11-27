package org.scalajs.testsuite.javalib.util

import java.{util => ju}

import org.scalajs.testsuite.utils.CollectionsTestBase

import scala.collection.JavaConversions._

import org.junit.Assert._

import org.scalajs.jasminetest.JasmineTest
import org.scalajs.testsuite.utils.AssertThrows._

object CollectionsJSTest extends JasmineTest with CollectionsTestBase {

  def testCheckedCollection[D, E <: D](newColl: () => ju.Collection[E], elem: E,
      wrongElem: D): Unit = {
    def superColl(): ju.Collection[D] = newColl().asInstanceOf[ju.Collection[D]]

    it("should add instances of the correct type (Collection methods)") {
      assertTrue(superColl().add(elem))
      assertTrue(superColl().addAll(Seq(elem)))
    }

    when("compliant-asinstanceofs").
    it("shouldn't add instances of a wrong type (Collection methods)") {
      expectThrows(classOf[ClassCastException], superColl().add(wrongElem))
      expectThrows(classOf[ClassCastException],
          superColl().addAll(Seq(wrongElem)))
    }
  }

  def testCheckedSet[D, E <: D](newSet: () => ju.Set[E], elem: E,
      wrongElem: D): Unit = {
    testCheckedCollection(newSet, elem, wrongElem)
  }

  def testCheckedSortedSet[D, E <: D](newSortedSet: () => ju.SortedSet[E],
      elem: E, wrongElem: D): Unit = {
    testCheckedSet(newSortedSet, elem, wrongElem)
  }

  def testCheckedList[D, E <: D](newList: () => ju.List[E], elem: E,
      wrongElem: D): Unit = {
    def superList(): ju.List[D] =
      newList().asInstanceOf[ju.List[D]]

    testCheckedCollection(newList, elem, wrongElem)

    it("should add instances of the correct type (List methods)") {
      expect(superList().add(0, elem)).toBeUndefined
      assertTrue(superList().addAll(0, Seq(elem)))
      testOnFirstPositionOfIterator[ju.ListIterator[D]](superList().listIterator,
        _.add(elem), None)
      testOnFirstPositionOfIterator[ju.ListIterator[D]](superList().listIterator,
        _.set(elem), None)
    }

    when("compliant-asinstanceofs").
    it("shouldn't add instances of a wrong type (List methods)") {
      expectThrows(classOf[ClassCastException], superList().add(0, wrongElem))
      expectThrows(classOf[ClassCastException],
          superList().addAll(0, Seq(wrongElem)))
      testOnFirstPositionOfIterator[ju.ListIterator[D]](
          superList().listIterator,
          _.add(wrongElem), Some(classOf[ClassCastException]))
      testOnFirstPositionOfIterator[ju.ListIterator[D]](
          superList().listIterator,
          _.set(wrongElem), Some(classOf[ClassCastException]))
    }
  }

  def testCheckedMap[J, K <: J, U, V <: U](newMap: () => ju.Map[K, V],
      key: K, wrongKey: J, value: V, wrongValue: U): Unit = {
    def superMap(): ju.Map[J, U] = newMap().asInstanceOf[ju.Map[J, U]]

    it("should add instances of the correct type (Map methods)") {
      assertTrue(superMap().put(key, value) == null)
    }

    when("compliant-asinstanceofs").
    it("shouldn't add instances of a wrong type (Map methods)") {
      expectThrows(classOf[ClassCastException], superMap().put(wrongKey, value))
      expectThrows(classOf[ClassCastException], superMap().put(key, wrongValue))
      expectThrows(classOf[ClassCastException],
          superMap().put(wrongKey, wrongValue))

      def singletonMap(): ju.Map[J, U] = {
        val m = superMap()
        m.put(key, value)
        m
      }
      expectThrows(classOf[ClassCastException],
          singletonMap().entrySet().head.setValue(wrongValue))
    }
  }

  def testCheckedSortedMap[J, K <: J, U, V <: U](
      newSortedMap: () => ju.SortedMap[K, V], key: K, wrongKey: J, value: V,
      wrongValue: U, recursive: Boolean = false): Unit = {
    def superSortedMap(): ju.SortedMap[J, U] =
      newSortedMap().asInstanceOf[ju.SortedMap[J, U]]

    def testSubsortedmap(fun: ju.SortedMap[K, V] => ju.SortedMap[K, V]): Unit = {
      if (recursive)
        testCheckedMap(() => fun(newSortedMap()), key, wrongKey, value, wrongValue)
      else
        testCheckedSortedMap(() => fun(newSortedMap()), key, wrongKey, value, wrongValue, true)
    }

    testCheckedMap(newSortedMap, key, wrongKey, value, wrongValue)

    testSubsortedmap(_.headMap(key))
    testSubsortedmap(_.tailMap(key))
    testSubsortedmap(_.subMap(key, key))
  }

  for (factory <- CollectionFactory.allFactories) {
    describe("java.util.Collections.checkedCollection[E]" +
        s"(${factory.implementationName}[E], Class[E]): Collection[E]") {
      testCheckedCollection(() => ju.Collections.checkedCollection(
          factory.empty[B], classOf[B]), new C, new A)
    }
  }

  for (factory <- SetFactory.allFactories) {
    describe("java.util.Collections.checkedSet[E]" +
        s"(${factory.implementationName}[E], Class[E]): Set[E]") {
      testCheckedSet(() => ju.Collections.checkedSet(factory.empty[B],
          classOf[B]), new C, new A)
    }
  }

  for (factory <- SortedSetFactory.allFactories) {
    describe("java.util.Collections.checkedSortedSet[E]" +
        s"(${factory.implementationName}[E], Class[E]): SortedSet[E]") {
      testCheckedSortedSet(() => ju.Collections.checkedSortedSet(
          factory.empty[B], classOf[B]), new C, new A)
    }
  }

  for (factory <- ListFactory.allFactories) {
    describe("java.util.Collections.checkedList[E]" +
        s"(${factory.implementationName}[E], Class[E]): List[E]") {
      testCheckedList(() => ju.Collections.checkedList(factory.empty[B],
          classOf[B]), new C, new A)
    }
  }

  for (factory <- MapFactory.allFactories) {
    describe("java.util.Collections.checkedMap[K, V]" +
        s"(${factory.implementationName}[K, V], Class[K], Class[V]): Map[K, V]") {
      testCheckedMap(() => ju.Collections.checkedMap(factory.empty[B, B],
          classOf[B], classOf[B]), new C, new A, new C, new A)
    }
  }

  for (factory <- SortedMapFactory.allFactories) {
    describe("should implement checkedSortedMap[K, V]" +
        s"(${factory.implementationName}[K, V], Class[K], Class[V]): SortedMap[K, V]") {
      testCheckedSortedMap(() => ju.Collections.checkedSortedMap(factory.empty[B, B],
        classOf[B], classOf[B]), new C, new A, new C, new A)
    }
  }
}
