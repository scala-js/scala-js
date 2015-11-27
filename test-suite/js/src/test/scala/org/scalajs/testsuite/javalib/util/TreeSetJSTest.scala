/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import org.scalajs.jasminetest.JasmineTest

import scala.language.implicitConversions

import scala.collection.JavaConversions._

import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

object TreeSetJSTest extends TreeSetJSTest(new TreeSetFactory)

object TreeSetWithNullJSTest extends TreeSetJSTest(new TreeSetWithNullFactory)

class TreeSetJSTest(treeSetTestFactory: TreeSetFactory) extends JasmineTest {
  class TestObj(num: Int)

  describe(treeSetTestFactory.implementationName) {

    when("compliant-asinstanceofs").
    it("should throw exception on non comparable objects") {
      val ts1 = treeSetTestFactory.empty[TestObj]
      assertEquals(0, ts1.size())
      expectThrows(classOf[ClassCastException], ts1.add(new TestObj(111)))
    }

    when("compliant-asinstanceofs").
    it("should throw exceptions on access outside bound on views") {
      val l = asJavaCollection(Set(2, 3, 6))
      val ts = treeSetTestFactory.empty[Int]
      ts.addAll(l)

      val hs1 = ts.headSet(5, true)
      assertTrue(hs1.add(4))
      assertTrue(hs1.add(5))
      expectThrows(classOf[IllegalArgumentException], hs1.add(6))

      ts.clear()
      ts.addAll(l)

      val hs2 = ts.headSet(5, false)
      assertTrue(hs2.add(4))
      expectThrows(classOf[IllegalArgumentException], hs2.add(5))

      ts.clear()
      ts.addAll(l)

      val ts1 = ts.tailSet(1, true)
      assertTrue(ts1.add(7))
      assertTrue(ts1.add(1))
      expectThrows(classOf[IllegalArgumentException], ts1.add(0))

      ts.clear()
      ts.addAll(l)

      val ts2 = ts.tailSet(1, false)
      assertTrue(ts2.add(7))
      expectThrows(classOf[IllegalArgumentException], ts2.add(1))

      ts.clear()
      ts.addAll(l)

      val ss1 = ts.subSet(1, true, 5, true)
      assertTrue(ss1.add(4))
      assertTrue(ss1.add(1))
      expectThrows(classOf[IllegalArgumentException], ss1.add(0))
      assertTrue(ss1.add(5))
      expectThrows(classOf[IllegalArgumentException], ss1.add(6))

      ts.clear()
      ts.addAll(l)

      val ss2 = ts.subSet(1, false, 5, false)
      assertTrue(ss2.add(4))
      expectThrows(classOf[IllegalArgumentException], ss2.add(1))
      expectThrows(classOf[IllegalArgumentException], ss2.add(5))
    }
  }
}
