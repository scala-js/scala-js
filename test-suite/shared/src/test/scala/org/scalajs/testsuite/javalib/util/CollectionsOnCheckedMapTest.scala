/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import java.{util => ju}

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

import scala.collection.JavaConversions._

import scala.reflect.ClassTag

trait CollectionsOnCheckedMapTest extends CollectionsOnMapsTest {

  def originalFactory: MapFactory

  def factory: MapFactory = {
    new MapFactory {
      override def implementationName: String =
        s"checkedMap(${originalFactory.implementationName})"

      def empty[K, V](implicit kct: ClassTag[K], vct: ClassTag[V]): ju.Map[K, V] = {
        ju.Collections.checkedMap(originalFactory.empty[K, V],
            kct.runtimeClass.asInstanceOf[Class[K]],
            vct.runtimeClass.asInstanceOf[Class[V]])
      }

      override def allowsNullKeys: Boolean =
        originalFactory.allowsNullKeys

      override def allowsNullValues: Boolean =
        originalFactory.allowsNullValues
    }
  }

  @Test def testCheckedMap(): Unit = {
    assertNull(superMap().put(new C, new C))
  }

  @Test def testCheckedMapBadInputs(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)

    expectThrows(classOf[ClassCastException], superMap().put(new A, new C))
    expectThrows(classOf[ClassCastException], superMap().put(new C, new A))
    expectThrows(classOf[ClassCastException], superMap().put(new A, new A))

    def singletonMap(): ju.Map[A, A] = {
      val m = factory.empty[B, B]
      m.put(new C, new C)
      m.asInstanceOf[ju.Map[A, A]]
    }
    expectThrows(classOf[ClassCastException],
      singletonMap().entrySet().head.setValue(new A))
  }

  private def superMap(): ju.Map[A, A] =
    factory.empty[B, B].asInstanceOf[ju.Map[A, A]]
}

trait CollectionsOnCheckedSortedMapTest extends CollectionsOnSortedMapsTest {

  def originalFactory: SortedMapFactory

  def factory: SortedMapFactory = {
    new SortedMapFactory {
      override def implementationName: String =
        s"checkedSortedMap(${originalFactory.implementationName})"

      def empty[K, V](implicit kct: ClassTag[K], vct: ClassTag[V]): ju.SortedMap[K, V] = {
        ju.Collections.checkedSortedMap(originalFactory.empty[K, V],
            kct.runtimeClass.asInstanceOf[Class[K]],
            vct.runtimeClass.asInstanceOf[Class[V]])
      }

      override def allowsNullKeys: Boolean =
        originalFactory.allowsNullKeys

      override def allowsNullValues: Boolean =
        originalFactory.allowsNullValues
    }
  }

  @Test def testCheckedMap(): Unit = {
    assertNull(superMap().put(new C, new C))
  }

  @Test def testCheckedMapBadInputs(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)

    expectThrows(classOf[ClassCastException], superMap().put(new A, new C))
    expectThrows(classOf[ClassCastException], superMap().put(new C, new A))
    expectThrows(classOf[ClassCastException], superMap().put(new A, new A))

    def singletonMap(): ju.Map[A, A] = {
      val m = factory.empty[B, B]
      m.put(new C, new C)
      m.asInstanceOf[ju.Map[A, A]]
    }
    expectThrows(classOf[ClassCastException],
        singletonMap().entrySet().head.setValue(new A))
  }

  private def superMap(): ju.Map[A, A] =
    factory.empty[B, B].asInstanceOf[ju.Map[A, A]]
}

class CollectionsOnCheckedMapOnHashMapTest
    extends CollectionsOnCheckedMapTest {
  def originalFactory: MapFactory = new HashMapFactory
}

class CollectionsOnCheckedMapOnLinkedHashMapInsertionOrderTest
    extends CollectionsOnCheckedMapTest {
  def originalFactory: MapFactory = new LinkedHashMapFactory(false, None)
}

class CollectionsOnCheckedMapOnLinkedHashMapInsertionOrderWithLimitTest
    extends CollectionsOnCheckedMapTest {
  def originalFactory: MapFactory = new LinkedHashMapFactory(false, Some(50))
}

class CollectionsOnCheckedMapOnLinkedHashMapAccessOrderTest
    extends CollectionsOnCheckedMapTest {
  def originalFactory: MapFactory = new LinkedHashMapFactory(true, None)
}

class CollectionsOnOnCheckedMapOnLinkedHashMapAccessOrderWithLimitTest
    extends CollectionsOnCheckedMapTest {
  def originalFactory: MapFactory = new LinkedHashMapFactory(true, Some(50))
}
