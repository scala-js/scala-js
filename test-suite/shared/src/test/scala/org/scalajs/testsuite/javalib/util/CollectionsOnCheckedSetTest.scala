/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import java.{util => ju}

import scala.reflect.ClassTag

trait CollectionsOnCheckedSetTest extends CollectionsOnSetsTest {

  def originalFactory: SetFactory

  def factory: SetFactory = {
    new SetFactory {
      override def implementationName: String =
        s"checkedSet(${originalFactory.implementationName})"

      override def empty[E](implicit ct: ClassTag[E]): ju.Set[E] = {
        ju.Collections.checkedSet(originalFactory.empty[E],
            ct.runtimeClass.asInstanceOf[Class[E]])
      }

      def allowsNullElement: Boolean =
        originalFactory.allowsNullElement
    }
  }
}

trait CollectionsOnCheckedSortedSetTest extends CollectionsOnSortedSetsTest {

  def originalFactory: SortedSetFactory

  def factory: SortedSetFactory = {
    new SortedSetFactory {
      override def implementationName: String =
        s"checkedSortedSet(${originalFactory.implementationName})"

      override def empty[E](implicit ct: ClassTag[E]): ju.SortedSet[E] = {
        ju.Collections.checkedSortedSet(originalFactory.empty[E],
            ct.runtimeClass.asInstanceOf[Class[E]])
      }

      def allowsNullElement: Boolean =
        originalFactory.allowsNullElement
    }
  }
}

class CollectionsOnCheckedSetHashSetFactoryTest
    extends CollectionsOnCheckedSetTest {
  def originalFactory: SetFactory = new HashSetFactory
}

class CollectionsOnCheckedSetCollectionLinkedHashSetFactoryTest
    extends CollectionsOnCheckedSetTest {
  def originalFactory: SetFactory = new LinkedHashSetFactory
}

class CollectionsOnCheckedSetCollectionConcurrentSkipListSetFactoryTest
    extends CollectionsOnCheckedSetTest {
  def originalFactory: SetFactory = new concurrent.ConcurrentSkipListSetFactory
}
