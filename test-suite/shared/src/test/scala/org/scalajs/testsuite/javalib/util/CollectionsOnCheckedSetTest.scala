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
