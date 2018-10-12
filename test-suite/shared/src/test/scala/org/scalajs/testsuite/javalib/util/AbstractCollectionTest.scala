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

class AbstractCollectionTest extends CollectionTest {
  def factory: AbstractCollectionFactory = new AbstractCollectionFactory
}

class AbstractCollectionFactory extends CollectionFactory {

  override def implementationName: String =
    "java.util.AbstractCollection"

  override def empty[E: ClassTag]: ju.AbstractCollection[E] = {
    // inefficient but simple for debugging implementation of AbstractCollection
    new ju.AbstractCollection[E] {

      private val inner = scala.collection.mutable.Set.empty[Box[E]]

      override def add(elem: E): Boolean = {
        val canAdd = !inner(Box(elem))
        if (canAdd)
          inner += Box(elem)
        canAdd
      }

      def size(): Int =
        inner.size

      override def iterator(): ju.Iterator[E] = {
        new ju.Iterator[E] {
          val innerIter = inner.iterator

          var last: Option[E] = None

          def next(): E = {
            val elem = innerIter.next().inner
            last = Option(elem)
            elem
          }

          override def remove(): Unit = {
            last match {
              case Some(elem) =>
                inner -= Box(elem)
                last = None
              case None =>
                throw new IllegalStateException()
            }
          }

          def hasNext: Boolean = {
            innerIter.hasNext
          }
        }
      }
    }
  }

}
