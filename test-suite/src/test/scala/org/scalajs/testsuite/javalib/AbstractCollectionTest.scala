package org.scalajs.testsuite.javalib

import java.{util => ju}

object AbstractCollectionTest
    extends AbstractCollectionTest(new AbstractCollectionFactory)

abstract class AbstractCollectionTest[F <: AbstractCollectionFactory](factory: F)
    extends CollectionTest {

  describe(factory.implementationName) {
    testApi()
  }

  def testApi(): Unit = {
    testCollectionApi(factory)
  }
}

class AbstractCollectionFactory extends CollectionFactory {

  override def implementationName: String =
    "java.util.AbstractCollection"

  override def empty[E]: ju.AbstractCollection[E] = {
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
          val innerIter = inner.seq.iterator

          var last: Option[E] = None

          def next(): E = {
            val elem = innerIter.next().inner
            last = Option(elem)
            elem
          }

          def remove(): Unit = {
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
