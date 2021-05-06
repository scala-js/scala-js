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

import org.junit.Test
import org.junit.Assert._

import java.{util => ju}
import java.util.function.Consumer

import org.scalajs.testsuite.utils.AssertThrows.assertThrows

class IteratorTest {
  @Test def testRemove(): Unit = {
    val iter = new ju.Iterator[String] {
      def hasNext(): Boolean = true
      def next(): String = "foo"
    }

    assertThrows(classOf[UnsupportedOperationException], iter.remove())
    iter.next()
    assertThrows(classOf[UnsupportedOperationException], iter.remove())
  }

  @Test def testForEachRemaining(): Unit = {
    val elems = Array("one", "two", "three", "four")
    val elemsList = elems.toList

    class Iter extends ju.Iterator[String] {
      private var index = 0

      def hasNext(): Boolean = index < elems.length

      def next(): String = {
        if (!hasNext())
          throw new NoSuchElementException
        index += 1
        elems(index - 1)
      }
    }

    // from scratch
    val iter1 = new Iter
    val builder1 = List.newBuilder[String]
    iter1.forEachRemaining(new Consumer[String] {
      def accept(elem: String): Unit =
        builder1 += elem
    })
    assertEquals(elemsList, builder1.result())

    // after some calls to next()
    val iter2 = new Iter
    iter2.next()
    iter2.next()
    val builder2 = List.newBuilder[String]
    iter2.forEachRemaining(new Consumer[String] {
      def accept(elem: String): Unit =
        builder2 += elem
    })
    assertEquals(elemsList.drop(2), builder2.result())
  }
}
