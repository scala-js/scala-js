package org.scalajs.testsuite.javalib.util

import java.{util => ju}

trait DequeTest extends CollectionTest {
  def factory: DequeFactory
}

object DequeFactory {
  def allFactories: Iterator[DequeFactory] =
    ArrayDequeFactory.allFactories
}

trait DequeFactory extends CollectionFactory {
  def empty[E]: ju.Deque[E]
}
