package org.scalajs.testsuite.javalib.util

import java.{util => ju}

import scala.reflect.ClassTag

trait DequeTest extends CollectionTest {
  def factory: DequeFactory
}

object DequeFactory {
  def allFactories: Iterator[DequeFactory] =
    ArrayDequeFactory.allFactories
}

trait DequeFactory extends CollectionFactory {
  def empty[E: ClassTag]: ju.Deque[E]
}
