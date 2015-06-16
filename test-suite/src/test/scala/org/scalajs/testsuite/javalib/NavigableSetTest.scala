package org.scalajs.testsuite.javalib

import java.{util => ju}

object NavigableSetFactory {
  def allFactories: Iterator[NavigableSetFactory] =
    ConcurrentSkipListSetFactory.allFactories
}

trait NavigableSetFactory extends SetFactory {
  def empty[E]: ju.NavigableSet[E]
}
