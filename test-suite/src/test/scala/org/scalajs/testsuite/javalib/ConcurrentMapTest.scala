package org.scalajs.testsuite.javalib

import java.{util => ju}

object ConcurrentMapFactory {
  def allFactories: Iterator[ConcurrentMapFactory] =
    ConcurrentHashMapFactory.allFactories
}

trait ConcurrentMapFactory extends MapFactory {
  def empty[K, V]: ju.concurrent.ConcurrentMap[K, V]
}
