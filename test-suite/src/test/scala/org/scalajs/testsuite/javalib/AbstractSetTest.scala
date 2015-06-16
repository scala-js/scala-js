package org.scalajs.testsuite.javalib

import java.{util => ju}

abstract class AbstractSetTest[F <: AbstractSetFactory](val factory: F) extends SetTest {

  describe(factory.implementationName) {
    testApi()
  }

  def testApi(): Unit = {
    testSetApi(factory)
  }
}

object AbstractSetFactory {
  def allFactories: Iterator[AbstractSetFactory] =
    HashSetFactory.allFactories
}

trait AbstractSetFactory extends SetFactory {
  def empty[E]: ju.AbstractSet[E]
}
