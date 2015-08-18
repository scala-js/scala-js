/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

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
