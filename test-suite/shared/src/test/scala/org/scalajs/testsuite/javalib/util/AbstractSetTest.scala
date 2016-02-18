/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import java.{util => ju}

import scala.reflect.ClassTag

abstract class AbstractSetTest extends SetTest {
  def factory: AbstractSetFactory
}

object AbstractSetFactory {
  def allFactories: Iterator[AbstractSetFactory] =
    HashSetFactory.allFactories
}

trait AbstractSetFactory extends SetFactory {
  def empty[E: ClassTag]: ju.AbstractSet[E]
}
