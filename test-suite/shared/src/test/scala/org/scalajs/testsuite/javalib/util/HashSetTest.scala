/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import scala.language.implicitConversions

import java.{util => ju}

import scala.reflect.ClassTag

class HashSetTest extends AbstractSetTest {
  def factory: HashSetFactory = new HashSetFactory
}

object HashSetFactory {
  def allFactories: Iterator[HashSetFactory] =
    Iterator(new HashSetFactory) ++ LinkedHashSetFactory.allFactories
}

class HashSetFactory extends AbstractSetFactory {
  def implementationName: String =
    "java.util.HashSet"

  def empty[E: ClassTag]: ju.HashSet[E] =
    new ju.HashSet[E]()

  def allowsNullElement: Boolean = true
}
