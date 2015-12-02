/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import scala.language.implicitConversions

import scala.collection.JavaConversions._
import scala.collection.mutable

import org.scalajs.jasminetest.JasmineTest

import scala.scalajs.runtime.UndefinedBehaviorError

import java.util.ArrayDeque

object ArrayDequeJSTest extends ArrayDequeJSTest(new ArrayDequeJSFactory)

abstract class ArrayDequeJSTest[F <: ArrayDequeJSFactory](listFactory: F)
    extends AbstractCollectionTest(listFactory) {

  override def testApi(): Unit = {

    super.testApi()
  }
}

class ArrayDequeJSFactory extends AbstractCollectionFactory {
  override def implementationName: String =
    "java.util.ArrayDeque"

  override def empty[E]: ArrayDeque[E] =
    new ArrayDeque[E]()
}

object DequeFactory {
  def allFactories: Iterator[CollectionFactory] =
    Iterator(new ArrayDequeJSFactory())
}
