/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util

import org.junit.Test

import java.{util => ju}

import scala.reflect.ClassTag

class ArrayListTest extends AbstractListTest {

  override def factory: AbstractListFactory = new ArrayListFactory

  @Test def `should_not_fail_with_pre-allocation_methods`(): Unit = {
    // note that these methods become no ops in js
    val al = new ju.ArrayList[String]
    al.ensureCapacity(0)
    al.ensureCapacity(34)
    al.trimToSize()
  }
}

class ArrayListFactory extends AbstractListFactory {
  override def implementationName: String =
    "java.util.ArrayList"

  override def empty[E: ClassTag]: ju.ArrayList[E] =
    new ju.ArrayList[E]
}
