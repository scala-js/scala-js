/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

import java.{util => ju}

object ArrayListTest extends ArrayListTest(new ArrayListFactory)

abstract class ArrayListTest[F <: ArrayListFactory](listFactory: F) extends AbstractListTest(listFactory) {

  override def testApi(): Unit = {

    super.testApi()

    it("should not fail with pre-allocation methods") {
      // note that these methods become no ops in js
      val al = new ju.ArrayList[String]
      al.ensureCapacity(0)
      al.ensureCapacity(34)
      al.trimToSize()
    }
  }

}

class ArrayListFactory extends AbstractListFactory {
  override def implementationName: String =
    "java.util.ArrayList"

  override def empty[E]: ju.ArrayList[E] =
    new ju.ArrayList[E]
}
