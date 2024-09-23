/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.testsuite.javalib.lang

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import scala.scalajs.js

import org.scalajs.testsuite.utils.Platform._

class ClassJSTestEx {
  import ClassJSTestEx._

  /** Tests for `java.lang.Class.getSuperclass()` on JS classes and traits. */
  @Test def getSuperclass(): Unit = {
    def test(parent: Class[_], child: Class[_]): Unit =
      assertSame(parent, child.getSuperclass())

    test(null, classOf[js.Any])

    test(classOf[Object], classOf[js.Object])

    test(classOf[SomeParentClass], classOf[SomeChildClass])
    test(classOf[AnyRef], classOf[String])
    test(classOf[Number], classOf[Integer])

    test(null, classOf[SomeParentTrait])
    test(null, classOf[SomeChildTrait])

    test(classOf[Object], classOf[Array[js.Object]])
    test(classOf[Object], classOf[Array[SomeChildClass]])
    test(classOf[Object], classOf[Array[SomeChildTrait]])
    test(classOf[Object], classOf[Array[Array[SomeChildClass]]])
  }

  @Test def getSuperclassWhenParentClassDataIsNotDirectlyAccessed_Issue1489(): Unit = {
    assertEquals(
        "org.scalajs.testsuite.javalib.lang.ClassJSTestEx$ParentClassWhoseDataIsNotAccessedDirectly",
        classOf[ChildClassWhoseDataIsAccessedDirectly].getSuperclass().getName())
  }
}

object ClassJSTestEx {
  class SomeParentClass extends js.Object
  class SomeChildClass extends SomeParentClass

  class ParentClassWhoseDataIsNotAccessedDirectly extends js.Object
  class ChildClassWhoseDataIsAccessedDirectly extends ParentClassWhoseDataIsNotAccessedDirectly

  trait SomeParentTrait extends js.Object
  trait SomeChildTrait extends SomeParentTrait
}
