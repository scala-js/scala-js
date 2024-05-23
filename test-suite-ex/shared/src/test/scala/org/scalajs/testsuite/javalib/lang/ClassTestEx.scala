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

import org.scalajs.testsuite.utils.Platform._

class ClassTestEx {
  import ClassTestEx._

  /** Tests for `java.lang.Class.getSuperclass()`.
   *
   *  The mere fact of calling `getSuperclass()` anywhere in the code base
   *  triggers the addition of `parentData`, and has some impact on the
   *  `$TypeData` class. We keep those in the test-suite-ex so that we do not
   *  pollute the whole test suite with those.
   */
  @Test def getSuperclass(): Unit = {
    assumeFalse("Not supported on WebAssembly", executingInWebAssembly)

    def test(parent: Class[_], child: Class[_]): Unit =
      assertSame(parent, child.getSuperclass())

    test(null, classOf[AnyRef])

    test(classOf[SomeParentClass], classOf[SomeChildClass])
    test(classOf[AnyRef], classOf[String])
    test(classOf[Number], classOf[Integer])

    test(null, classOf[Seq[_]])

    test(classOf[Object], classOf[Array[Object]])
    test(classOf[Object], classOf[Array[Int]])
    test(classOf[Object], classOf[Array[List[_]]])
    test(classOf[Object], classOf[Array[Seq[_]]])
    test(classOf[Object], classOf[Array[Array[Object]]])
    test(classOf[Object], classOf[Array[Array[Int]]])
    test(classOf[Object], classOf[Array[Array[List[_]]]])
  }

  @Test def getSuperclassWhenParentClassDataIsNotDirectlyAccessed_Issue1489(): Unit = {
    assumeFalse("Not supported on WebAssembly", executingInWebAssembly)

    assertEquals("org.scalajs.testsuite.javalib.lang.ClassTestEx$ParentClassWhoseDataIsNotAccessedDirectly",
        classOf[ChildClassWhoseDataIsAccessedDirectly].getSuperclass.getName)
  }
}

object ClassTestEx {
  class SomeParentClass
  class SomeChildClass extends SomeParentClass

  class ParentClassWhoseDataIsNotAccessedDirectly
  class ChildClassWhoseDataIsAccessedDirectly extends ParentClassWhoseDataIsNotAccessedDirectly
}
