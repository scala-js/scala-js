/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.lang

import org.junit.Test
import org.junit.Assert._

import scala.scalajs.js
import scala.scalajs.testsuite.utils.AssertThrows._

/** Additional tests for java.lang.Object that have to be in a separate
 *  codebase than testSuite to be meaningful.
 *
 *  If moved to testSuite, those tests "fail to fail" due to mass effects
 *  produced by the immensity of the testSuite codebase.
 */
class ObjectTestEx {

  @Test def clone_issue_2010(): Unit = {
    class NotCloneable extends Object {
      override def clone(): NotCloneable =
        super.clone().asInstanceOf[NotCloneable]
    }

    assertThrows(classOf[CloneNotSupportedException], new NotCloneable().clone())

    class SomeCloneable(val x: Int) extends Object with Cloneable {
      override def clone(): SomeCloneable =
        super.clone().asInstanceOf[SomeCloneable]

      @noinline def y(): Int = x + 3
    }

    val o = new SomeCloneable(5)
    val o2 = o.clone()
    assertNotSame(o, o2)
    assertSame(classOf[SomeCloneable], o2.getClass)
    assertEquals(5, o2.x)
    assertEquals(8, o2.y())
  }
}
