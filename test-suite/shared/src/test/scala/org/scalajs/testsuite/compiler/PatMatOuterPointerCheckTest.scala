package org.scalajs.testsuite.compiler

import org.junit.Test
import org.junit.Assert._

class PatMatOuterPointerCheckTest {
  import PatMatOuterPointerCheckTest._

  @Test def testPatMatOuterPointerCheck(): Unit = {
    assertEquals(1, (new A).call())
  }
}

object PatMatOuterPointerCheckTest {
  class A {
    def call(): Int = f(Foo.B(1))

    private def f(x: Foo): Int = x match {
      case Foo.B(x) => x
    }

    sealed abstract class Foo

    object Foo {
      case class B(x: Int) extends Foo
    }
  }
}
