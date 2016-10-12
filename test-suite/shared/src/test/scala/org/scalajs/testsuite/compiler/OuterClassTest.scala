package org.scalajs.testsuite.compiler

import org.junit.Test
import org.junit.Assert._

/* This test only works with 2.12.0-RC2 onwards. With previous versions of
 * Scala, it suffers from #2382.
 */
class OuterClassTest {

  @Test def `Test code variant 1 from #2382`(): Unit = {
    val b1 = new B1 {}
    val y1 = new b1.Y1
    val z1 = new y1.Z1

    assertEquals(1, b1.a)
    assertEquals(1, z1.z1)
    assertEquals(1, z1.z2)
    assertEquals(2, y1.y)
  }

  @Test def `Test code variant 2 from #2382`(): Unit = {
    val b2 = new B2 {}
    val y2 = new b2.Y2
    val z2 = new y2.Z2

    assertEquals(1, b2.a)
    assertEquals(1, z2.z1)
    assertEquals(2, y2.y)
    assertEquals(2, z2.z2)
  }

  @Test def `Test code variant 3 from #2382`(): Unit = {
    val a3 = new A3 {}
    val y3 = new a3.Y3
    val z3 = new y3.Z3

    assertEquals(1, a3.a)
    assertEquals(1, z3.b)
  }

  @Test def `Test code variant 4 from #2382`(): Unit = {
    val a4 = new A4 {}
    val y4 = new a4.Y4
    val z4 = new y4.Z4

    assertEquals(1, a4.a)
    assertEquals(1, z4.b)
  }

  @Test def `Test code variant 5 from #2382`(): Unit = {
    val a5 = new A5 {}
    val y5 = new a5.Y5
    val z5 = new y5.Z5

    assertEquals(1, a5.a)
    assertEquals(1, z5.b)
    assertEquals(2, z5.c)
    assertEquals(2, z5.d)
    assertEquals(3, z5.e)
    assertEquals(3, z5.f)
  }
}

// Code from issue #2382 variant 1

trait A1 {
  val a: Int = 1
  class X1 {
    def x: Int = a
  }
}

trait B1 extends A1 {
  class Y1 {
    def y: Int = 2
    class Z1 extends X1 {
      def z1: Int = a
      def z2: Int = x
    }
  }
}

// Code from issue #2382 variant 2

trait A2 {
  val a: Int = 1
  class X2
}

trait B2 extends A2 {
  class Y2 {
    def y: Int = 2
    class Z2 extends X2 {
      def z1: Int = a
      def z2: Int = y
    }
  }
}

// Code from issue #2382 variant 2

trait A3 {
  val a: Int = 1
  class X3
  class Y3 {
    class Z3 extends X3 {
      def b: Int = a
    }
  }
}

// Code from issue #2382 variant 4

class A4 {
  val a: Int = 1
  class X4
  class Y4 {
    class Z4 extends X4 {
      def b: Int = a
    }
  }
}

// Code from issue #2382 variant 5

class A5 {
  val a: Int = 1
  trait B5 {
    def c: Int = 2
  }
  trait C5 extends B5
  trait D5 extends C5
  class X5 {
    def e: Int = 3
  }
  trait U5 extends X5
  trait S5 extends U5
  class Y5 extends S5 {
    class Z5 extends X5 with D5 {
      def b: Int = a
      def d: Int = c
      def f: Int = e
    }
  }
}
