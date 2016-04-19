/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.compiler

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform._

/* General note on the way these tests are written:
 * We leverage the constant folding applied by the Scala compiler to write
 * sound tests. We always perform the same operation, on the same operands,
 * once in a way constant folding understands, and once in a way it doesn't.
 * Since constant folding is performed on the JVM, we know it has the right
 * semantics.
 */
class IntJSTest {

  // final val without type ascription to make sure these are constant-folded
  final val MinVal = Int.MinValue
  final val MaxVal = Int.MaxValue
  final val AlmostMinVal = Int.MinValue + 43
  final val AlmostMaxVal = Int.MaxValue - 36

  @Test def `should_support_%`(): Unit = {
    assumeFalse("Assumed not executing in PhantomJS", executingInPhantomJS) // see #593

    def test(a: Int, b: Int, expected: Int): Unit =
      assertEquals(expected, a % b)

    test(654, 56, 654 % 56)
    test(0, 25, 0 % 25)
    test(-36, 13, -36 % 13)
    test(-55, -6, -55 % -6)

    test(MinVal, 1, MinVal % 1)
    test(MinVal, -1, MinVal % -1)
    test(MaxVal, 1, MaxVal % 1)
    test(MaxVal, -1, MaxVal % -1)

    test(MaxVal, MinVal, MaxVal % MinVal)
    test(MaxVal, MaxVal, MaxVal % MaxVal)
    test(MinVal, MaxVal, MinVal % MaxVal)
    test(MinVal, MinVal, MinVal % MinVal)

    test(AlmostMaxVal, 2, AlmostMaxVal % 2)
    test(AlmostMaxVal, 5, AlmostMaxVal % 5)
    test(AlmostMaxVal, -7, AlmostMaxVal % -7)
    test(AlmostMaxVal, -14, AlmostMaxVal % -14)
    test(AlmostMinVal, 100, AlmostMinVal % 100)
    test(AlmostMaxVal, -123, AlmostMaxVal % -123)
  }
}
