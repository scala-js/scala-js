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

package org.scalajs.testsuite.compiler

import org.junit.Test
import org.junit.Assert._

class BooleanTest {
  @noinline def hideFromOptimizer(x: Boolean): Boolean = x

  // See #220, #409
  @noinline def anyIsBoolean(x: Any): Boolean = x.isInstanceOf[Boolean]

  @Test def nonShortCircuitingAnd(): Unit = {
    @inline def test(expected: Boolean, x: Boolean, y: Boolean): Unit = {
      assertEquals(expected, x & y)
      assertEquals(expected, hideFromOptimizer(x) & y)
      assertEquals(expected, x & hideFromOptimizer(y))
      assertEquals(expected, hideFromOptimizer(x) & hideFromOptimizer(y))

      assertTrue(anyIsBoolean(hideFromOptimizer(x) & hideFromOptimizer(y)))
    }

    test(false, false, false)
    test(false, false, true)
    test(false, true, false)
    test(true, true, true)
  }

  @Test def nonShortCircuitingOr(): Unit = {
    @inline def test(expected: Boolean, x: Boolean, y: Boolean): Unit = {
      assertEquals(expected, x | y)
      assertEquals(expected, hideFromOptimizer(x) | y)
      assertEquals(expected, x | hideFromOptimizer(y))
      assertEquals(expected, hideFromOptimizer(x) | hideFromOptimizer(y))

      assertTrue(anyIsBoolean(hideFromOptimizer(x) | hideFromOptimizer(y)))
    }

    test(false, false, false)
    test(true, false, true)
    test(true, true, false)
    test(true, true, true)
  }

  @Test def xorAkaNotEquals(): Unit = {
    @inline def test(expected: Boolean, x: Boolean, y: Boolean): Unit = {
      assertEquals(expected, x ^ y)
      assertEquals(expected, hideFromOptimizer(x) ^ y)
      assertEquals(expected, x ^ hideFromOptimizer(y))
      assertEquals(expected, hideFromOptimizer(x) ^ hideFromOptimizer(y))

      assertTrue(anyIsBoolean(hideFromOptimizer(x) ^ hideFromOptimizer(y)))

      // != also basically computes xor

      assertEquals(expected, x != y)
      assertEquals(expected, hideFromOptimizer(x) != y)
      assertEquals(expected, x != hideFromOptimizer(y))
      assertEquals(expected, hideFromOptimizer(x) != hideFromOptimizer(y))

      assertTrue(anyIsBoolean(hideFromOptimizer(x) != hideFromOptimizer(y)))
    }

    test(false, false, false)
    test(true, false, true)
    test(true, true, false)
    test(false, true, true)
  }

  @Test def eqEq(): Unit = {
    @inline def test(expected: Boolean, x: Boolean, y: Boolean): Unit = {
      assertEquals(expected, x == y)
      assertEquals(expected, hideFromOptimizer(x) == y)
      assertEquals(expected, x == hideFromOptimizer(y))
      assertEquals(expected, hideFromOptimizer(x) == hideFromOptimizer(y))

      assertTrue(anyIsBoolean(hideFromOptimizer(x) == hideFromOptimizer(y)))
    }

    test(true, false, false)
    test(false, false, true)
    test(false, true, false)
    test(true, true, true)
  }
}
