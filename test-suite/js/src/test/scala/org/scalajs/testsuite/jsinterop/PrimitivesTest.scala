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

package org.scalajs.testsuite.jsinterop

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Test

class PrimitivesTest {

  @noinline
  def assertJSEquals(expected: js.Any, actual: js.Any): Unit = {
    assertTrue(s"expected: $expected; but got: $actual",
        js.special.strictEquals(actual, expected))
  }

  @Test def primitivesToJSAny(): Unit = {
    assertJSEquals(false, false)
    assertJSEquals(42, 42.toByte)
    assertJSEquals(42, 42.toShort)
    assertJSEquals(42, 42)
    assertJSEquals(42.0, 42L) // converted to Double!
    assertJSEquals(42.0f, 42.0f)
    assertJSEquals(42.0, 42.0)
  }

  @Test def javaBoxedTypesToJSAny(): Unit = {
    assertJSEquals(false, new java.lang.Boolean(false))
    assertJSEquals(null, null: java.lang.Boolean)

    assertJSEquals(42, new java.lang.Byte(42.toByte))
    assertJSEquals(null, null: java.lang.Byte)

    assertJSEquals(42, new java.lang.Short(42.toShort))
    assertJSEquals(null, null: java.lang.Short)

    assertJSEquals(42, new java.lang.Integer(42))
    assertJSEquals(null, null: java.lang.Integer)

    assertJSEquals(42.0, new java.lang.Long(42L)) // converted to Double!
    assertJSEquals(null, null: java.lang.Long)

    assertJSEquals(42.0f, new java.lang.Float(42.0f))
    assertJSEquals(null, null: java.lang.Float)

    assertJSEquals(42.0, new java.lang.Double(42.0))
    assertJSEquals(null, null: java.lang.Double)
  }

}
