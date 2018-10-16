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

import scala.scalajs.js

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.Platform._

/** Tests the compiler re-patching of native longs to
 *  scala.scalajs.runtime.Long
 *  see org.scalajs.testsuite.jsinterop.RuntimeLongTest
 *  for a test of the implementation itself
 */
class LongJSTest {
  @Test def `should_convert_to_js.Any`(): Unit = {
    val x = 5: js.Any
    assertEquals(x, 5L: js.Any)
  }

  @Test def should_correctly_implement_asInstanceOf_Longs_negative(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)

    val dyn: Any = 5L
    assertThrows(classOf[Exception], dyn.asInstanceOf[Int])
  }

}
