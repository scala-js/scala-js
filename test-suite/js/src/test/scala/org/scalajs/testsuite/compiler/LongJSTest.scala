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

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform._

/** Tests the re-patching of native longs */
class LongJSTest {
  @Test def longToJSAny(): Unit = {
    val x = 5: js.Any
    assertEquals(x, 5L: js.Any)
  }

  @Test def asInstanceOfIntWithLongAnyThrows(): Unit = {
    assumeTrue("Assumed compliant asInstanceOf", hasCompliantAsInstanceOfs)

    val dyn: Any = 5L
    assertThrows(classOf[Exception], dyn.asInstanceOf[Int])
  }

}
