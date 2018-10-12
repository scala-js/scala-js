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

package org.scalajs.testsuite.utils

import org.junit.Test
import org.junit.Assert._
import org.junit.Assume._

class PlatformTest {
  import Platform._

  @Test def typedarray_implies_areTypedArraysSupportedshould(): Unit = {
    // Note that if we don't have the tag set we can't say anything
    // on the typed arrays support.
    assumeTrue("Assumed typed arrays", typedArrays)
    assertTrue(areTypedArraysSupported)
  }
}
