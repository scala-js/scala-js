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

import org.junit.Assume._
import org.junit.BeforeClass
import org.scalajs.testsuite.utils.Platform._

object Requires {

  trait TypedArray {
    @BeforeClass def needsTypedArrays(): Unit =
      assumeTrue("Assumed typed arrays are supported", typedArrays)
  }

  trait StrictFloats {
    @BeforeClass def needsTypedArrays(): Unit =
      assumeTrue("Assumed strict floats", hasStrictFloats)
  }

}
