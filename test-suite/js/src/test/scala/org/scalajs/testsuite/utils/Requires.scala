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
