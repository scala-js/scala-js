package org.scalajs.testsuite.utils

import org.junit.Assume._
import org.junit.BeforeClass
import org.scalajs.testsuite.utils.Platform._

object Requires {

  trait TypedArray {
    @BeforeClass def needsTypedArrays(): Unit =
      assumeTrue("Requires typedArrays", typedArrays)
  }

  trait StrictFloats {
    @BeforeClass def needsTypedArrays(): Unit =
      assumeTrue("Requires strict-floats", hasStrictFloats)
  }

}
