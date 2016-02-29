package org.scalajs.testsuite.utils

import org.junit.Assume._
import org.junit.BeforeClass
import org.scalajs.testsuite.utils.Platform._

trait TestClassRequiresTypedArray {
  @BeforeClass def needsTypedArrays(): Unit = {
    assumeTrue("Test class requires typedArrays", typedArrays)
  }
}
