/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
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
