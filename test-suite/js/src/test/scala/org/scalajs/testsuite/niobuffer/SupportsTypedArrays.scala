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

package org.scalajs.testsuite.niobuffer

import org.junit.BeforeClass
import org.junit.Assume._

import org.scalajs.testsuite.utils.Platform

trait SupportsTypedArrays {
  @BeforeClass def assumeThatContextSupportsTypedByteArrays(): Unit = {
    assumeTrue("Assumed typed arrays are supported", Platform.typedArrays)
  }
}
