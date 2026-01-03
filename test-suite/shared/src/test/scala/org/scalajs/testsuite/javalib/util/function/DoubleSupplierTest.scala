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

package org.scalajs.testsuite.javalib.util.function

import java.util.function.DoubleSupplier

import org.junit.Assert._
import org.junit.Test

class DoubleSupplierTest {
  import DoubleSupplierTest._

  @Test def getAsDouble(): Unit =
    assertEquals(1.234d, makeSupplier(1.234d).getAsDouble(), 0.0d)
}

object DoubleSupplierTest {
  def makeSupplier(f: => Double): DoubleSupplier = {
    new DoubleSupplier {
      def getAsDouble(): Double = f
    }
  }
}
