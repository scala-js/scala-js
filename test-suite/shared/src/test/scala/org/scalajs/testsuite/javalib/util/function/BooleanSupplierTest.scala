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

import java.util.function.BooleanSupplier

import org.junit.Assert._
import org.junit.Test

class BooleanSupplierTest {
  import BooleanSupplierTest._

  @Test def getAsBoolean(): Unit = {
    assertEquals(true, makeSupplier(true).getAsBoolean())
    assertEquals(false, makeSupplier(false).getAsBoolean())
  }
}

object BooleanSupplierTest {
  def makeSupplier(f: => Boolean): BooleanSupplier = {
    new BooleanSupplier {
      def getAsBoolean(): Boolean = f
    }
  }
}
