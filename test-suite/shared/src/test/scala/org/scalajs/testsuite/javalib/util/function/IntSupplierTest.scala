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

import java.util.function.IntSupplier

import org.junit.Assert._
import org.junit.Test

class IntSupplierTest {
  import IntSupplierTest._

  @Test def getAsInt(): Unit = {
    assertEquals(Int.MinValue, makeSupplier(Int.MinValue).getAsInt())
    assertEquals(1024, makeSupplier(1024).getAsInt())
    assertEquals(Int.MaxValue, makeSupplier(Int.MaxValue).getAsInt())
  }
}

object IntSupplierTest {
  def makeSupplier(f: => Int): IntSupplier = {
    new IntSupplier {
      def getAsInt(): Int = f
    }
  }
}
