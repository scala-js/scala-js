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

import java.util.function.LongSupplier

import org.junit.Assert._
import org.junit.Test

class LongSupplierTest {
  import LongSupplierTest._

  @Test def getAsLong(): Unit = {
    assertEquals(Long.MinValue, makeSupplier(Long.MinValue).getAsLong())
    assertEquals(1024L, makeSupplier(1024L).getAsLong())
    assertEquals(Long.MaxValue, makeSupplier(Long.MaxValue).getAsLong())
  }
}

object LongSupplierTest {
  def makeSupplier(f: => Long): LongSupplier = {
    new LongSupplier {
      def getAsLong(): Long = f
    }
  }
}
