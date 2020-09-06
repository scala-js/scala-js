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

import java.util.function.Supplier

import org.junit.Assert._
import org.junit.Test

class SupplierTest {
  import SupplierTest._

  @Test def get(): Unit = {
    val supplier: Supplier[String] = makeSupplier("scala")

    assertEquals("scala", supplier.get())
  }
}

object SupplierTest {
  def makeSupplier[T](f: => T): Supplier[T] = {
    new Supplier[T] {
      def get(): T = f
    }
  }
}
