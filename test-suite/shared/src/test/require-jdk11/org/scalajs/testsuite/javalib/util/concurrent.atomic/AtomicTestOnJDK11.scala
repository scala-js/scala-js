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

package org.scalajs.testsuite.javalib.util.concurrent.atomic

import org.junit.Test
import org.junit.Assert._

class AtomicTestOnJDK11 {

  @Test def atomicReferenceCompareAndExchangeTest(): Unit = {
    val atomic = new java.util.concurrent.atomic.AtomicReference(1)

    // doesn't replace if the expected value is incorrect, returns actual value
    assertSame(atomic.compareAndExchange(2, 10), 1)
    assertSame(atomic.get(), 1)

    // replaces if the expected value is correct, returns the expected value
    assertSame(atomic.compareAndExchange(1, 10), 1)
    assertSame(atomic.get(), 10)
  }

}
