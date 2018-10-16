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

package org.scalajs.testsuite.library

import scala.util.control.NonFatal

import org.junit.Assert._
import org.junit.Test

import scala.scalajs.runtime.UndefinedBehaviorError

class UndefinedBehaviorErrorTest {

  @Test def ubeIsAFatalError(): Unit = {
    val error: Throwable = new UndefinedBehaviorError("test")
    assertFalse(NonFatal(error))
    assertTrue(error match {
      case NonFatal(_) => false
      case _           => true
    })
  }

}
