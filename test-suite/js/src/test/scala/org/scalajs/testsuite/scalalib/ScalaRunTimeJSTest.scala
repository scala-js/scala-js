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

package org.scalajs.testsuite.scalalib

import org.junit.Test
import org.junit.Assert.assertFalse

import scala.scalajs.js

class ScalaRunTimeJSTest {

  @Test def ScalaRunTime_isArray_should_not_fail_with_JS_objects(): Unit = {
    def isScalaArray(x: Any): Boolean = {
      x match {
        case _: Array[_] => true
        case _           => false
      }
    }

    assertFalse(isScalaArray(js.Array(1, 2, 3)))
    assertFalse(isScalaArray(new js.RegExp("abc")))
    assertFalse(isScalaArray(js.Dynamic.literal()))
    assertFalse(isScalaArray(((x: Int) => x + 1): js.Function1[Int, Int]))
  }

}
