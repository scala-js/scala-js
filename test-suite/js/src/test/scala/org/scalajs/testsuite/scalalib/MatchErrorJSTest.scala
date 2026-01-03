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

import scala.scalajs.js

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows

class MatchErrorJSTest {

  @Test def matchErrorWithJSObject_Issue5287(): Unit = {
    val x: Any = js.Array(5, 6)

    val e1 = new MatchError(x)
    assertEquals("5,6 (of a JS class)", e1.getMessage())

    val e2 = assertThrows(classOf[MatchError], {
      x match {
        case x: js.Date => x
      }
    })
    assertEquals("5,6 (of a JS class)", e2.getMessage())
  }

}
