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

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Test

class WrappedDictionaryToTest {

  @Test def to_T(): Unit = {
    val dict = js.Dictionary("a" -> "a", "b" -> 6, "e" -> js.undefined)
    val list = dict.to(List)
    assertEquals(3, list.size)
  }

}
