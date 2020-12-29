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

import org.junit.Assert._
import org.junit.Test

import scala.scalajs.js

class RegExpTest {
  @Test def execNoGroup(): Unit = {
    val result = js.RegExp("([0-9]{4})-([0-9]{2})-([0-9]{2})")
      .exec("1992-12-31")

    assertEquals(4, result.length)
    assertEquals("1992-12-31", result(0))
    assertEquals("1992", result(1))
    assertEquals("12", result(2))
    assertEquals("31", result(3))
    assertEquals(js.undefined, result(4))
    assertEquals(js.undefined, result.groups)
  }

  @Test def execWithGroupNoMatch(): Unit = {
    val result = js.RegExp("(?<year>[0-9]{4})-(?<month>[0-9]{2})-(?<day>[0-9]{2})")
      .exec("abc")

    assertEquals(null, result)
  }

  @Test def execWithGroupMatch(): Unit = {
    val result = js.RegExp("(?<year>[0-9]{4})-(?<month>[0-9]{2})-(?<day>[0-9]{2})")
      .exec("1992-12-31")

    assertEquals(4, result.length)
    assertEquals("1992-12-31", result(0))
    assertEquals("1992", result(1))
    assertEquals("12", result(2))
    assertEquals("31", result(3))
    assertEquals(js.undefined, result(4))

    val groups = result.groups.get
    assertEquals(3, js.Object.entries(groups).length)
    assertEquals("1992", groups("year"))
    assertEquals("12", groups("month"))
    assertEquals("31", groups("day"))
  }

  @Test def execWithOptGroupMatch(): Unit = {
    val result = js.RegExp("foo(?<prop>bar)?baz")
      .exec("foobaz")

    assertEquals(2, result.length)
    assertEquals("foobaz", result(0))
    assertEquals(js.undefined, result(1))

    val groups = result.groups.get
    assertEquals(1, js.Object.entries(groups).length)
    assertEquals(js.undefined, groups("prop"))
  }
}
