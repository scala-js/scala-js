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
import org.junit.Assume._
import org.junit.{BeforeClass, Test}
import org.scalajs.testsuite.utils.Platform

import scala.scalajs.js

class DateTest {

  @Test def toLocaleDateString(): Unit = {
    val x = new js.Date(1600000000000L).toLocaleDateString("en-US")
    assertEquals("9/13/2020", x)

    val y = new js.Date(1600000000000L).toLocaleDateString("en-US", new js.DateTimeFormatOptions {
      day = "2-digit"
      month = "2-digit"
      year = "2-digit"
      hour12 = false
      hour = "2-digit"
      minute = "2-digit"
      timeZone = "UTC"
    })
    assertEquals("09/13/20, 12:26", y)

    val z = new js.Date(1600000000000L).toLocaleDateString("en-US", new js.DateTimeFormatOptions {
      calendar = "buddhist"
      dayPeriod = "long"
      hour12 = true
      hour = "numeric"
      minute = "numeric"
      timeZone = "UTC"
    })
    assertEquals("9/13/2563 BE, 12:26 in the afternoon", z)

    val w = new js.Date(1600000000000L).toLocaleDateString(new js.Array("en-US"), new js.DateTimeFormatOptions {
      numberingSystem = "latn"
      fractionalSecondDigits = 3
      hour12 = false
      hour = "numeric"
      minute = "numeric"
      second = "numeric"
      timeZone = "UTC"
    })
    assertEquals("9/13/2020, 12:26:40.000", w)
  }

}
