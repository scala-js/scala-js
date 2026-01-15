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

/** All doc-comments marked as "MDN" are by Mozilla Contributors,
 *  distributed under the Creative Commons Attribution-ShareAlike license from
 *  https://developer.mozilla.org/en-US/docs/Web/Reference/API
 */
package scala.scalajs.js

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** Creates a JavaScript Date instance that represents a single moment in time.
 *  Date objects are based on a time value that is the number of milliseconds
 *  since 1 January, 1970 UTC.
 *
 *  MDN
 *
 *  @note
 *   `js.Date` objects can represent an *invalid date*, for example, if they
 *   are constructed from a `String` that cannot be parsed as a date. Most
 *   methods of such a `js.Date` will return `NaN` (for those returning a
 *   `Double`) or other invalid values.
 */
@js.native
@JSGlobal
class Date extends js.Object {

  def this(value: Double) = this()
  def this(value: String) = this()

  def this(year: Int, month: Int, date: Int = 1, hours: Int = 0,
      minutes: Int = 0, seconds: Int = 0, ms: Int = 0) = this()

  def toDateString(): String = js.native
  def toTimeString(): String = js.native
  def toLocaleDateString(): String = js.native
  def toLocaleTimeString(): String = js.native

  override def valueOf(): Double = js.native

  /** Returns the numeric value of the specified date as the number of
   *  milliseconds since January 1, 1970, 00:00:00 UTC.
   *  (Negative values are returned for prior times).
   *
   *  MDN
   */
  def getTime(): Double = js.native

  /** Returns the year (4 digits for 4-digit years) of the specified date according to local time.
   *
   *  MDN
   */
  def getFullYear(): Double = js.native

  /** Returns the year (4 digits for 4-digit years) in the specified date according to universal time.
   *
   *  MDN
   */
  def getUTCFullYear(): Double = js.native

  /** Returns the month (0-11) in the specified date according to local time.
   *
   *  MDN
   */
  def getMonth(): Double = js.native

  /** Returns the month (0-11) in the specified date according to universal time.
   *
   *  MDN
   */
  def getUTCMonth(): Double = js.native

  /** Returns the day of the month (1-31) for the specified date according to local time.
   *
   *  MDN
   */
  def getDate(): Double = js.native

  /** Returns the day (date) of the month (1-31) in the specified date according to universal time.
   *
   *  MDN
   */
  def getUTCDate(): Double = js.native

  /** Returns the day of the week (0-6) for the specified date according to local time.
   *
   *  MDN
   */
  def getDay(): Double = js.native

  /** Returns the day of the week (0-6) in the specified date according to universal time.
   *  MDN
   */
  def getUTCDay(): Double = js.native

  /** Returns the hour (0-23) in the specified date according to local time.
   *
   *  MDN
   */
  def getHours(): Double = js.native

  /** Returns the hours (0-23) in the specified date according to universal time.
   *
   *  MDN
   */
  def getUTCHours(): Double = js.native

  /** Returns the minutes (0-59) in the specified date according to local time.
   *
   *  MDN
   */
  def getMinutes(): Double = js.native

  /** Returns the minutes (0-59) in the specified date according to universal time.
   *
   *  MDN
   */
  def getUTCMinutes(): Double = js.native

  /** Returns the seconds (0-59) in the specified date according to local time.
   *
   *  MDN
   */
  def getSeconds(): Double = js.native

  /** Returns the seconds (0-59) in the specified date according to universal time.
   *
   *  MDN
   */
  def getUTCSeconds(): Double = js.native

  /** Returns the milliseconds (0-999) in the specified date according to local time.
   *
   *  MDN
   */
  def getMilliseconds(): Double = js.native

  /** Returns the milliseconds (0-999) in the specified date according to universal time.
   *
   *  MDN
   */
  def getUTCMilliseconds(): Double = js.native

  /** Returns the time-zone offset in minutes for the current locale.
   *
   *  MDN
   */
  def getTimezoneOffset(): Double = js.native

  def setTime(time: Double): Unit = js.native
  def setMilliseconds(ms: Double): Unit = js.native
  def setUTCMilliseconds(ms: Double): Unit = js.native
  def setSeconds(sec: Double, ms: Double = getMilliseconds()): Unit = js.native
  def setUTCSeconds(sec: Double,
      ms: Double = getMilliseconds()): Unit = js.native
  def setMinutes(min: Double, sec: Double = getSeconds(),
      ms: Double = getMilliseconds()): Unit = js.native
  def setUTCMinutes(min: Double, sec: Double = getSeconds(),
      ms: Double = getMilliseconds()): Unit = js.native
  def setHours(hours: Double, min: Double = getMinutes(),
      sec: Double = getSeconds(),
      ms: Double = getMilliseconds()): Unit = js.native
  def setUTCHours(hours: Double, min: Double = getMinutes(),
      sec: Double = getSeconds(),
      ms: Double = getMilliseconds()): Unit = js.native

  def setDate(date: Double): Unit = js.native
  def setUTCDate(date: Double): Unit = js.native
  def setMonth(month: Double, date: Double = getDate()): Unit = js.native
  def setUTCMonth(month: Double, date: Double = getDate()): Unit = js.native
  def setFullYear(year: Double, month: Double = getMonth(),
      date: Double = getDate()): Unit = js.native
  def setUTCFullYear(year: Double, month: Double = getMonth(),
      date: Double = getDate()): Unit = js.native

  def toUTCString(): String = js.native
  def toISOString(): String = js.native
  def toJSON(key: Any): String = js.native
  def toJSON(): String = js.native
}

/** Factory for [[js.Date]] objects. */
@js.native
@JSGlobal
object Date extends js.Object {
  def apply(): String = js.native

  /** Parses a string representation of a date and returns the number of
   *  milliseconds since 1 January, 1970, 00:00:00, local time.
   *
   *  The parse method takes a date string (such as "Dec 25, 1995") and returns
   *  the number of milliseconds since January 1, 1970, 00:00:00 UTC. The local
   *  time zone is used to interpret arguments that do not contain time zone
   *  information. This function is useful for setting date values based on
   *  string values, for example in conjunction with the setTime() method and
   *  the Date object.
   *
   *  Given a string representing a time, parse returns the time value. It
   *  accepts the RFC2822 / IETF date syntax (RFC2822 Section 3.3), e.g.
   *  "Mon, 25 Dec 1995 13:30:00 GMT". It understands the continental US time-
   *  zone abbreviations, but for general use, use a time-zone offset, for
   *  example, "Mon, 25 Dec 1995 13:30:00 +0430" (4 hours, 30 minutes east of
   *  the Greenwich meridian). If you do not specify a time zone, the local time
   *  zone is assumed. GMT and UTC are considered equivalent.
   *
   *  MDN
   */
  def parse(s: String): Double = js.native

  def UTC(year: Int, month: Int, date: Int = 1, hours: Int = 0,
      minutes: Int = 0, seconds: Int = 0, ms: Int = 0): Double = js.native

  /** Returns the numeric value corresponding to the current time - the number
   *  of milliseconds elapsed since 1 January 1970 00:00:00 UTC.
   *
   *  MDN
   */
  def now(): Double = js.native
}
