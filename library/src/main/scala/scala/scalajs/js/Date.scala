/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


/**
 * All doc-comments marked as "MDN" are by Mozilla Contributors,
 * distributed under the Creative Commons Attribution-ShareAlike license from
 * https://developer.mozilla.org/en-US/docs/Web/Reference/API
 */
package scala.scalajs.js

/**
 * Creates a JavaScript Date instance that represents a single moment in time.
 * Date objects are based on a time value that is the number of milliseconds
 * since 1 January, 1970 UTC.
 *
 * MDN
 */
@native
class Date extends Object {

  def this(value: Double) = this()
  def this(value: String) = this()

  def this(year: Int, month: Int, date: Int = 1, hours: Int = 0,
      minutes: Int = 0, seconds: Int = 0, ms: Int = 0) = this()

  def toDateString(): String = native
  def toTimeString(): String = native
  def toLocaleDateString(): String = native
  def toLocaleTimeString(): String = native

  override def valueOf(): Double = native

  def getTime(): Double = native

  /**
   * Returns the year (4 digits for 4-digit years) of the specified date according to local time.
   *
   * MDN
   */
  def getFullYear(): Int = native

  /**
   * Returns the year (4 digits for 4-digit years) in the specified date according to universal time.
   *
   * MDN
   */
  def getUTCFullYear(): Int = native

  /**
   * Returns the month (0-11) in the specified date according to local time.
   *
   * MDN
   */
  def getMonth(): Int = native

  /**
   * Returns the month (0-11) in the specified date according to universal time.
   *
   * MDN
   */
  def getUTCMonth(): Int = native

  /**
   * Returns the day of the month (1-31) for the specified date according to local time.
   *
   * MDN
   */
  def getDate(): Int = native

  /**
   * Returns the day (date) of the month (1-31) in the specified date according to universal time.
   *
   * MDN
   */
  def getUTCDate(): Int = native

  /**
   * Returns the day of the week (0-6) for the specified date according to local time.
   *
   * MDN
   */
  def getDay(): Int = native

  /**
   * Returns the day of the week (0-6) in the specified date according to universal time.
   * MDN
   */
  def getUTCDay(): Int = native

  /**
   * Returns the hour (0-23) in the specified date according to local time.
   *
   * MDN
   */
  def getHours(): Int = native

  /**
   * Returns the hours (0-23) in the specified date according to universal time.
   *
   * MDN
   */
  def getUTCHours(): Int = native

  /**
   * Returns the minutes (0-59) in the specified date according to local time.
   *
   * MDN
   */
  def getMinutes(): Int = native

  /**
   * Returns the minutes (0-59) in the specified date according to universal time.
   *
   * MDN
   */
  def getUTCMinutes(): Int = native

  /**
   * Returns the seconds (0-59) in the specified date according to local time.
   *
   * MDN
   */
  def getSeconds(): Int = native

  /**
   * Returns the seconds (0-59) in the specified date according to universal time.
   *
   * MDN
   */
  def getUTCSeconds(): Int = native

  /**
   * Returns the milliseconds (0-999) in the specified date according to local time.
   *
   * MDN
   */
  def getMilliseconds(): Int = native

  /**
   * Returns the milliseconds (0-999) in the specified date according to universal time.
   *
   * MDN
   */
  def getUTCMilliseconds(): Int = native

  /**
   * Returns the time-zone offset in minutes for the current locale.
   *
   * MDN
   */
  def getTimezoneOffset(): Int = native

  def setTime(time: Double): Unit = native
  def setMilliseconds(ms: Int): Unit = native
  def setUTCMilliseconds(ms: Int): Unit = native
  def setSeconds(sec: Int, ms: Int = getMilliseconds()): Unit = native
  def setUTCSeconds(sec: Int, ms: Int = getMilliseconds()): Unit = native
  def setMinutes(min: Int, sec: Int = getSeconds(),
      ms: Int = getMilliseconds()): Unit = native
  def setUTCMinutes(min: Int, sec: Int = getSeconds(),
      ms: Int = getMilliseconds()): Unit = native
  def setHours(hours: Int, min: Int = getMinutes(),
      sec: Int = getSeconds(), ms: Int = getMilliseconds()): Unit = native
  def setUTCHours(hours: Int, min: Int = getMinutes(),
      sec: Int = getSeconds(), ms: Int = getMilliseconds()): Unit = native

  def setDate(date: Int): Unit = native
  def setUTCDate(date: Int): Unit = native
  def setMonth(month: Int, date: Int = getDate()): Unit = native
  def setUTCMonth(month: Int, date: Int = getDate()): Unit = native
  def setFullYear(year: Int, month: Int = getMonth(),
      date: Int = getDate()): Unit = native
  def setUTCFullYear(year: Int, month: Int = getMonth(),
      date: Int = getDate()): Unit = native

  def toUTCString(): String = native
  def toISOString(): String = native
  def toJSON(key: Any): String = native
  def toJSON(): String = native
}

/** Factory for [[js.Date]] objects. */
@native
object Date extends Object {
  def apply(): String = native

  /**
   * Parses a string representation of a date and returns the number of
   * milliseconds since 1 January, 1970, 00:00:00, local time.
   *
   * The parse method takes a date string (such as "Dec 25, 1995") and returns
   * the number of milliseconds since January 1, 1970, 00:00:00 UTC. The local
   * time zone is used to interpret arguments that do not contain time zone
   * information. This function is useful for setting date values based on
   * string values, for example in conjunction with the setTime() method and
   * the Date object.
   *
   * Given a string representing a time, parse returns the time value. It
   * accepts the RFC2822 / IETF date syntax (RFC2822 Section 3.3), e.g.
   * "Mon, 25 Dec 1995 13:30:00 GMT". It understands the continental US time-
   * zone abbreviations, but for general use, use a time-zone offset, for
   * example, "Mon, 25 Dec 1995 13:30:00 +0430" (4 hours, 30 minutes east of
   * the Greenwich meridian). If you do not specify a time zone, the local time
   * zone is assumed. GMT and UTC are considered equivalent.
   *
   * MDN
   */
  def parse(s: String): Double = native

  def UTC(year: Int, month: Int, date: Int = 1, hours: Int = 0,
      minutes: Int = 0, seconds: Int = 0, ms: Int = 0): Double = native

  /**
   * Returns the numeric value corresponding to the current time - the number
   * of milliseconds elapsed since 1 January 1970 00:00:00 UTC.
   *
   * MDN
   */
  def now(): Double = native
}
