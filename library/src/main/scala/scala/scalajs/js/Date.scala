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
class Date extends Object {

  def this(value: Double) = this()
  def this(value: String) = this()

  def this(year: Int, month: Int, date: Int = 1, hours: Int = 0,
      minutes: Int = 0, seconds: Int = 0, ms: Int = 0) = this()

  def toDateString(): String = ???
  def toTimeString(): String = ???
  def toLocaleDateString(): String = ???
  def toLocaleTimeString(): String = ???

  override def valueOf(): Double = ???

  def getTime(): Double = ???

  /**
   * Returns the year (4 digits for 4-digit years) of the specified date according to local time.
   *
   * MDN
   */
  def getFullYear(): Int = ???

  /**
   * Returns the year (4 digits for 4-digit years) in the specified date according to universal time.
   *
   * MDN
   */
  def getUTCFullYear(): Int = ???

  /**
   * Returns the month (0-11) in the specified date according to local time.
   *
   * MDN
   */
  def getMonth(): Int = ???

  /**
   * Returns the month (0-11) in the specified date according to universal time.
   *
   * MDN
   */
  def getUTCMonth(): Int = ???

  /**
   * Returns the day of the month (1-31) for the specified date according to local time.
   *
   * MDN
   */
  def getDate(): Int = ???

  /**
   * Returns the day (date) of the month (1-31) in the specified date according to universal time.
   *
   * MDN
   */
  def getUTCDate(): Int = ???

  /**
   * Returns the day of the week (0-6) for the specified date according to local time.
   *
   * MDN
   */
  def getDay(): Int = ???

  /**
   * Returns the day of the week (0-6) in the specified date according to universal time.
   * MDN
   */
  def getUTCDay(): Int = ???

  /**
   * Returns the hour (0-23) in the specified date according to local time.
   *
   * MDN
   */
  def getHours(): Int = ???

  /**
   * Returns the hours (0-23) in the specified date according to universal time.
   *
   * MDN
   */
  def getUTCHours(): Int = ???

  /**
   * Returns the minutes (0-59) in the specified date according to local time.
   *
   * MDN
   */
  def getMinutes(): Int = ???

  /**
   * Returns the minutes (0-59) in the specified date according to universal time.
   *
   * MDN
   */
  def getUTCMinutes(): Int = ???

  /**
   * Returns the seconds (0-59) in the specified date according to local time.
   *
   * MDN
   */
  def getSeconds(): Int = ???

  /**
   * Returns the seconds (0-59) in the specified date according to universal time.
   *
   * MDN
   */
  def getUTCSeconds(): Int = ???

  /**
   * Returns the milliseconds (0-999) in the specified date according to local time.
   *
   * MDN
   */
  def getMilliseconds(): Int = ???

  /**
   * Returns the milliseconds (0-999) in the specified date according to universal time.
   *
   * MDN
   */
  def getUTCMilliseconds(): Int = ???

  /**
   * Returns the time-zone offset in minutes for the current locale.
   *
   * MDN
   */
  def getTimezoneOffset(): Int = ???

  def setTime(time: Double): Unit = ???
  def setMilliseconds(ms: Int): Unit = ???
  def setUTCMilliseconds(ms: Int): Unit = ???
  def setSeconds(sec: Int, ms: Int = getMilliseconds()): Unit = ???
  def setUTCSeconds(sec: Int, ms: Int = getMilliseconds()): Unit = ???
  def setMinutes(min: Int, sec: Int = getSeconds(),
      ms: Int = getMilliseconds()): Unit = ???
  def setUTCMinutes(min: Int, sec: Int = getSeconds(),
      ms: Int = getMilliseconds()): Unit = ???
  def setHours(hours: Int, min: Int = getMinutes(),
      sec: Int = getSeconds(), ms: Int = getMilliseconds()): Unit = ???
  def setUTCHours(hours: Int, min: Int = getMinutes(),
      sec: Int = getSeconds(), ms: Int = getMilliseconds()): Unit = ???

  def setDate(date: Int): Unit = ???
  def setUTCDate(date: Int): Unit = ???
  def setMonth(month: Int, date: Int = getDate()): Unit = ???
  def setUTCMonth(month: Int, date: Int = getDate()): Unit = ???
  def setFullYear(year: Int, month: Int = getMonth(),
      date: Int = getDate()): Unit = ???
  def setUTCFullYear(year: Int, month: Int = getMonth(),
      date: Int = getDate()): Unit = ???

  def toUTCString(): String = ???
  def toISOString(): String = ???
  def toJSON(key: Any): String = ???
  def toJSON(): String = ???
}

/** Factory for [[js.Date]] objects. */
object Date extends Object {
  def apply(): String = ???

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
  def parse(s: String): Int = ???

  def UTC(year: Int, month: Int, date: Int = 1, hours: Int = 0,
      minutes: Int = 0, seconds: Int = 0, ms: Int = 0): Double = ???

  /**
   * Returns the numeric value corresponding to the current time - the number
   * of milliseconds elapsed since 1 January 1970 00:00:00 UTC.
   *
   * MDN
   */
  def now(): Double = ???
}
