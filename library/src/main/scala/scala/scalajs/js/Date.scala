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

  def this(value: Number) = this()
  def this(value: String) = this()

  def this(year: Number, month: Number, date: Number, hours: Number, minutes: Number, seconds: Number, ms: Number) = this()
  def this(year: Number, month: Number, date: Number, hours: Number, minutes: Number, seconds: Number) = this()
  def this(year: Number, month: Number, date: Number, hours: Number, minutes: Number) = this()
  def this(year: Number, month: Number, date: Number, hours: Number) = this()
  def this(year: Number, month: Number, date: Number) = this()
  def this(year: Number, month: Number) = this()

  def toDateString(): String = ???
  def toTimeString(): String = ???
  def toLocaleDateString(): String = ???
  def toLocaleTimeString(): String = ???

  override def valueOf(): Number = ???

  def getTime(): Number = ???

  /**
   * Returns the year (4 digits for 4-digit years) of the specified date according to local time.
   *
   * MDN
   */
  def getFullYear(): Number = ???

  /**
   * Returns the year (4 digits for 4-digit years) in the specified date according to universal time.
   *
   * MDN
   */
  def getUTCFullYear(): Number = ???

  /**
   * Returns the month (0-11) in the specified date according to local time.
   *
   * MDN
   */
  def getMonth(): Number = ???

  /**
   * Returns the month (0-11) in the specified date according to universal time.
   *
   * MDN
   */
  def getUTCMonth(): Number = ???

  /**
   * Returns the day of the month (1-31) for the specified date according to local time.
   *
   * MDN
   */
  def getDate(): Number = ???

  /**
   * Returns the day (date) of the month (1-31) in the specified date according to universal time.
   *
   * MDN
   */
  def getUTCDate(): Number = ???

  /**
   * Returns the day of the week (0-6) for the specified date according to local time.
   *
   * MDN
   */
  def getDay(): Number = ???

  /**
   * Returns the day of the week (0-6) in the specified date according to universal time.
   * MDN
   */
  def getUTCDay(): Number = ???

  /**
   * Returns the hour (0-23) in the specified date according to local time.
   *
   * MDN
   */
  def getHours(): Number = ???

  /**
   * Returns the hours (0-23) in the specified date according to universal time.
   *
   * MDN
   */
  def getUTCHours(): Number = ???

  /**
   * Returns the minutes (0-59) in the specified date according to local time.
   *
   * MDN
   */
  def getMinutes(): Number = ???

  /**
   * Returns the minutes (0-59) in the specified date according to universal time.
   *
   * MDN
   */
  def getUTCMinutes(): Number = ???

  /**
   * Returns the seconds (0-59) in the specified date according to local time.
   *
   * MDN
   */
  def getSeconds(): Number = ???

  /**
   * Returns the seconds (0-59) in the specified date according to universal time.
   *
   * MDN
   */
  def getUTCSeconds(): Number = ???

  /**
   * Returns the milliseconds (0-999) in the specified date according to local time.
   *
   * MDN
   */
  def getMilliseconds(): Number = ???

  /**
   * Returns the milliseconds (0-999) in the specified date according to universal time.
   *
   * MDN
   */
  def getUTCMilliseconds(): Number = ???

  /**
   * Returns the time-zone offset in minutes for the current locale.
   *
   * MDN
   */
  def getTimezoneOffset(): Number = ???

  def setTime(time: Number): Unit = ???
  def setMilliseconds(ms: Number): Unit = ???
  def setUTCMilliseconds(ms: Number): Unit = ???
  def setSeconds(sec: Number, ms: Number): Unit = ???
  def setSeconds(sec: Number): Unit = ???
  def setUTCSeconds(sec: Number, ms: Number): Unit = ???
  def setUTCSeconds(sec: Number): Unit = ???
  def setMinutes(min: Number, sec: Number, ms: Number): Unit = ???
  def setMinutes(min: Number, sec: Number): Unit = ???
  def setMinutes(min: Number): Unit = ???
  def setUTCMinutes(min: Number, sec: Number, ms: Number): Unit = ???
  def setUTCMinutes(min: Number, sec: Number): Unit = ???
  def setUTCMinutes(min: Number): Unit = ???
  def setHours(hours: Number, min: Number, sec: Number, ms: Number): Unit = ???
  def setHours(hours: Number, min: Number, sec: Number): Unit = ???
  def setHours(hours: Number, min: Number): Unit = ???
  def setHours(hours: Number): Unit = ???
  def setUTCHours(hours: Number, min: Number, sec: Number, ms: Number): Unit = ???
  def setUTCHours(hours: Number, min: Number, sec: Number): Unit = ???
  def setUTCHours(hours: Number, min: Number): Unit = ???
  def setUTCHours(hours: Number): Unit = ???
  def setDate(date: Number): Unit = ???
  def setUTCDate(date: Number): Unit = ???
  def setMonth(month: Number, date: Number): Unit = ???
  def setMonth(month: Number): Unit = ???
  def setUTCMonth(month: Number, date: Number): Unit = ???
  def setUTCMonth(month: Number): Unit = ???
  def setFullYear(year: Number, month: Number, date: Number): Unit = ???
  def setFullYear(year: Number, month: Number): Unit = ???
  def setFullYear(year: Number): Unit = ???
  def setUTCFullYear(year: Number, month: Number, date: Number): Unit = ???
  def setUTCFullYear(year: Number, month: Number): Unit = ???
  def setUTCFullYear(year: Number): Unit = ???

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
  def parse(s: String): Number = ???

  def UTC(year: Number, month: Number, date: Number, hours: Number, minutes: Number, seconds: Number, ms: Number): Number = ???
  def UTC(year: Number, month: Number, date: Number, hours: Number, minutes: Number, seconds: Number): Number = ???
  def UTC(year: Number, month: Number, date: Number, hours: Number, minutes: Number): Number = ???
  def UTC(year: Number, month: Number, date: Number, hours: Number): Number = ???
  def UTC(year: Number, month: Number, date: Number): Number = ???
  def UTC(year: Number, month: Number): Number = ???

  /**
   * Returns the numeric value corresponding to the current time - the number
   * of milliseconds elapsed since 1 January 1970 00:00:00 UTC.
   *
   * MDN
   */
  def now(): Number = ???
}
