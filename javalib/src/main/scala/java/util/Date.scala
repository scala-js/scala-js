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

package java.util

import scala.annotation.switch

import java.lang.Cloneable
import java.time.Instant
import java.util.function._

import scalajs.js

class Date(private var millis: Long)
    extends Object with Serializable with Cloneable with Comparable[Date] {

  import Date._

  def this() = this(System.currentTimeMillis())

  @Deprecated
  def this(year: Int, month: Int, date: Int, hrs: Int, min: Int, sec: Int) =
    this(Date.safeGetTime(new js.Date(1900 + year, month, date, hrs, min, sec, 0)))

  @Deprecated
  def this(year: Int, month: Int, date: Int, hrs: Int, min: Int) =
    this(year, month, date, hrs, min, 0)

  @Deprecated
  def this(year: Int, month: Int, date: Int) =
    this(year, month, date, 0, 0, 0)

  @Deprecated
  def this(date: String) = this(Date.parse(date))

  def after(when: Date): Boolean = millis > when.millis

  def before(when: Date): Boolean = millis < when.millis

  override def clone(): Object = new Date(millis)

  override def compareTo(anotherDate: Date): Int =
    java.lang.Long.compare(millis, anotherDate.millis)

  override def equals(obj: Any): Boolean = obj match {
    case d: Date => d.millis == millis
    case _       => false
  }

  override def hashCode(): Int = millis.hashCode()

  private def asDate(): js.Date = {
    if (!isSafeJSDate()) {
      throw new IllegalArgumentException(
          s"cannot convert this java.util.Date ($millis millis) to a js.Date")
    }
    new js.Date(millis.toDouble)
  }

  @inline
  private def mutDate(mutator: Consumer[js.Date]): Unit = {
    val date = asDate()
    mutator.accept(date)
    millis = safeGetTime(date)
  }

  @Deprecated
  def getDate(): Int = asDate().getDate().toInt

  @Deprecated
  def getDay(): Int = asDate().getDay().toInt

  @Deprecated
  def getHours(): Int = asDate().getHours().toInt

  @Deprecated
  def getMinutes(): Int = asDate().getMinutes().toInt

  @Deprecated
  def getMonth(): Int = asDate().getMonth().toInt

  @Deprecated
  def getSeconds(): Int = asDate().getSeconds().toInt

  def getTime(): Long = millis

  @Deprecated
  def getTimezoneOffset(): Int = new js.Date().getTimezoneOffset().toInt

  @Deprecated
  def getYear(): Int = asDate().getFullYear().toInt - 1900

  @Deprecated
  def setDate(date: Int): Unit = mutDate(_.setDate(date))

  @Deprecated
  def setHours(hours: Int): Unit = mutDate(_.setHours(hours))

  @Deprecated
  def setMinutes(minutes: Int): Unit = mutDate(_.setMinutes(minutes))

  @Deprecated
  def setMonth(month: Int): Unit = mutDate(_.setMonth(month))

  @Deprecated
  def setSeconds(seconds: Int): Unit = mutDate(_.setSeconds(seconds))

  def setTime(time: Long): Unit = millis = time

  @Deprecated
  def setYear(year: Int): Unit = mutDate(_.setFullYear(1900 + year))

  @Deprecated
  def toGMTString(): String = {
    val date = asDate()
    "" + date.getUTCDate().toInt + " " + Months(date.getUTCMonth().toInt) + " " +
      date.getUTCFullYear().toInt + " " + pad0(date.getUTCHours().toInt) + ":" +
      pad0(date.getUTCMinutes().toInt) + ":" +
      pad0(date.getUTCSeconds().toInt) + " GMT"
  }

  def toInstant(): Instant = Instant.ofEpochMilli(getTime())

  @Deprecated
  def toLocaleString(): String = {
    val date = asDate()
    "" + date.getDate().toInt + "-" + Months(date.getMonth().toInt) + "-" +
      date.getFullYear().toInt + "-" + pad0(date.getHours().toInt) + ":" +
      pad0(date.getMinutes().toInt) + ":" + pad0(date.getSeconds().toInt)
  }

  override def toString(): String = {
    if (isSafeJSDate()) {
      val date = asDate()
      val offset = -date.getTimezoneOffset().toInt
      val sign = if (offset < 0) "-" else "+"
      val hours = pad0(Math.abs(offset) / 60)
      val mins = pad0(Math.abs(offset) % 60)
      Days(date.getDay().toInt) + " " + Months(date.getMonth().toInt) + " " +
        pad0(date.getDate().toInt) + " " + pad0(date.getHours().toInt) + ":" +
        pad0(date.getMinutes().toInt) + ":" + pad0(date.getSeconds().toInt) +
        " GMT" + " " + date.getFullYear().toInt
    } else {
      s"java.util.Date($millis)"
    }
  }

  @inline
  private def isSafeJSDate(): Boolean =
    -MaxMillis <= millis && millis <= MaxMillis
}

object Date {
  /* Maximum amount of milliseconds supported in a js.Date.
   * See https://www.ecma-international.org/ecma-262/5.1/#sec-15.9.1.14
   */
  private final val MaxMillis = 8640000000000000L

  private final val MSPerMinute = 60000

  /** Marker for end-of-string in parsing; distinct from every `Char`. */
  private final val EOF = -1

  private val Days = Array(
      "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")

  private val Months = Array(
      "Jan", "Feb", "Mar", "Apr", "May", "Jun",
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  /** All the "words" for which `parse` tries a prefix, in order of precedence. */
  private val ParsePrefixWords: Array[String] = Array(
      "SUNDAY", "MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", "FRIDAY", "SATURDAY",
      "JANUARY", "FEBRUARY", "MARCH", "APRIL", "MAY", "JUNE",
      "JULY", "AUGUST", "SEPTEMBER", "OCTOBER", "NOVEMBER", "DECEMBER"
  )

  /** The current year, used to interpret small years in `parse`.
   *
   *  It is explicitly stored, rather than recomputed, because the JavaDoc
   *  says the computation is relative to
   *  > the time when the Date class is initialized
   *
   *  We don't follow the exact initialization time, but at least this way we
   *  preserve the fact that only one `currentYear` is observed through the
   *  lifetime of the application.
   */
  private lazy val currentYear: Int =
    new js.Date().getFullYear().toInt

  private def pad0(i: Int): String = {
    val str = "" + i
    if (str.length < 2) "0" + str else str
  }

  def from(instant: Instant): Date = {
    try {
      new Date(instant.toEpochMilli())
    } catch {
      case ex: ArithmeticException =>
        throw new IllegalArgumentException(ex)
    }
  }

  @Deprecated
  def UTC(year: Int, month: Int, date: Int,
      hrs: Int, min: Int, sec: Int): Long = {
    js.Date.UTC(year + 1900, month, date, hrs, min, sec).toLong
  }

  @Deprecated
  def parse(string: String): Long = {
    /* Parse the string according to
     * https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/util/Date.html#parse(java.lang.String)
     * Comments starting with '>' are quoted from the JavaDoc.
     */

    /* We can use -1 as absent value for these fields because they cannot
     * be parsed as negative values.
     */
    var year = -1
    var month = -1
    var dayOfMonth = -1
    var hour = -1
    var minute = -1
    var second = -1

    /* The timezone offset (in minutes) can be negative, but is parsed from a
     * positive int and therefore cannot be Int.MinValue.
     */
    var timezoneOffset = Int.MinValue

    @inline def isASCIIDigit(c: Char): Boolean =
      c >= '0' && c <= '9'

    @inline def isASCIILetter(c: Char): Boolean =
      (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

    def fail(): Nothing =
      throw new IllegalArgumentException(string)

    val len = string.length()
    var i = 0
    while (i != len) {
      val start = i
      val c = string.charAt(start)
      i += 1

      if (isASCIIDigit(c)) {
        // Parse the whole number
        while (i != len && isASCIIDigit(string.charAt(i))) {
          i += 1
        }
        val number = Integer.parseInt(string.substring(start, i))

        // Look behind and ahead
        val prev = if (start == 0) EOF else string.charAt(start - 1).toInt
        val next = if (i == len) EOF else string.charAt(i).toInt

        // > The number is regarded as a year number if one of the following conditions is true:
        @inline def isYear: Boolean = {
          (year == -1) && { // out of spec, but otherwise nothing works
            if (number >= 70) {
              /* > The number is equal to or greater than 70 and followed by a
               * > space, comma, slash, or end of string.
               */
              next == ' ' || next == ',' || next == '/' || next == EOF
            } else {
              /* > The number is less than 70, and both a month and a day of the
               * > month have already been recognized.
               */
              month != -1 && dayOfMonth != -1
            }
          }
        }

        // Decide what to do with the number
        if ((prev == '+' || prev == '-') && year != -1) {
          /* > If a number is preceded by + or - and a year has already been
           * > recognized, then the number is a time-zone offset.
           */
          if (timezoneOffset != Int.MinValue && timezoneOffset != 0)
            fail() // out of spec
          val signedNumber = if (prev == '-') -number else number
          if (number < 24) {
            timezoneOffset = 60 * signedNumber
          } else {
            // convert HHmm (= 100*HH + mm) to 60*HH + mm
            val tzHours = signedNumber / 100 // for once, actually use truncating signed division
            val tzMinutes = signedNumber - 100 * tzHours
            timezoneOffset = (60 * tzHours) + tzMinutes
          }
        } else if (isYear) {
          /* > If the recognized year number is less than 100, it is interpreted as an
           * > abbreviated year relative to a century of which dates are within 80 years
           * > before and 19 years after the time when the Date class is initialized.
           */
          year = if (number >= 100) {
            number
          } else {
            val relCenturyStart = currentYear - 80
            val absCenturyStart = relCenturyStart - Integer.remainderUnsigned(relCenturyStart, 100)
            val candidate = absCenturyStart + number
            if (candidate >= relCenturyStart)
              candidate
            else
              candidate + 100
          }
        } else {
          (next: @switch) match {
            case ':' =>
              /* > If the number is followed by a colon, it is regarded as an hour,
               * > unless an hour has already been recognized, in which case it is
               * > regarded as a minute.
               */
              if (hour == -1)
                hour = number
              else
                minute = number

            case '/' =>
              /* > If the number is followed by a slash, it is regarded as a month
               * > (it is decreased by 1 to produce a number in the range 0 to 11),
               * > unless a month has already been recognized, in which case it is
               * > regarded as a day of the month.
               */
              if (month == -1)
                month = number - 1
              else
                dayOfMonth = number

            case '\t' | '\n' | '\f' | '\r' | ' ' | ',' | '-' | EOF =>
              /* > If the number is followed by whitespace, a comma, a hyphen, or
               * > end of string, then if an hour has been recognized but not a
               * > minute, it is regarded as a minute; otherwise, if a minute has
               * > been recognized but not a second, it is regarded as a second;
               * > otherwise, it is regarded as a day of the month.
               */
              if (hour != -1 && minute == -1)
                minute = number
              else if (minute != -1 && second == -1)
                second = number
              else
                dayOfMonth = number

            case _ =>
              // Otherwise fail (not clearly specified)
              fail()
          }
        }
      } else if (isASCIILetter(c)) {
        // Parse the whole ASCII word
        while (i != len && isASCIILetter(string.charAt(i))) {
          i += 1
        }
        if (i == start + 1)
          fail() // out of spec, but apparently the JVM wants at least 2 letters here
        val word = string.substring(start, i).toUpperCase()

        // Decide what to do with it

        val prefixWords = ParsePrefixWords // local copy
        val prefixWordsLen = prefixWords.length
        var prefixIndex = 0
        while (prefixIndex != prefixWordsLen && !prefixWords(prefixIndex).startsWith(word)) {
          prefixIndex += 1
        }

        if (prefixIndex < 7) {
          // day of week, ignore
        } else if (prefixIndex < prefixWordsLen) {
          // named month
          month = prefixIndex - 7
        } else {
          // one of the explicitly known timezones, otherwise fail
          timezoneOffset = word match {
            case "GMT" | "UT" | "UTC" => 0
            case "EDT"                => -4 * 60
            case "EST" | "CDT"        => -5 * 60
            case "CST" | "MDT"        => -6 * 60
            case "MST" | "PDT"        => -7 * 60
            case "PST"                => -8 * 60
            case _                    => fail()
          }
        }
      } else {
        (c: @switch) match {
          case '\t' | '\n' | '\f' | '\r' | ' ' | ',' | '+' | '-' | ':' | '/' =>
            // ignore
            ()
          case '(' =>
            // Ignore all characters until a matching ')'
            var nesting = 1
            while (i != len && nesting > 0) {
              string.charAt(i) match {
                case '(' => nesting += 1
                case ')' => nesting -= 1
                case _   => ()
              }
              i += 1
            }
          case _ =>
            fail()
        }
      }
    }

    // We need all the components of a date, otherwise fail
    if (year == -1 || month == -1 || dayOfMonth == -1)
      fail()

    // However, time components are optional and default to 0
    if (hour == -1)
      hour = 0
    if (minute == -1)
      minute = 0
    if (second == -1)
      second = 0

    if (timezoneOffset != Int.MinValue) {
      /* > If a time zone or time-zone offset has been recognized, then the year,
       * > month, day of month, hour, minute, and second are interpreted in UTC
       * > and then the time-zone offset is applied.
       */
      val utc = UTC(year - 1900, month, dayOfMonth, hour, minute, second)
      utc - (timezoneOffset * MSPerMinute).toLong
    } else {
      /* > Otherwise, the year, month, day of month, hour, minute, and second
       * > are interpreted in the local time zone.
       */
      safeGetTime(new js.Date(year, month, dayOfMonth, hour, minute, second, 0))
    }
  }

  @inline
  private def safeGetTime(date: js.Date): Long = {
    val time = date.getTime()
    if (java.lang.Double.isNaN(time))
      throw new IllegalArgumentException
    time.toLong
  }
}
