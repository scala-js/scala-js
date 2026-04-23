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

/* Note that the default timezone is always Etc/GMT (= UTC, without any
 * daylight saving time).
 */

class Date(private var time: Long)
    extends Object with Serializable with Cloneable with Comparable[Date] {

  import Date._

  def this() = this(System.currentTimeMillis())

  @Deprecated
  def this(year: Int, month: Int, date: Int, hrs: Int, min: Int, sec: Int) =
    this(Date.UTC(year, month, date, hrs, min, sec))

  @Deprecated
  def this(year: Int, month: Int, date: Int, hrs: Int, min: Int) =
    this(year, month, date, hrs, min, 0)

  @Deprecated
  def this(year: Int, month: Int, date: Int) =
    this(year, month, date, 0, 0, 0)

  @Deprecated
  def this(date: String) = this(Date.parse(date))

  /** The cached decoded year.
   *
   *  Valid iff `cachedRest` is valid.
   */
  private var cachedYear: Int = 0

  /** Cached data for every else.
   *
   *  Bit field with dayOfWeek, month, day, hour, minute, second.
   *
   *  Valid iff `cachedRest != 0` (because `day` is 1-based).
   */
  private var cachedRest: Int = 0

  // API that only uses the timestamp (no decoding/encoding)

  def after(when: Date): Boolean = time > when.time

  def before(when: Date): Boolean = time < when.time

  override def clone(): Object = new Date(time)

  override def compareTo(anotherDate: Date): Int =
    java.lang.Long.compare(time, anotherDate.time)

  override def equals(obj: Any): Boolean = obj match {
    case d: Date => d.time == time
    case _       => false
  }

  override def hashCode(): Int =
    java.lang.Long.hashCode(time)

  // API that requires decoding and encoding into calendar fields

  @inline
  private def decode(): Decoded = {
    if (cachedRest == 0)
      doDecodeAndCache()
    Decoded.fromBits(cachedYear, cachedRest)
  }

  @noinline
  private def doDecodeAndCache(): Unit = {
    // Decompose into day and time-in-day
    val (daysSinceEpoch, millisInDay) = floorDivMod(this.time, MSPerDay)

    // Decompose time-in-day into hour, minute, second (ignore millis)
    val (hour, millisInHour) = unsignedDivMod(millisInDay, MSPerHour)
    val (minute, millisInMinute) = unsignedDivMod(millisInHour, MSPerMinute)
    val second = Integer.divideUnsigned(millisInMinute, MSPerSecond)

    // Find the year and day-in-year
    val isGregorian = daysSinceEpoch >= FirstGregorianDaySinceEpoch
    val (year, dayInYear, isLeapYear) =
      if (isGregorian) decodeGregorianYearAndDayInYear(daysSinceEpoch)
      else decodeJulianYearAndDayInYear(daysSinceEpoch)

    // Compute the month-in-year and day-in-month
    val (month, day) = if (dayInYear < 31 + 28 + isLeapYear) {
      if (dayInYear < 31)
        (0, dayInYear)
      else
        (1, dayInYear - 31)
    } else {
      val dayInYearAsIfNonLeap = dayInYear - isLeapYear
      val cumulDaysInMonths = CumulativeDaysInMonthsNonLeap // local copy
      var m = 2
      while (dayInYearAsIfNonLeap >= cumulDaysInMonths(m))
        m += 1
      (m, dayInYearAsIfNonLeap - cumulDaysInMonths(m - 1))
    }

    /* The Julian/Gregorian transition did not interrupt the day-of-week cycle.
     * Therefore, we directly compute it from `daysSinceEpoch`.
     * The epoch was a Wednesday, so we add 4 and take the remainder by 7.
     */
    val dayOfWeek = Math.floorMod(daysSinceEpoch + 4L, 7)

    val decoded = new Decoded(year, month, day + 1, hour, minute, second, dayOfWeek)
    this.cachedYear = decoded.year
    this.cachedRest = decoded.toRestBits
  }

  /** Decodes the year and day-in-year of a day that falls in the Gregorian calendar.
   *
   *  @return
   *    `(year, dayInYear, isLeapYear)` where `isLeapYear` is 1 if yes and 0 otherwise.
   */
  @inline // exactly 1 call site
  private def decodeGregorianYearAndDayInYear(daysSinceEpoch: Long): (Int, Int, Int) = {
    // Decompose the day into a 400-year-cycle and days-in-cycle
    val daysSinceYear0 = DaysFromGregorianYear0ToEpoch + daysSinceEpoch
    val (cycleLong, daysIn400YearCycle) = floorDivMod(daysSinceYear0, DaysInGregorian400Years)
    val cycle = cycleLong.toInt // we have divided enough now that we must fit in an int

    /* Compute the year in the cycle and the day in the year.
     *
     * estimatedYearInCycle is either the correct year, or 1 too low.
     *
     * This assumption can be checked with
     *   for (d <- 0 until 146097) { // days in the 400 cycle
     *     val estimatedYear = 1600 + d/366 // start at 1600 for the Gregorian calendar
     *     val correctYear = new Date(1600 - 1900, 0, 1 + d).getYear() + 1900
     *     assert(estimatedYear == correctYear || estimatedYear == correctYear-1)
     *   }
     */
    val (yearInCycle, dayInYear, isLeapYear) = {
      val estimatedYearInCycle = Integer.divideUnsigned(daysIn400YearCycle, 366)
      val estimatedDayInYear = daysIn400YearCycle - daysToYearInGregorianCycle(estimatedYearInCycle)
      val estimatedIsLeap = if (isGregorianLeapYearInCycle(estimatedYearInCycle)) 1 else 0
      val estimatedNumDays = 365 + estimatedIsLeap
      if (estimatedDayInYear < estimatedNumDays) {
        // The estimated day falls into the estimated year, so the estimation is correct
        (estimatedYearInCycle, estimatedDayInYear, estimatedIsLeap)
      } else {
        // Adjust the estimation
        val correctedYear = estimatedYearInCycle + 1
        val correctedDayInYear = estimatedDayInYear - estimatedNumDays
        val correctedYearIsLeap = if (isGregorianLeapYearInCycle(correctedYear)) 1 else 0
        (correctedYear, correctedDayInYear, correctedYearIsLeap)
      }
    }
    val year = cycle * 400 + yearInCycle

    (year, dayInYear, isLeapYear)
  }

  /** Decodes the year and day-in-year of a day that falls in the Julian calendar.
   *
   *  @return
   *    `(year, dayInYear, isLeapYear)` where `isLeapYear` is 1 if yes and 0 otherwise.
   */
  @inline // exactly 1 call site
  private def decodeJulianYearAndDayInYear(daysSinceEpoch: Long): (Int, Int, Int) = {
    // Decompose the day into a 4-year-cycle and days-in-cycle
    val daysSinceYear0 = DaysFromJulianYear0ToEpoch + daysSinceEpoch
    val (cycleLong, daysIn4YearCycle) = floorDivMod(daysSinceYear0, DaysInJulian4Years)
    val cycle = cycleLong.toInt // we have divided enough now that we must fit in an int

    val cycleYears = cycle * 4

    if (daysIn4YearCycle < 366) {
      // First year, which is the leap year
      (cycleYears + 0, daysIn4YearCycle, 1)
    } else {
      // One of the other 3 years, which are not leap years
      val (yearInCycle, dayInYear) = unsignedDivMod(daysIn4YearCycle - 1, 365)
      (cycleYears + yearInCycle, dayInYear, 0)
    }
  }

  @Deprecated
  def getYear(): Int = decode().apiYear

  @Deprecated
  def getMonth(): Int = decode().month

  @Deprecated
  def getDate(): Int = decode().day

  @Deprecated
  def getDay(): Int = decode().dayOfWeek

  @Deprecated
  def getHours(): Int = decode().hour

  @Deprecated
  def getMinutes(): Int = decode().minute

  @Deprecated
  def getSeconds(): Int = decode().second

  def getTime(): Long = time

  @Deprecated
  def getTimezoneOffset(): Int = 0

  /** Resets the year and month.
   *
   *  This requires a full re-encoding.
   */
  private def setYearMonth(d: Decoded, newYear: Int, newMonth: Int): Unit = {
    val timeInDay = Math.floorMod(time, MSPerDay.toLong)
    setTime(encode(newYear, newMonth, d.day, timeInDay))
  }

  /** Sets a field whose difference translate directly into a number of milliseconds.
   *
   *  This is the case for every field except `year` and  `month`.
   */
  private def setCalendarIndependentField(prevValue: Int, newValue: Int, msPerField: Long): Unit =
    setTime(time + (newValue.toLong - prevValue.toLong) * msPerField)

  @Deprecated
  def setYear(year: Int): Unit = {
    val d = decode()
    setYearMonth(d, year + 1900, d.month)
  }

  @Deprecated
  def setMonth(month: Int): Unit = {
    val d = decode()
    setYearMonth(d, d.year, month)
  }

  @Deprecated
  def setDate(date: Int): Unit =
    setCalendarIndependentField(decode().day, date, MSPerDay)

  @Deprecated
  def setHours(hours: Int): Unit =
    setCalendarIndependentField(decode().hour, hours, MSPerHour)

  @Deprecated
  def setMinutes(minutes: Int): Unit =
    setCalendarIndependentField(decode().minute, minutes, MSPerMinute)

  @Deprecated
  def setSeconds(seconds: Int): Unit =
    setCalendarIndependentField(decode().second, seconds, MSPerSecond)

  def setTime(time: Long): Unit = {
    cachedRest = 0 // invalidate cache
    this.time = time
  }

  @Deprecated
  def toGMTString(): String = {
    // Specified format: "d mon yyyy hh:mm:ss GMT".
    val d = decode()
    s"${d.day} ${Months(d.month)} ${d.eraYear} ${d.formattedTimeInDay} GMT"
  }

  def toInstant(): Instant = Instant.ofEpochMilli(getTime())

  @Deprecated
  def toLocaleString(): String = {
    /* Use the format "yyyy mon d hh:mm:ss", which appears to be what the JVM
     * uses for the timezone Etc/GMT and the ROOT locale.
     */
    val d = decode()
    s"${d.eraYear} ${Months(d.month)} ${d.day} ${d.formattedTimeInDay}"
  }

  override def toString(): String = {
    // Specified format: "dow mon dd hh:mm:ss zzz yyyy". zzz is always "GMT".
    val d = decode()
    s"${Days(d.dayOfWeek)} ${Months(d.month)} ${pad0(d.day)} ${d.formattedTimeInDay} GMT ${d.eraYear}"
  }
}

object Date {
  private final val MSPerSecond = 1000
  private final val MSPerMinute = 60000
  private final val MSPerHour = 3600000
  private final val MSPerDay = 86400000

  /** The number of days "since" the epoch to the first day using the Gregorian calendar.
   *
   *  That is Friday, 15 October 1582 in the Gregorian calendar. It would have
   *  corresponded to Friday, 5 October 1582 in the Julian calendar.
   *
   *  It is before the epoch, so it is negative.
   */
  private final val FirstGregorianDaySinceEpoch = -141427L

  /** The number of days in a 400-year cycle in the Gregorian calendar.
   *
   *  This can be computed from `365*400+97`, where `97` is the number of leap
   *  years in the cycle (`400/4 - 3`).
   */
  private final val DaysInGregorian400Years = 146097

  /** The number of days in a 4-year cycle in the Julian calendar.
   *
   *  This can be computed from `365*4+1`.
   */
  private final val DaysInJulian4Years = 1461

  /** The number of days between Gregorian 1st Jan of year 0 (1 BC) and the Epoch.
   *
   *  Note that this particular date is a reference point for calculations, but
   *  it does not exist, since at that time the active calendar is Julian.
   */
  private final val DaysFromGregorianYear0ToEpoch = 719528

  /** The number of days between Julian 1st Jan of year 0 (1 BC) and the Epoch. */
  private final val DaysFromJulianYear0ToEpoch = 719530

  private val CumulativeDaysInMonthsNonLeap: Array[Int] =
    Array(31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365)

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
    new Date().getYear() + 1900

  @inline
  private def floorDivMod(x: Long, y: Int): (Long, Int) = {
    val div = Math.floorDiv(x, y)
    (div, x.toInt - div.toInt * y)
  }

  @inline
  private def floorDivMod(x: Int, y: Int): (Int, Int) = {
    val div = Math.floorDiv(x, y)
    (div, x - div * y)
  }

  @inline
  private def unsignedDivMod(x: Int, y: Int): (Int, Int) = {
    val div = Integer.divideUnsigned(x, y)
    (div, x - div * y)
  }

  @inline
  private final class Decoded(
      val year: Int,
      val month: Int,
      val day: Int,
      val hour: Int,
      val minute: Int,
      val second: Int,
      val dayOfWeek: Int
  ) {
    import Decoded._

    @inline
    def toRestBits: Int = {
      (dayOfWeek << DayOfWeekShift) |
      (month << MonthShift) |
      (day << DayShift) |
      (hour << HourShift) |
      (minute << MinuteShift) |
      (second << SecondShift)
    }

    /** The year as presented in the API (1900-based). */
    @inline
    def apiYear: Int = year - 1900

    /** The year in the AD/BC era, as presented in strings. */
    @inline
    def eraYear: Int =
      if (year > 0) year
      else -year + 1 // 0 is 1 BC, -1 is 2 BC, etc.

    /** The time in day formatted as `hh:mm:ss`, shared by the 3 string methods. */
    @inline
    def formattedTimeInDay: String =
      s"${pad0(hour)}:${pad0(minute)}:${pad0(second)}"
  }

  private object Decoded {
    // Total bits used: 29
    private final val SecondBits = 6 // [0, 59] (we don't track leap seconds)
    private final val MinuteBits = 6 // [0, 59]
    private final val HourBits = 5 // [0, 23]
    private final val DayBits = 5 // [1, 31]
    private final val MonthBits = 4 // [0, 11]
    private final val DayOfWeekBits = 3 // [0, 6]

    private final val SecondShift = 0
    private final val SecondMask = (1 << SecondBits) - 1

    private final val MinuteShift = SecondShift + SecondBits
    private final val MinuteMask = (1 << MinuteBits) - 1

    private final val HourShift = MinuteShift + MinuteBits
    private final val HourMask = (1 << HourBits) - 1

    private final val DayShift = HourShift + HourBits
    private final val DayMask = (1 << DayBits) - 1

    private final val MonthShift = DayShift + DayBits
    private final val MonthMask = (1 << MonthBits) - 1

    private final val DayOfWeekShift = MonthShift + MonthBits
    private final val DayOfWeekMask = (1 << DayOfWeekBits) - 1

    @inline
    def fromBits(year: Int, bits: Int): Decoded = {
      new Decoded(
        year,
        (bits >>> MonthShift) & MonthMask,
        (bits >>> DayShift) & DayMask,
        (bits >>> HourShift) & HourMask,
        (bits >>> MinuteShift) & MinuteMask,
        (bits >>> SecondShift) & SecondMask,
        (bits >>> DayOfWeekShift) & DayOfWeekMask
      )
    }
  }

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

  /** The number of days from the start of a 400-year-cycle to the given year-in-cycle. */
  private def daysToYearInGregorianCycle(yearInCycle: Int): Int = {
    val divBy4ToSkip = Integer.divideUnsigned(yearInCycle + 3, 4)
    val divBy100ToSkip = Integer.divideUnsigned(yearInCycle + 99, 100)
    val divBy400ToSkip = if (yearInCycle == 0) 0 else 1
    val leapYearsToSkip = divBy4ToSkip - divBy100ToSkip + divBy400ToSkip
    365 * yearInCycle + leapYearsToSkip
  }

  @inline
  private def isGregorianLeapYearInCycle(yearMod400: Int): Boolean =
    (yearMod400 & 3) == 0 && yearMod400 != 100 && yearMod400 != 200 && yearMod400 != 300

  @Deprecated
  def UTC(year: Int, month: Int, date: Int,
      hrs: Int, min: Int, sec: Int): Long = {
    encode(year + 1900, month, date, hrs, min, sec, 0)
  }

  private def encode(year: Int, month: Int, day: Int,
      hour: Int, minute: Int, second: Int, millisecond: Int): Long = {
    val timeInDay = encodeTimeInDay(hour, minute, second, millisecond)
    encode(year, month, day, timeInDay)
  }

  private def encode(year: Int, month: Int, day: Int, timeInDay: Long): Long = {
    val daysSinceEpoch = encodeDaysSinceEpoch(year, month, day)
    daysSinceEpoch * MSPerDay + timeInDay
  }

  @inline
  private def encodeTimeInDay(hour: Int, min: Int, sec: Int, ms: Int): Long =
    hour.toLong * MSPerHour + min.toLong * MSPerMinute + sec.toLong * MSPerSecond + ms.toLong

  private def encodeDaysSinceEpoch(year: Int, month: Int, day: Int): Long = {
    // First transfer any excess months to the year
    val (additionalYearsFromMonth, actualMonth) = floorDivMod(month, 12)
    val actualYear = year + additionalYearsFromMonth

    // Determine in which calendar we are; Gregorian starts on Oct 15, 1582
    val isGregorian = {
      actualYear > 1582 ||
      (actualYear == 1582 && (actualMonth > 9 || (actualMonth == 9 && day >= 15)))
    }

    // Compute the number of days until the start of the year
    val (daysToYear, isLeapYear) = if (isGregorian) {
      val (cycle, yearInCycle) = floorDivMod(actualYear, 400)
      val daysToYearInCycle = daysToYearInGregorianCycle(yearInCycle)
      val daysToYear = cycle * DaysInGregorian400Years + daysToYearInCycle
      val isLeapYear = isGregorianLeapYearInCycle(yearInCycle)
      (daysToYear, isLeapYear)
    } else {
      val (cycle, yearInCycle) = floorDivMod(actualYear, 4)
      val isLeapYear = yearInCycle == 0
      val daysToYearInCycle = if (isLeapYear) 0 else 1 + 365 * yearInCycle
      val daysToYear = cycle * DaysInJulian4Years + daysToYearInCycle
      (daysToYear, isLeapYear)
    }

    // Compute the number of days in the year until the start of the month
    val daysInYearToMonth1st = actualMonth match {
      case 0 => 0
      case 1 => 31
      case _ => CumulativeDaysInMonthsNonLeap(actualMonth - 1) + (if (isLeapYear) 1 else 0)
    }

    // Add up all the days since the epoch
    val daysSinceYear0 =
      daysToYear.toLong + Integer.toUnsignedLong(daysInYearToMonth1st) + (day - 1).toLong
    if (isGregorian)
      daysSinceYear0 - DaysFromGregorianYear0ToEpoch
    else
      daysSinceYear0 - DaysFromJulianYear0ToEpoch
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

    /* If we do not find any timezone, we will default to GMT. We don't need a
     * separate encoding of "not there". All the code paths behave the same for
     * "last parsed is GMT" and "no timezone parsed so far".
     */
    var timezoneOffset = 0

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
          if (timezoneOffset != 0)
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

    /* > If a time zone or time-zone offset has been recognized, then the year,
     * > month, day of month, hour, minute, and second are interpreted in UTC
     * > and then the time-zone offset is applied.
     * > Otherwise, the year, month, day of month, hour, minute, and second
     * > are interpreted in the local time zone.
     *
     * Since the local time zone is always GMT=UTC, we have a unique code path.
     */
    val utc = encode(year, month, dayOfMonth, hour, minute, second, 0)
    utc - (timezoneOffset * MSPerMinute).toLong
  }
}
