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

import java.lang.Cloneable
import java.time.Instant
import java.util.function._

import scalajs.js

class Date(private var millis: Long) extends Object
    with Serializable with Cloneable with Comparable[Date] {

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

  private val Days = Array(
      "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")

  private val Months = Array(
      "Jan", "Feb", "Mar", "Apr", "May", "Jun",
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

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
      hrs: Int, min: Int, sec: Int): Long =
    js.Date.UTC(year + 1900, month, date, hrs, min, sec).toLong

  @Deprecated
  def parse(string: String): Long = safeGetTime(new js.Date(string))

  private def safeGetTime(date: js.Date): Long = {
    val time = date.getTime()
    if (java.lang.Double.isNaN(time))
      throw new IllegalArgumentException
    time.toLong
  }
}
