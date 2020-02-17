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

import scalajs.js

class Date private (private val date: js.Date) extends Object
    with Serializable with Cloneable with Comparable[Date] {

  import Date._

  def this() = this(new js.Date())

  @Deprecated
  def this(year: Int, month: Int, date: Int, hrs: Int, min: Int, sec: Int) = {
    this(new js.Date())
    this.date.setFullYear(1900 + year, month, date)
    this.date.setHours(hrs, min, sec, 0)
  }

  @Deprecated
  def this(year: Int, month: Int, date: Int, hrs: Int, min: Int) =
    this(year, month, date, hrs, min, 0)

  @Deprecated
  def this(year: Int, month: Int, date: Int) =
    this(year, month, date, 0, 0, 0)

  def this(date: Long) = this(new js.Date(date.toDouble))

  @Deprecated
  def this(date: String) = {
    this({
      val jsDate = new js.Date(date)
      if (java.lang.Double.isNaN(jsDate.getTime()))
        throw new IllegalArgumentException
      jsDate
    })
  }

  def after(when: Date): Boolean = date.getTime() > when.date.getTime()

  def before(when: Date): Boolean = date.getTime() < when.date.getTime()

  override def clone(): Object = new Date(new js.Date(date.getTime()))

  override def compareTo(anotherDate: Date): Int =
    java.lang.Double.compare(date.getTime(), anotherDate.date.getTime())

  override def equals(obj: Any): Boolean = obj match {
    case d: Date => d.date.getTime() == date.getTime()
    case _       => false
  }

  override def hashCode(): Int = date.getTime().hashCode()

  @Deprecated
  def getDate(): Int = date.getDate().toInt

  @Deprecated
  def getDay(): Int = date.getDay().toInt

  @Deprecated
  def getHours(): Int = date.getHours().toInt

  @Deprecated
  def getMinutes(): Int = date.getMinutes().toInt

  @Deprecated
  def getMonth(): Int = date.getMonth().toInt

  @Deprecated
  def getSeconds(): Int = date.getSeconds().toInt

  def getTime(): Long = date.getTime().toLong

  @Deprecated
  def getTimezoneOffset(): Int = date.getTimezoneOffset().toInt

  @Deprecated
  def getYear(): Int = date.getFullYear().toInt - 1900

  @Deprecated
  def setDate(date: Int): Unit = this.date.setDate(date)

  @Deprecated
  def setHours(hours: Int): Unit = date.setHours(hours)

  @Deprecated
  def setMinutes(minutes: Int): Unit = date.setMinutes(minutes)

  @Deprecated
  def setMonth(month: Int): Unit = date.setMonth(month)

  @Deprecated
  def setSeconds(seconds: Int): Unit = date.setSeconds(seconds)

  def setTime(time: Long): Unit = date.setTime(time.toDouble)

  @Deprecated
  def setYear(year: Int): Unit = date.setFullYear(1900 + year)

  @Deprecated
  def toGMTString(): String = {
    "" + date.getUTCDate().toInt + " " + Months(date.getUTCMonth().toInt) + " " +
      date.getUTCFullYear().toInt + " " + pad0(date.getUTCHours().toInt) + ":" +
      pad0(date.getUTCMinutes().toInt) + ":" +
      pad0(date.getUTCSeconds().toInt) +" GMT"
  }

  @Deprecated
  def toLocaleString(): String = {
    "" + date.getDate().toInt + "-" + Months(date.getMonth().toInt) + "-" +
      date.getFullYear().toInt + "-" + pad0(date.getHours().toInt) + ":" +
      pad0(date.getMinutes().toInt) + ":" + pad0(date.getSeconds().toInt)
  }

  override def toString(): String = {
    val offset = -date.getTimezoneOffset().toInt
    val sign = if (offset < 0) "-" else "+"
    val hours = pad0(Math.abs(offset) / 60)
    val mins = pad0(Math.abs(offset) % 60)
    Days(date.getDay().toInt) + " "+ Months(date.getMonth().toInt) + " " +
      pad0(date.getDate().toInt) + " " + pad0(date.getHours().toInt) + ":" +
      pad0(date.getMinutes().toInt) + ":" + pad0(date.getSeconds().toInt) +
      " GMT" + " " + date.getFullYear().toInt
  }
}

object Date {
  private val Days = Array(
      "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")

  private val Months = Array(
      "Jan", "Feb", "Mar", "Apr", "May", "Jun",
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  private def pad0(i: Int): String = {
    val str = "" + i
    if (str.length < 2) "0" + str else str
  }

  @Deprecated
  def UTC(year: Int, month: Int, date: Int,
      hrs: Int, min: Int, sec: Int): Long =
    js.Date.UTC(year + 1900, month, date, hrs, min, sec).toLong

  @Deprecated
  def parse(string: String): Long = {
    val time = new js.Date(string).getTime()
    if (java.lang.Double.isNaN(time))
      throw new IllegalArgumentException
    time.toLong
  }
}
