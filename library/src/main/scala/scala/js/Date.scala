/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.js

/** Represents JavaScript `Date` instances.
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
  def getFullYear(): Number = ???
  def getUTCFullYear(): Number = ???
  def getMonth(): Number = ???
  def getUTCMonth(): Number = ???
  def getDate(): Number = ???
  def getUTCDate(): Number = ???
  def getDay(): Number = ???
  def getUTCDay(): Number = ???
  def getHours(): Number = ???
  def getUTCHours(): Number = ???
  def getMinutes(): Number = ???
  def getUTCMinutes(): Number = ???
  def getSeconds(): Number = ???
  def getUTCSeconds(): Number = ???
  def getMilliseconds(): Number = ???
  def getUTCMilliseconds(): Number = ???
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

  def parse(s: String): Number = ???

  def UTC(year: Number, month: Number, date: Number, hours: Number, minutes: Number, seconds: Number, ms: Number): Number = ???
  def UTC(year: Number, month: Number, date: Number, hours: Number, minutes: Number, seconds: Number): Number = ???
  def UTC(year: Number, month: Number, date: Number, hours: Number, minutes: Number): Number = ???
  def UTC(year: Number, month: Number, date: Number, hours: Number): Number = ???
  def UTC(year: Number, month: Number, date: Number): Number = ???
  def UTC(year: Number, month: Number): Number = ???

  def now(): Number = ???
}
