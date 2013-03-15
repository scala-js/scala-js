/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.js

import scala.language.{ dynamics, implicitConversions }

sealed trait JSAny extends AnyRef {
  def unary_+(): JSNumber
  def unary_-(): JSNumber
  def unary_~(): JSNumber

  def unary_!(): JSBoolean

  def +(that: JSString): JSString
  def +(that: JSDynamic): JSAny // JSNumber v JSString

  def &&[A <: JSAny](that: A): that.type

  // def ||[A <: JSAny](that: A): this.type v that.type
  def ||(that: JSAny): JSAny
}

object JSAny {
  implicit def fromUnit(value: Unit): JSUndefined = sys.error("stub")

  implicit def fromBoolean(value: Boolean): JSBoolean = sys.error("stub")

  implicit def fromByte(value: Byte): JSNumber = sys.error("stub")
  implicit def fromShort(value: Short): JSNumber = sys.error("stub")
  implicit def fromInt(value: Int): JSNumber = sys.error("stub")
  implicit def fromLong(value: Long): JSNumber = sys.error("stub")
  implicit def fromFloat(value: Float): JSNumber = sys.error("stub")
  implicit def fromDouble(value: Double): JSNumber = sys.error("stub")

  implicit def fromString(s: String): JSString = sys.error("stub")
}

sealed trait JSDynamic extends JSAny with scala.Dynamic {
  def applyDynamic(name: JSString)(args: JSAny*): JSDynamic
  def selectDynamic(name: JSString): JSDynamic
  def updateDynamic(name: JSString)(value: JSAny): Unit
  def apply(args: JSAny*): JSDynamic

  def +(that: JSNumber): JSNumber
  def +(that: JSAny): JSAny // JSNumber v JSString

  def -(that: JSNumber): JSNumber
  def *(that: JSNumber): JSNumber
  def /(that: JSNumber): JSNumber
  def %(that: JSNumber): JSNumber
  def <<(that: JSNumber): JSNumber
  def >>(that: JSNumber): JSNumber
  def >>>(that: JSNumber): JSNumber
  def &(that: JSNumber): JSNumber
  def |(that: JSNumber): JSNumber
  def ^(that: JSNumber): JSNumber

  def -(that: JSDynamic): JSNumber
  def *(that: JSDynamic): JSNumber
  def /(that: JSDynamic): JSNumber
  def %(that: JSDynamic): JSNumber
  def <<(that: JSDynamic): JSNumber
  def >>(that: JSDynamic): JSNumber
  def >>>(that: JSDynamic): JSNumber
  def &(that: JSDynamic): JSNumber
  def |(that: JSDynamic): JSNumber
  def ^(that: JSDynamic): JSNumber

  def ||(that: JSDynamic): JSDynamic
}

object JSDynamic {
  implicit def fromAny(value: JSAny): JSDynamic = sys.error("stub")

  def window: JSDynamic = sys.error("stub")
}

sealed trait JSNumber extends JSAny {
  def +(that: JSNumber): JSNumber

  def -(that: JSNumber): JSNumber
  def *(that: JSNumber): JSNumber
  def /(that: JSNumber): JSNumber
  def %(that: JSNumber): JSNumber
  def <<(that: JSNumber): JSNumber
  def >>(that: JSNumber): JSNumber
  def >>>(that: JSNumber): JSNumber
  def &(that: JSNumber): JSNumber
  def |(that: JSNumber): JSNumber
  def ^(that: JSNumber): JSNumber

  def -(that: JSDynamic): JSNumber
  def *(that: JSDynamic): JSNumber
  def /(that: JSDynamic): JSNumber
  def %(that: JSDynamic): JSNumber
  def <<(that: JSDynamic): JSNumber
  def >>(that: JSDynamic): JSNumber
  def >>>(that: JSDynamic): JSNumber
  def &(that: JSDynamic): JSNumber
  def |(that: JSDynamic): JSNumber
  def ^(that: JSDynamic): JSNumber

  def ||(that: JSNumber): JSNumber
}

object JSNumber {
  implicit def toDouble(value: JSNumber): Double = sys.error("stub")
}

sealed trait JSBoolean extends JSAny {
  def ||(that: JSBoolean): JSBoolean

  def unary_!(): JSBoolean
}

object JSBoolean {
  implicit def toBoolean(value: JSBoolean): Boolean = sys.error("stub")
}

sealed trait JSString extends JSAny {
  def +(that: JSAny): JSString
  override def +(that: JSDynamic): JSString

  def ||(that: JSString): JSString
}

object JSString {
  implicit def toScalaString(value: JSString): String = sys.error("stub")
}

sealed trait JSUndefined extends JSAny with NotNull

abstract class JSObject extends JSAny

object JSObject {
  def newEmpty: JSObject = sys.error("stub")
}

trait JSFunction extends JSObject
