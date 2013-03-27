/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.js

import scala.language.{ dynamics, implicitConversions }
import scala.reflect.ClassTag

sealed trait JSAny extends AnyRef {
  def unary_+(): JSNumber = sys.error("stub")
  def unary_-(): JSNumber = sys.error("stub")
  def unary_~(): JSNumber = sys.error("stub")

  def unary_!(): JSBoolean = sys.error("stub")

  def +(that: JSString): JSString = sys.error("stub")
  def +(that: JSDynamic): JSAny = sys.error("stub") // JSNumber v JSString

  def &&[A <: JSAny](that: A): that.type = sys.error("stub")

  // def ||[A <: JSAny](that: A): this.type v that.type = sys.error("stub")
  def ||(that: JSAny): JSAny = sys.error("stub")
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

  implicit def fromArray[B <: JSAny, A](array: Array[A])(
      implicit ev: A => B): JSArray[B] = {
    val length = array.length
    val result = new JSArray[B](length)
    var i = 0
    while (i < length) {
      result(i) = array(i)
      i += 1
    }
    result
  }

  implicit def fromFunction0[R <: JSAny](f: Function0[R]): JSFunction0[R] = sys.error("stub")
  implicit def fromFunction1[T1 <: JSAny, R <: JSAny](f: Function1[T1, R]): JSFunction1[T1, R] = sys.error("stub")
  implicit def fromFunction2[T1 <: JSAny, T2 <: JSAny, R <: JSAny](f: Function2[T1, T2, R]): JSFunction2[T1, T2, R] = sys.error("stub")
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

/** Dictionary "view" of a JavaScript value */
sealed trait JSDictionary extends JSAny {
  def apply(key: JSString): JSAny
  def update(key: JSString, value: JSAny)
}

object JSDictionary {
  def empty: JSDictionary = new JSObject

  def apply(properties: (JSString, JSAny)*): JSDictionary =
    apply(properties)

  def apply(properties: TraversableOnce[(JSString, JSAny)]): JSDictionary = {
    val result = empty
    for ((key, value) <- properties)
      result(key) = value
    result
  }

  def apply[A <% JSString, B <% JSAny](properties: (A, B)*): JSDictionary =
    apply(properties)

  def apply[A <% JSString, B <% JSAny](
      properties: TraversableOnce[(A, B)]): JSDictionary = {
    val result = empty
    for ((key, value) <- properties)
      result(key) = value
    result
  }

  implicit def fromAny(value: JSAny): JSDictionary = value.asInstanceOf
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

  def toString(base: JSNumber): JSString
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
  override def +(that: JSDynamic): JSString = sys.error("stub")

  def ||(that: JSString): JSString
}

object JSString {
  implicit def toScalaString(value: JSString): String = sys.error("stub")
}

sealed trait JSUndefined extends JSAny with NotNull

class JSObject extends JSAny {
  def this(value: JSAny) = this()
}

final class JSArray[A <: JSAny](_len: JSNumber) extends JSObject {
  def this() = this(0)

  def apply(index: JSNumber): A = sys.error("stub")
  def update(index: JSNumber, value: A): Unit = sys.error("stub")

  val length: JSNumber = sys.error("stub")
}

object JSArray {
  def apply[A <: JSAny](elements: A*): JSArray[A] = sys.error("stub")

  implicit def toArray[B : ClassTag, A <: JSAny](array: JSArray[A])(
      implicit ev: A => B): Array[B] = {
    val length = array.length.toInt
    val result = new Array[B](length)
    var i = 0
    while (i < length) {
      result(i) = array(i)
      i += 1
    }
    result
  }
}

class Date extends JSObject {
  def this(milliseconds: JSNumber) = this()
  def this(dateString: JSString) = this()
  def this(year: JSNumber, month: JSNumber, day: JSNumber = 1,
      hours: JSNumber = 0, minutes: JSNumber = 0, seconds: JSNumber = 0,
      milliseconds: JSNumber = 0) = this()

  def getTime(): JSNumber = sys.error("stub")
}

trait JSFunction0[+R <: JSAny] extends JSObject {
  def apply(): R
}

trait JSFunction1[-T1 <: JSAny, +R <: JSAny] extends JSObject {
  def apply(arg1: T1): R
}

trait JSFunction2[-T1 <: JSAny, -T2 <: JSAny, +R <: JSAny] extends JSObject {
  def apply(arg1: T1, arg2: T2): R
}
