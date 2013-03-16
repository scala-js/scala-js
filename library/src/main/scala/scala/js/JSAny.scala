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

  implicit def fromArray[B <: JSAny, A](array: Array[A])(
      implicit ev: A => B): JSArray[B] = {
    val length = array.length
    val result = JSArray.newArray[B](length)
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

sealed trait JSArray[A <: JSAny] extends JSObject {
  def apply(index: JSNumber): A
  def update(index: JSNumber, value: A): Unit

  val length: JSNumber
}

object JSArray {
  def newArray[A <: JSAny](length: JSNumber): JSArray[A] = sys.error("stub")

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

trait JSFunction0[+R <: JSAny] extends JSObject {
  def apply(): R
}

trait JSFunction1[-T1 <: JSAny, +R <: JSAny] extends JSObject {
  def apply(arg1: T1): R
}

trait JSFunction2[-T1 <: JSAny, -T2 <: JSAny, +R <: JSAny] extends JSObject {
  def apply(arg1: T1, arg2: T2): R
}
