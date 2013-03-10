/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.js

import scala.language.{ dynamics, implicitConversions }

class JavaScriptObject private (private val underlying: AnyRef) extends scala.Dynamic {
  @native def applyDynamic(name: String)(args: JavaScriptObject*): JavaScriptObject
  @native def selectDynamic(name: String): JavaScriptObject
  @native def updateDynamic(name: String)(value: JavaScriptObject): Unit
  @native def apply(args: JavaScriptObject*): JavaScriptObject

  @native override def toString(): String
}

object JavaScriptObject {
  import scala.js.{ JavaScriptObject => JSO }

  @native def newEmpty: JavaScriptObject
  @native def window: JavaScriptObject

  @native implicit def fromUnit(value: Unit): JavaScriptObject
  @native implicit def fromBoolean(value: Boolean): JavaScriptObject
  @native implicit def fromChar(value: Char): JavaScriptObject
  @native implicit def fromByte(value: Byte): JavaScriptObject
  @native implicit def fromShort(value: Short): JavaScriptObject
  @native implicit def fromInt(value: Int): JavaScriptObject
  @native implicit def fromLong(value: Long): JavaScriptObject
  @native implicit def fromFloat(value: Float): JavaScriptObject
  @native implicit def fromDouble(value: Double): JavaScriptObject

  @native implicit def fromString(value: String): JavaScriptObject
  @native def fromObject(value: AnyRef): JavaScriptObject

  @native implicit def fromArray(value: Array[JavaScriptObject]): JavaScriptObject

  implicit def fromSeq[A](value: Seq[A])(implicit ev: A => JSO): JSO = {
    fromArray(value.map(ev).toArray)
  }

  implicit def fromMap[A](map: Map[String, A])(implicit ev: A => JSO): JSO = {
    val result = newEmpty
    for ((name, value) <- map)
      result.updateDynamic(name)(value)
    result
  }

  @native implicit def fromFunction0(f: Function0[JSO]): JSO
  @native implicit def fromFunction1(f: Function1[JSO, JSO]): JSO
  @native implicit def fromFunction2(f: Function2[JSO, JSO, JSO]): JSO

  implicit def fromAnyFunction0[R](f: Function0[R])(
      implicit evr: R => JSO): JSO =
    fromFunction0(() => evr(f()))

  implicit def fromAnyFunction1[T1, R](f: Function1[T1, R])(
      implicit ev1: JSO => T1, evr: R => JSO): JSO =
    fromFunction1((arg1: JSO) => evr(f(ev1(arg1))))

  implicit def fromAnyFunction2[T1, T2, R](f: Function2[T1, T2, R])(
      implicit ev1: JSO => T1, ev2: JSO => T2, evr: R => JSO): JSO =
    fromFunction2((arg1: JSO, arg2: JSO) => evr(f(ev1(arg1), ev2(arg2))))

  @native def toBoolean(value: JavaScriptObject): Boolean
  @native def toInt(value: JavaScriptObject): Int
  @native def toString(value: JavaScriptObject): String
  @native def toObject(value: JavaScriptObject): AnyRef
}
