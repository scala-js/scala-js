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

sealed trait Any extends scala.AnyRef {
  def unary_+(): Number = sys.error("stub")
  def unary_-(): Number = sys.error("stub")
  def unary_~(): Number = sys.error("stub")

  def unary_!(): Boolean = sys.error("stub")

  def +(that: String): String = sys.error("stub")
  def +(that: Dynamic): Any = sys.error("stub") // JSNumber v JSString

  def &&[A <: Any](that: A): that.type = sys.error("stub")

  // def ||[A <: Any](that: A): this.type v that.type = sys.error("stub")
  def ||(that: Any): Any = sys.error("stub")
}

object Any {
  implicit def fromUnit(value: Unit): Undefined = sys.error("stub")

  implicit def fromBoolean(value: scala.Boolean): Boolean = sys.error("stub")

  implicit def fromByte(value: scala.Byte): Number = sys.error("stub")
  implicit def fromShort(value: scala.Short): Number = sys.error("stub")
  implicit def fromInt(value: scala.Int): Number = sys.error("stub")
  implicit def fromLong(value: scala.Long): Number = sys.error("stub")
  implicit def fromFloat(value: scala.Float): Number = sys.error("stub")
  implicit def fromDouble(value: scala.Double): Number = sys.error("stub")

  implicit def fromString(s: java.lang.String): String = sys.error("stub")

  implicit def fromArray[A](array: scala.Array[A]): Array[A] = {
    val length = array.length
    val result = new Array[A](length)
    var i = 0
    while (i < length) {
      result(i) = array(i)
      i += 1
    }
    result
  }

  implicit def fromFunction0[R](f: scala.Function0[R]): Function0[R] = sys.error("stub")
  implicit def fromFunction1[T1, R](f: scala.Function1[T1, R]): Function1[T1, R] = sys.error("stub")
  implicit def fromFunction2[T1, T2, R](f: scala.Function2[T1, T2, R]): Function2[T1, T2, R] = sys.error("stub")
}

sealed trait Dynamic extends Any with scala.Dynamic {
  def applyDynamic(name: java.lang.String)(args: Any*): Dynamic
  def selectDynamic(name: java.lang.String): Dynamic
  def updateDynamic(name: java.lang.String)(value: Any): Unit
  def apply(args: Any*): Dynamic

  def +(that: Number): Number
  def +(that: Any): Any // Number v String

  def -(that: Number): Number
  def *(that: Number): Number
  def /(that: Number): Number
  def %(that: Number): Number
  def <<(that: Number): Number
  def >>(that: Number): Number
  def >>>(that: Number): Number
  def &(that: Number): Number
  def |(that: Number): Number
  def ^(that: Number): Number

  def -(that: Dynamic): Number
  def *(that: Dynamic): Number
  def /(that: Dynamic): Number
  def %(that: Dynamic): Number
  def <<(that: Dynamic): Number
  def >>(that: Dynamic): Number
  def >>>(that: Dynamic): Number
  def &(that: Dynamic): Number
  def |(that: Dynamic): Number
  def ^(that: Dynamic): Number

  def ||(that: Dynamic): Dynamic

  // Work around the annoying implicits in Predef in Scala 2.10.
  def x: Dynamic
  def x_=(value: Any): Dynamic
}

object Dynamic {
  implicit def fromAny(value: Any): Dynamic = sys.error("stub")

  /** Dynamic view of the global scope */
  def global: Dynamic = sys.error("stub")

  /** Instantiate a new object of a JavaScript class */
  def newInstance(clazz: Dynamic)(args: Any*): Dynamic = sys.error("stub")
}

/** Dictionary "view" of a JavaScript value */
sealed trait Dictionary extends Any {
  def apply(key: String): Any
  def update(key: String, value: Any)
}

object Dictionary {
  def empty: Dictionary = new Object

  def apply(properties: (String, Any)*): Dictionary =
    apply(properties)

  def apply(properties: TraversableOnce[(String, Any)]): Dictionary = {
    val result = empty
    for ((key, value) <- properties)
      result(key) = value
    result
  }

  def apply[A <% String, B <% Any](properties: (A, B)*): Dictionary =
    apply(properties)

  def apply[A <% String, B <% Any](
      properties: TraversableOnce[(A, B)]): Dictionary = {
    val result = empty
    for ((key, value) <- properties)
      result(key) = value
    result
  }

  def propertiesOf(obj: Any): Array[String] = sys.error("stub")

  implicit def fromAny(value: Any): Dictionary = value.asInstanceOf
}

sealed trait Number extends Any {
  def +(that: Number): Number

  def -(that: Number): Number
  def *(that: Number): Number
  def /(that: Number): Number
  def %(that: Number): Number
  def <<(that: Number): Number
  def >>(that: Number): Number
  def >>>(that: Number): Number
  def &(that: Number): Number
  def |(that: Number): Number
  def ^(that: Number): Number

  def -(that: Dynamic): Number
  def *(that: Dynamic): Number
  def /(that: Dynamic): Number
  def %(that: Dynamic): Number
  def <<(that: Dynamic): Number
  def >>(that: Dynamic): Number
  def >>>(that: Dynamic): Number
  def &(that: Dynamic): Number
  def |(that: Dynamic): Number
  def ^(that: Dynamic): Number

  def ||(that: Number): Number

  def toString(base: Number): String
}

object Number {
  implicit def toDouble(value: Number): scala.Double = sys.error("stub")
}

sealed trait Boolean extends Any {
  def ||(that: Boolean): Boolean

  def unary_!(): Boolean
}

object Boolean {
  implicit def toBoolean(value: Boolean): scala.Boolean = sys.error("stub")
}

sealed trait String extends Any {
  def +(that: Any): String
  override def +(that: Dynamic): String = sys.error("stub")

  def ||(that: String): String
}

object String {
  implicit def toScalaString(value: String): java.lang.String = sys.error("stub")
}

sealed trait Undefined extends Any with NotNull

class Object extends Any {
  def this(value: Any) = this()
}

/** Marker trait for static modules representing the JS global scope
 *  When calling method on a top-level object or package object that is a
 *  subtype of GlobalScope, the receiver is dropped, and the JS global
 *  scope is used instead.
 */
trait GlobalScope extends Object

final class Array[A](_len: Number) extends Object {
  def this() = this(0)

  def apply(index: Number): A = sys.error("stub")
  def update(index: Number, value: A): Unit = sys.error("stub")

  val length: Number = sys.error("stub")
}

object Array {
  def apply[A](elements: A*): Array[A] = sys.error("stub")

  implicit def toArray[A : ClassTag](array: Array[A]): scala.Array[A] = {
    val length = array.length.toInt
    val result = new scala.Array[A](length)
    var i = 0
    while (i < length) {
      result(i) = array(i)
      i += 1
    }
    result
  }
}

class Date extends Object {
  def this(milliseconds: Number) = this()
  def this(dateString: String) = this()
  def this(year: Number, month: Number, day: Number = 1,
      hours: Number = 0, minutes: Number = 0, seconds: Number = 0,
      milliseconds: Number = 0) = this()

  def getTime(): Number = sys.error("stub")
}

trait Function0[+R] extends Object {
  def apply(): R
}

trait Function1[-T1, +R] extends Object {
  def apply(arg1: T1): R
}

trait Function2[-T1, -T2, +R] extends Object {
  def apply(arg1: T1, arg2: T2): R
}
