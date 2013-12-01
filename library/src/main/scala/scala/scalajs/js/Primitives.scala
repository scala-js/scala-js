/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */



package scala.scalajs.js

import annotation.JSBracketAccess

import scala.language.{ dynamics, implicitConversions }
import scala.reflect.ClassTag
import scala.collection.mutable

/** Super-type of all JavaScript values.
 *
 *  All values of a subtype of this trait represent JavaScript values, without
 *  boxing of proxying of any kind.
 */
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

/** Provides implicit conversions from Scala values to JavaScript values. */
object Any {
  implicit def fromUnit(value: Unit): Undefined = sys.error("stub")

  implicit def fromBoolean(value: scala.Boolean): Boolean = sys.error("stub")

  implicit def fromByte(value: scala.Byte): Number = sys.error("stub")
  implicit def fromShort(value: scala.Short): Number = sys.error("stub")
  implicit def fromInt(value: scala.Int): Number = sys.error("stub")
  implicit def fromLong(value: scala.Long): Number = sys.error("stub")
  implicit def fromFloat(value: scala.Float): Number = sys.error("stub")
  implicit def fromDouble(value: scala.Double): Number = sys.error("stub")

  implicit def fromString(s: java.lang.String): JsString = sys.error("stub")

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

  def fromTraversableOnce[A](col: TraversableOnce[A]): Array[A] = {
    val result = new Array[A]
    col.foreach(x => result.push(x))
    result
  }

  implicit def arrayOps[A : ClassTag](array: Array[A]): mutable.ArrayOps[A] =
    genericArrayOps(toArray(array))

  implicit def fromFunction0[R](f: scala.Function0[R]): Function0[R] = sys.error("stub")
  implicit def fromFunction1[T1, R](f: scala.Function1[T1, R]): Function1[T1, R] = sys.error("stub")
  implicit def fromFunction2[T1, T2, R](f: scala.Function2[T1, T2, R]): Function2[T1, T2, R] = sys.error("stub")
  implicit def fromFunction3[T1, T2, T3, R](f: scala.Function3[T1, T2, T3, R]): Function3[T1, T2, T3, R] = sys.error("stub")
  implicit def fromFunction4[T1, T2, T3, T4, R](f: scala.Function4[T1, T2, T3, T4, R]): Function4[T1, T2, T3, T4, R] = sys.error("stub")
  implicit def fromFunction5[T1, T2, T3, T4, T5, R](f: scala.Function5[T1, T2, T3, T4, T5, R]): Function5[T1, T2, T3, T4, T5, R] = sys.error("stub")

  implicit def toFunction0[R](f: Function0[R]): scala.Function0[R] = () => f()
  implicit def toFunction1[T1, R](f: Function1[T1, R]): scala.Function1[T1, R] = (x1) => f(x1)
  implicit def toFunction2[T1, T2, R](f: Function2[T1, T2, R]): scala.Function2[T1, T2, R] = (x1, x2) => f(x1, x2)
  implicit def toFunction3[T1, T2, T3, R](f: Function3[T1, T2, T3, R]): scala.Function3[T1, T2, T3, R] = (x1, x2, x3) => f(x1, x2, x3)
  implicit def toFunction4[T1, T2, T3, T4, R](f: Function4[T1, T2, T3, T4, R]): scala.Function4[T1, T2, T3, T4, R] = (x1, x2, x3, x4) => f(x1, x2, x3, x4)
  implicit def toFunction5[T1, T2, T3, T4, T5, R](f: Function5[T1, T2, T3, T4, T5, R]): scala.Function5[T1, T2, T3, T4, T5, R] = (x1, x2, x3, x4, x5) => f(x1, x2, x3, x4, x5)
}

/** Dynamically typed JavaScript value.
 *
 *  Values of this trait accept all possible JavaScript operations in a
 *  dynamically typed way. You can read and write any field, call any method,
 *  apply any JavaScript operator to values of this type.
 */
sealed trait Dynamic extends Any with scala.Dynamic {
  /** Calls a method of this object. */
  def applyDynamic(name: java.lang.String)(args: Any*): Dynamic

  /** Reads a field of this object. */
  def selectDynamic(name: java.lang.String): Dynamic

  /** Writes a field of this object. */
  def updateDynamic(name: java.lang.String)(value: Any): Unit

  /** Calls this object as a callable. */
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

/** Factory for dynamically typed JavaScript values. */
object Dynamic {
  /** Dynamic view of the global scope. */
  def global: Dynamic = sys.error("stub")

  /** Instantiates a new object of a JavaScript class. */
  def newInstance(clazz: Dynamic)(args: Any*): Dynamic = sys.error("stub")
}

/** Dictionary "view" of a JavaScript value */
sealed trait Dictionary extends Any {
  /** Reads a field of this object by its name. */
  @JSBracketAccess
  def apply(key: String): Any

  /** Writes a field of this object by its name. */
  @JSBracketAccess
  def update(key: String, value: Any): Unit
}

/** Factory for [[Dictionary]] instances. */
object Dictionary {
  /** Returns a new empty dictionary */
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

  /** Returns the names of all the enumerable properties of this object. */
  def propertiesOf(obj: Any): Array[String] = sys.error("stub")

  implicit def fromAny(value: Any): Dictionary = value.asInstanceOf
}

/** Primitive JavaScript number. */
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

  def toString(radix: Number): String = ???
  def toFixed(fractionDigits: Number): String = ???
  def toFixed(): String = ???
  def toExponential(fractionDigits: Number): String = ???
  def toExponential(): String = ???
  def toPrecision(precision: Number): String = ???
}

/** The top-level `Number` JavaScript object */
object Number extends Object {
  implicit def toDouble(value: Number): scala.Double = sys.error("stub")

  val MAX_VALUE: Number = ???
  val MIN_VALUE: Number = ???
  val NaN: Number = ???
  val NEGATIVE_INFINITY: Number = ???
  val POSITIVE_INFINITY: Number = ???
}

/** Primitive JavaScript boolean. */
sealed trait Boolean extends Any {
  def ||(that: Boolean): Boolean

  def unary_!(): Boolean
}

/** The top-level `Boolean` JavaScript object. */
object Boolean extends Object {
  implicit def toBoolean(value: Boolean): scala.Boolean = sys.error("stub")
  
}


/** Primitive JavaScript string. */
sealed trait JsString extends Any {
  def +(that: Any): String
  override def +(that: String): String = sys.error("stub")
  override def +(that: Dynamic): JsString = sys.error("stub")

  def ||(that: String): String

  val length: Number = ???

  def charAt(pos: Number): String = ???
  def charCodeAt(index: Number): Number = ???
  def concat(strings: String*): String = ???
  def indexOf(searchString: String, position: Number): Number = ???
  def indexOf(searchString: String): Number = ???
  def lastIndexOf(searchString: String, position: Number): Number = ???
  def lastIndexOf(searchString: String): Number = ???
  def localeCompare(that: String): Number = ???
  def `match`(regexp: String): Array[String] = ???
  def `match`(regexp: RegExp): Array[String] = ???
  def replace(searchValue: String, replaceValue: String): String = ???
  def replace(searchValue: String, replaceValue: Any): String = ???
  def replace(searchValue: RegExp, replaceValue: String): String = ???
  def replace(searchValue: RegExp, replaceValue: Any): String = ???
  def search(regexp: String): Number = ???
  def search(regexp: RegExp): Number = ???
  def slice(start: Number, end: Number): String = ???
  def slice(start: Number): String = ???
  def split(separator: String, limit: Number): Array[String] = ???
  def split(separator: String): Array[String] = ???
  def split(separator: RegExp, limit: Number): Array[String] = ???
  def split(separator: RegExp): Array[String] = ???
  def substring(start: Number, end: Number): String = ???
  def substring(start: Number): String = ???
  def toLowerCase(): String = ???
  def toLocaleLowerCase(): String = ???
  def toUpperCase(): String = ???
  def toLocaleUpperCase(): String = ???
  def trim(): String = ???
}

object JsString{
  implicit def Stringify(s: String) = s.asInstanceOf[JsString]

}
/** The top-level `String` JavaScript object. */
object String extends Object {
  def fromCharCode(codes: Number*): JsString = ???
}

/** Primitive JavaScript undefined value. */
sealed trait Undefined extends Any with NotNull

/** Base class of all JavaScript objects. */
class Object extends Any {
  def this(value: Any) = this()

  def toLocaleString(): String = ???
  def valueOf(): Any = ???
  def hasOwnProperty(v: String): Boolean = ???
  def isPrototypeOf(v: Object): Boolean = ???
  def propertyIsEnumerable(v: String): Boolean = ???
}

/** The top-level `Object` JavaScript object. */
object Object extends Object {
  def apply(): Object = ???
  def apply(value: Any): Object = ???

  def getPrototypeOf(o: Object): Any = ???
  def getOwnPropertyDescriptor(o: Object, p: String): PropertyDescriptor = ???
  def getOwnPropertyNames(o: Object): Array[String] = ???

  def create(o: Object, properties: Any): Object = ???
  def create(o: Object): Object = ???

  def defineProperty(o: Object, p: String, attributes: PropertyDescriptor): o.type = ???
  def defineProperties(o: Object, properties: Any): o.type = ???

  def seal(o: Object): o.type = ???
  def freeze(o: Object): o.type = ???
  def preventExtensions(o: Object): o.type = ???

  def isSealed(o: Object): Boolean = ???
  def isFrozen(o: Object): Boolean = ???
  def isExtensible(o: Object): Boolean = ???

  def keys(o: Object): Array[String] = ???
}
