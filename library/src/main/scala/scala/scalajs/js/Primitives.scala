/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


/**
 * All doc-comments marked as "MDN" are by Mozilla Contributors,
 * distributed under the Creative Commons Attribution-ShareAlike license from
 * https://developer.mozilla.org/en-US/docs/Web/Reference/API
 */
package scala.scalajs.js

import annotation.JSBracketAccess

import scala.language.{ dynamics, implicitConversions }
import scala.reflect.ClassTag
import scala.collection.{ immutable, mutable }

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
  implicit def fromLong(value: scala.Long): Number = value.toDouble
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
  implicit def stringOps(string: String): immutable.StringOps =
    new immutable.StringOps(string: java.lang.String)
  implicit def richDouble(num: Number): scala.runtime.RichDouble =
    new scala.runtime.RichDouble(num: scala.Double)
  implicit def richBoolean(b: Boolean): scala.runtime.RichBoolean =
    new scala.runtime.RichBoolean(b: scala.Boolean)

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

  implicit def fromAny(value: Any): Dictionary = value.asInstanceOf[Dictionary]
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

  /**
   * Returns a string representation of number that does not use exponential
   * notation and has exactly digits digits after the decimal place. The number
   * is rounded if necessary, and the fractional part is padded with zeros if
   * necessary so that it has the specified length. If number is greater than
   * 1e+21, this method simply calls Number.prototype.toString() and returns
   * a string in exponential notation.
   *
   * MDN
   */
  def toFixed(fractionDigits: Number): String = ???
  def toFixed(): String = ???

  /**
   * Returns a string representing a Number object in exponential notation with one
   * digit before the decimal point, rounded to fractionDigits digits after the
   * decimal point. If the fractionDigits argument is omitted, the number of
   * digits after the decimal point defaults to the number of digits necessary
   * to represent the value uniquely.
   *
   * If a number has more digits that requested by the fractionDigits parameter,
   * the number is rounded to the nearest number represented by fractionDigits
   * digits. See the discussion of rounding in the description of the toFixed()
   * method, which also applies to toExponential().
   *
   * MDN
   */
  def toExponential(fractionDigits: Number): String = ???
  def toExponential(): String = ???

  /**
   * Returns a string representing a Number object in fixed-point or exponential
   * notation rounded to precision significant digits. See the discussion of
   * rounding in the description of the Number.prototype.toFixed() method, which
   * also applies to toPrecision.
   *
   * If the precision argument is omitted, behaves as Number.prototype.toString().
   * If it is a non-integer value, it is rounded to the nearest integer.
   *
   * MDN
   */
  def toPrecision(precision: Number): String = ???
  def toPrecision(): String = ???
}

/** The top-level `Number` JavaScript object */
object Number extends Object {
  implicit def toDouble(value: Number): scala.Double = sys.error("stub")

  /**
   * The Number.MAX_VALUE property represents the maximum numeric value
   * representable in JavaScript.
   *
   * The MAX_VALUE property has a value of approximately 1.79E+308. Values
   * larger than MAX_VALUE are represented as "Infinity".
   *
   * MDN
   */
  val MAX_VALUE: Number = ???
  /**
   * The Number.MIN_VALUE property represents the smallest positive numeric
   * value representable in JavaScript.
   *
   * The MIN_VALUE property is the number closest to 0, not the most negative
   * number, that JavaScript can represent.
   *
   * MIN_VALUE has a value of approximately 5e-324. Values smaller than MIN_VALUE
   * ("underflow values") are converted to 0.
   *
   * MDN
   */
  val MIN_VALUE: Number = ???
  /**
   * The Number.NaN property represents Not-A-Number. Equivalent of NaN.
   *
   * MDN
   */
  val NaN: Number = ???

  /**
   * The Number.NEGATIVE_INFINITY property represents the negative Infinity value.
   *
   * MDN
   */
  val NEGATIVE_INFINITY: Number = ???
  /**
   * The Number.POSITIVE_INFINITY property represents the positive Infinity value.
   *
   * MDN
   */
  val POSITIVE_INFINITY: Number = ???
}

/** Primitive JavaScript boolean. */
sealed trait Boolean extends Any {
  def ||(that: Boolean): Boolean
}

/** The top-level `Boolean` JavaScript object. */
object Boolean extends Object {
  implicit def toBoolean(value: Boolean): scala.Boolean = sys.error("stub")
}

/** Primitive JavaScript string. */
sealed trait String extends Any {
  def +(that: Any): String
  override def +(that: String): String = sys.error("stub")
  override def +(that: Dynamic): String = sys.error("stub")

  def ||(that: String): String

  /**
   * This property returns the number of code units in the string. UTF-16,
   * the string format used by JavaScript, uses a single 16-bit code unit to
   * represent the most common characters, but needs to use two code units for
   * less commonly-used characters, so it's possible for the value returned by
   * length to not match the actual number of characters in the string.
   *
   * For an empty string, length is 0.
   *
   * MDN
   */
  val length: Number = ???

  /**
   * The chartAt() method returns the specified character from a string.
   *
   * Characters in a string are indexed from left to right. The index of the
   * first character is 0, and the index of the last character in a string
   * called stringName is stringName.length - 1. If the index you supply is out
   * of range, JavaScript returns an empty string.
   *
   * MDN
   */
  def charAt(pos: Number): String = ???

  /**
   * The charCodeAt() method returns the numeric Unicode value of the character
   * at the given index (except for unicode codepoints > 0x10000).
   *
   * MDN
   */
  def charCodeAt(index: Number): Number = ???

  /**
   * concat combines the text from one or more strings and returns a new string.
   * Changes to the text in one string do not affect the other string.
   * MDN
   */
  def concat(strings: String*): String = ???

  /**
   * Returns the index within the calling String object of the first occurrence
   * of the specified value, starting the search at fromIndex,
   *
   * returns -1 if the value is not found.
   *
   * MDN
   */
  def indexOf(searchString: String, position: Number): Number = ???
  def indexOf(searchString: String): Number = ???

  /**
   * Returns the index within the calling String object of the last occurrence
   * of the specified value, or -1 if not found. The calling string is searched
   * backward, starting at fromIndex.
   *
   * MDN
   */
  def lastIndexOf(searchString: String, position: Number): Number = ???
  def lastIndexOf(searchString: String): Number = ???

  /**
   * Returns a number indicating whether a reference string comes before or
   * after or is the same as the given string in sort order. The new locales
   * and options arguments let applications specify the language whose sort
   * order should be used and customize the behavior of the function. In older
   * implementations, which ignore the locales and options arguments, the locale
   * and sort order used are entirely implementation dependent.
   *
   * MDN
   */
  def localeCompare(that: String): Number = ???

  /**
   * Used to retrieve the matches when matching a string against a regular
   * expression.
   *
   * If the regular expression does not include the g flag, returns the same
   * result as regexp.exec(string). The returned Array has an extra input
   * property, which contains the original string that was parsed. In addition,
   * it has an index property, which represents the zero-based index of the
   * match in the string.
   *
   * If the regular expression includes the g flag, the method returns an Array
   * containing all matches. If there were no matches, the method returns null.
   *
   * MDN
   */
  def `match`(regexp: String): Array[String] = ???
  def `match`(regexp: RegExp): Array[String] = ???

  /**
   * Returns a new string with some or all matches of a pattern replaced by a
   * replacement.  The pattern can be a string or a RegExp, and the replacement
   * can be a string or a function to be called for each match.
   *
   * This method does not change the String object it is called on. It simply
   * returns a new string.
   *
   * To perform a global search and replace, either include the g switch in the
   * regular expression or if the first parameter is a string, include g in the
   * flags parameter.
   *
   * MDN
   */
  def replace(searchValue: String, replaceValue: String): String = ???
  def replace(searchValue: String, replaceValue: Any): String = ???
  def replace(searchValue: RegExp, replaceValue: String): String = ???
  def replace(searchValue: RegExp, replaceValue: Any): String = ???

  /**
   * If successful, search returns the index of the regular expression inside
   * the string. Otherwise, it returns -1.
   *
   * When you want to know whether a pattern is found in a string use search
   * (similar to the regular expression test method); for more information
   * (but slower execution) use match (similar to the regular expression exec
   * method).
   *
   * MDN
   */
  def search(regexp: String): Number = ???
  def search(regexp: RegExp): Number = ???

  /**
   * slice extracts the text from one string and returns a new string. Changes
   * to the text in one string do not affect the other string.
   *
   * slice extracts up to but not including endSlice. string.slice(1,4) extracts
   * the second character through the fourth character (characters indexed 1, 2,
   * and 3).
   *
   * As an example, string.slice(2,-1) extracts the third character through the
   * second to last character in the string.
   *
   * MDN
   */
  def slice(start: Number, end: Number): String = ???
  def slice(start: Number): String = ???

  /**
   * Splits a String object into an array of strings by separating the string
   * into substrings.
   *
   * When found, separator is removed from the string and the substrings are
   * returned in an array. If separator is omitted, the array contains one
   * element consisting of the entire string. If separator is an empty string,
   * string is converted to an array of characters.
   *
   * If separator is a regular expression that contains capturing parentheses,
   * then each time separator is matched, the results (including any undefined
   * results) of the capturing parentheses are spliced into the output array.
   * However, not all browsers support this capability.
   *
   * Note: When the string is empty, split returns an array containing one
   * empty string, rather than an empty array.
   *
   * MDN
   */
  def split(separator: String, limit: Number): Array[String] = ???
  def split(separator: String): Array[String] = ???
  def split(separator: RegExp, limit: Number): Array[String] = ???
  def split(separator: RegExp): Array[String] = ???

  /**
   * Returns a subset of a string between one index and another, or through
   * the end of the string.
   *
   * MDN
   */
  def substring(start: Number, end: Number): String = ???
  def substring(start: Number): String = ???

  /**
   * Returns the calling string value converted to lowercase.
   *
   * MDN
   */
  def toLowerCase(): String = ???

  /**
   * The toLocaleLowerCase method returns the value of the string converted to
   * lower case according to any locale-specific case mappings. toLocaleLowerCase
   * does not affect the value of the string itself. In most cases, this will
   * produce the same result as toLowerCase(), but for some locales, such as
   * Turkish, whose case mappings do not follow the default case mappings in Unicode,
   * there may be a different result.
   *
   * MDN
   */
  def toLocaleLowerCase(): String = ???

  /**
   * Returns the calling string value converted to uppercase.
   *
   * MDN
   */
  def toUpperCase(): String = ???

  /**
   * The toLocaleUpperCase method returns the value of the string converted to
   * upper case according to any locale-specific case mappings. toLocaleUpperCase
   * does not affect the value of the string itself. In most cases, this will
   * produce the same result as toUpperCase(), but for some locales, such as
   * Turkish, whose case mappings do not follow the default case mappings in Unicode,
   * there may be a different result.
   *
   * MDN
   */
  def toLocaleUpperCase(): String = ???

  /**
   * Removes whitespace from both ends of the string.
   *
   * MDN
   */
  def trim(): String = ???
}

/** The top-level `String` JavaScript object. */
object String extends Object {
  implicit def toScalaString(value: String): java.lang.String = sys.error("stub")

  def fromCharCode(codes: Number*): String = ???
}

/** Primitive JavaScript undefined value. */
sealed trait Undefined extends Any

/** Base class of all JavaScript objects. */
class Object extends Any {
  def this(value: Any) = this()

  def toLocaleString(): String = ???
  def valueOf(): Any = ???

  /**
   * Every object descended from Object inherits the hasOwnProperty method.
   * This method can be used to determine whether an object has the specified
   * property as a direct property of that object; unlike the in operator,
   * this method does not check down the object's prototype chain.
   *
   * MDN
   */
  def hasOwnProperty(v: String): Boolean = ???

  /**
   * The isPrototypeOf mehtod allows you to check whether or not an object exists
   * within another object's prototype chain.
   *
   * MDN
   */
  def isPrototypeOf(v: Object): Boolean = ???

  /**
   * Every object has a propertyIsEnumerable method. This method can determine
   * whether the specified property in an object can be enumerated by a for...in
   * loop, with the exception of properties inherited through the prototype
   * chain. If the object does not have the specified property, this method
   * returns false.
   *
   * MDN
   */
  def propertyIsEnumerable(v: String): Boolean = ???
}

/** The top-level `Object` JavaScript object. */
object Object extends Object {
  def apply(): Object = ???
  def apply(value: Any): Object = ???

  /**
   * The Object.getPrototypeOf() method returns the prototype (i.e. the
   * internal [[Prototype]]) of the specified object.
   *
   * MDN
   */
  def getPrototypeOf(o: Object): Any = ???

  /**
   * The Object.getOwnPropertyDescriptor() method returns a property descriptor
   * for an own property (that is, one directly present on an object, not
   * present by dint of being along an object's prototype chain) of a given object.
   *
   * MDN
   */
  def getOwnPropertyDescriptor(o: Object, p: String): PropertyDescriptor = ???

  /**
   * Object.getOwnPropertyNames returns an array whose elements are strings
   * corresponding to the enumerable and non-enumerable properties found
   * directly upon obj. The ordering of the enumerable properties in the array
   * is consistent with the ordering exposed by a for...in loop (or by Object.keys)
   * over the properties of the object. The ordering of the non-enumerable
   * properties in the array, and among the enumerable properties, is not defined.
   *
   * MDN
   */
  def getOwnPropertyNames(o: Object): Array[String] = ???

  /**
   * The Object.create() method creates a new object with the specified
   * prototype object and properties.
   *
   * MDN
   */
  def create(o: Object, properties: Any): Object = ???
  def create(o: Object): Object = ???

  /**
   * The Object.defineProperty() method defines a new property directly on an
   * object, or modifies an existing property on an object, and returns the
   * object.
   *
   * This method allows precise addition to or modification of a property on an
   * object. Normal property addition through assignment creates properties
   * which show up during property enumeration (for...in loop or Object.keys method),
   * whose values may be changed, and which may be deleted. This method allows
   * these extra details to be changed from their defaults.
   *
   * Property descriptors present in objects come in two main flavors: data
   * descriptors and accessor descriptors. A data descriptor is a property
   * that has a value, which may or may not be writable. An accessor descriptor
   * is a property described by a getter-setter pair of functions. A descriptor
   * must be one of these two flavors; it cannot be both.
   *
   * MDN
   */
  def defineProperty(o: Object, p: String, attributes: PropertyDescriptor): o.type = ???

  /**
   * The Object.defineProperties() method defines new or modifies existing
   * properties directly on an object, returning the object.
   *
   * MDN
   */
  def defineProperties(o: Object, properties: Any): o.type = ???

  /**
   * The Object.seal() method seals an object, preventing new properties from
   * being added to it and marking all existing properties as non-configurable.
   * Values of present properties can still be changed as long as they are
   * writable.
   *
   * MDN
   */
  def seal(o: Object): o.type = ???

  /**
   * The Object.freeze() method freezes an object: that is, prevents new properties
   * from being added to it; prevents existing properties from being removed;
   * and prevents existing properties, or their enumerability, configurability,
   * or writability, from being changed. In essence the object is made effectively
   * immutable. The method returns the object being frozen.
   *
   * MDN
   */
  def freeze(o: Object): o.type = ???

  /**
   * The Object.preventExtensions() method prevents new properties from ever
   * being added to an object (i.e. prevents future extensions to the object).
   *
   * An object is extensible if new properties can be added to it.  preventExtensions
   * marks an object as no longer extensible, so that it will never have
   * properties beyond the ones it had at the time it was marked as non-extensible.
   * Note that the properties of a non-extensible object, in general, may still be
   * deleted. Attempting to add new properties to a non-extensible object will
   * fail, either silently or by throwing a TypeError (most commonly, but not
   * exclusively, when in strict mode).
   *
   * Object.preventExtensions only prevents addition of own properties. Properties
   * can still be added to the object prototype. However, calling Object.preventExtensions
   * on an object will also prevent extensions on its __proto__ property.
   *
   * MDN
   */
  def preventExtensions(o: Object): o.type = ???

  /**
   * Returns true if the object is sealed, otherwise false. An object is sealed
   * if it is not extensible and if all its properties are non-configurable and
   * therefore not removable (but not necessarily non-writable).
   *
   * MDN
   */
  def isSealed(o: Object): Boolean = ???

  /**
   * The Object.isFrozen() determines if an object is frozen.
   *
   * An object is frozen if and only if it is not extensible, all its properties
   * are non-configurable, and all its data properties (that is, properties which
   * are not accessor properties with getter or setter components) are non-writable.
   *
   * MDN
   */
  def isFrozen(o: Object): Boolean = ???

  /**
   * Determines if extending of an object is allowed
   *
   * Objects are extensible by default: they can have new properties added to
   * them, and (in engines that support __proto__  their __proto__ property)
   * can be modified. An object can be marked as non-extensible using
   * Object.preventExtensions, Object.seal, or Object.freeze
   *
   * MDN
   */
  def isExtensible(o: Object): Boolean = ???

  /**
   * The Object.keys() method returns an array of a given object's own enumerable
   * properties, in the same order as that provided by a for...in loop (the
   * difference being that a for-in loop enumerates properties in the prototype
   * chain as well).
   *
   * MDN
   */
  def keys(o: Object): Array[String] = ???
}
