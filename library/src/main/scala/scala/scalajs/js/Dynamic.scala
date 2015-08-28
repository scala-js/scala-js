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

import scala.language.dynamics

import annotation.{JSBracketAccess, JSBracketCall}

/** Dynamically typed JavaScript value.
 *
 *  Values of this trait accept all possible JavaScript operations in a
 *  dynamically typed way. You can read and write any field, call any method,
 *  apply any JavaScript operator to values of this type.
 */
@native
sealed trait Dynamic extends Any with scala.Dynamic {
  /** Calls a method of this object. */
  @JSBracketCall
  def applyDynamic(name: String)(args: Any*): Dynamic = native

  /** Reads a field of this object. */
  @JSBracketAccess
  def selectDynamic(name: String): Dynamic = native

  /** Writes a field of this object. */
  @JSBracketAccess
  def updateDynamic(name: String)(value: Any): Unit = native

  /** Calls this object as a callable. */
  def apply(args: Any*): Dynamic = native

  def unary_!(): Dynamic = native

  def unary_+(): Dynamic = native
  def unary_-(): Dynamic = native
  def unary_~(): Dynamic = native

  def +(that: Dynamic): Dynamic = native
  def -(that: Dynamic): Dynamic = native
  def *(that: Dynamic): Dynamic = native
  def /(that: Dynamic): Dynamic = native
  def %(that: Dynamic): Dynamic = native
  def <<(that: Dynamic): Dynamic = native
  def >>(that: Dynamic): Dynamic = native
  def >>>(that: Dynamic): Dynamic = native
  def &(that: Dynamic): Dynamic = native
  def |(that: Dynamic): Dynamic = native
  def ^(that: Dynamic): Dynamic = native

  def <(that: Dynamic): Dynamic = native
  def >(that: Dynamic): Dynamic = native
  def <=(that: Dynamic): Dynamic = native
  def >=(that: Dynamic): Dynamic = native

  def &&(that: Dynamic): Dynamic = native
  def ||(that: Dynamic): Dynamic = native

  // Work around the annoying implicits in Predef in Scala 2.10.
  def x: Dynamic = native
  def x_=(value: Any): Unit = native
}

/** Factory for dynamically typed JavaScript values. */
object Dynamic {
  /** Dynamic view of the global scope. */
  @inline def global: Dynamic = scala.scalajs.runtime.environmentInfo.global

  /** Instantiates a new object of a JavaScript class. */
  def newInstance(clazz: Dynamic)(args: Any*): Object with Dynamic = sys.error("stub")

  /** Creates a new object with a literal syntax.
   *
   *  For example,
   *    js.Dynamic.literal(foo = 3, bar = "foobar")
   *  returns the JavaScript object
   *    {foo: 3, bar: "foobar"}
   */
  object literal extends scala.Dynamic { // scalastyle:ignore
    /** literal creation like this:
     *  js.Dynamic.literal(name1 = "value", name2 = "value")
     */
    def applyDynamicNamed(name: String)(
        fields: (String, Any)*): Object with Dynamic = sys.error("stub")

    /** literal creation like this:
     *  js.Dynamic.literal("name1" -> "value", "name2" -> "value")
     *
     *  Note that this could be simply `def apply`, but this would make the
     *  applyDynamicNamed fail, since a call with named arguments would
     *  be routed to the `def apply`, rather than def dynamic version.
     */
    def applyDynamic(name: String)(
        fields: (String, Any)*): Object with Dynamic = sys.error("stub")

  }
}
