/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */



package scala.scalajs

// Can't link to Null - #1969
/** Types, methods and values for interoperability with JavaScript libraries.
 *
 *  This package is only relevant to the Scala.js compiler, and should not be
 *  referenced by any project compiled to the JVM.
 *
 *  == Guide ==
 *
 *  General documentation on Scala.js is available at
 *  [[http://www.scala-js.org/doc/]].
 *
 *  == Overview ==
 *
 *  The trait [[js.Any]] is the root of the hierarchy of JavaScript types.
 *  This package defines important subtypes of [[js.Any]] that are defined
 *  in the standard library of ECMAScript 5.1 (or ES 6, with a label in the
 *  documentation), such as [[Object js.Object]], [[Array js.Array]] and
 *  [[RegExp js.RegExp]].
 *
 *  Implicit conversions to and from standard Scala types to their equivalent
 *  in JavaScript are provided. For example, from Scala functions to JavaScript
 *  functions and back.
 *
 *  The most important subtypes of [[js.Any]] declared in this package are:
 *  - [[Object js.Object]], the superclass of most (all) JavaScript classes
 *  - [[Array js.Array]]
 *  - [[Function js.Function]] (and subtraits with specific number of
 *    parameters)
 *  - [[ThisFunction js.ThisFunction]] and its subtraits for functions that
 *    take the JavaScript `this` as an explicit parameter
 *  - [[Dictionary js.Dictionary]], a [[scala.collection.Map Map]]-like view
 *    of the properties of a JS object
 *
 *  The trait [[js.Dynamic]] is a special subtrait of [[js.Any]]. It can
 *  represent any JavaScript value in a dynamically-typed way. It is possible
 *  to call any method and read and write any field of a value of type
 *  [[js.Dynamic]].
 *
 *  There are no explicit definitions for JavaScript primitive types, as one
 *  could expect, because the corresponding Scala types stand in their stead:
 *  - [[Boolean]] is the type of primitive JavaScript booleans
 *  - [[Double]] is the type of primitive JavaScript numbers
 *  - [[java.lang.String String]] is the type of primitive JavaScript strings (or `null`)
 *  - [[Unit]] is the type of the JavaScript undefined value
 *  - `Null` is the type of the JavaScript null value
 *
 *  [[UndefOr js.UndefOr]] gives a [[scala.Option]]-like interface where the
 *  JavaScript value `undefined` takes the role of `None`.
 *
 *  [[| A | B]] is an unboxed pseudo-union type, suitable to type values that
 *  admit several unrelated types in facade types.
 */
package object js {
  /** The undefined value. */
  @inline def undefined: js.UndefOr[Nothing] =
    ().asInstanceOf[js.UndefOr[Nothing]]

  /** Tests whether the given value is undefined. */
  @inline def isUndefined(v: scala.Any): Boolean =
    v.asInstanceOf[scala.AnyRef] eq undefined

  /** Returns the type of `x` as identified by `typeof x` in JavaScript. */
  def typeOf(x: Any): String = sys.error("stub")

  /** Returns the constructor function of a JavaScript class.
   *
   *  The specified type parameter `T` must be a class type (i.e., valid for
   *  `classOf[T]`) and represent a class extending `js.Any` (not a trait nor
   *  an object).
   */
  def constructorOf[T <: js.Any]: js.Dynamic = sys.error("stub")

  /** Makes explicit an implicitly available `ConstructorTag[T]`. */
  def constructorTag[T <: js.Any](implicit tag: ConstructorTag[T]): ConstructorTag[T] =
    tag

  /** Invokes any available debugging functionality.
   *  If no debugging functionality is available, this statement has no effect.
   *
   *  MDN
   *
   *  Browser support:
   *  - Has no effect in Firefox, apparently
   *  - In Chrome, it has no effect unless the developer tools are opened
   *    beforehand.
   */
  def debugger(): Unit = sys.error("stub")

  /** Evaluates JavaScript code and returns the result. */
  @inline def eval(x: String): Any =
    js.Dynamic.global.eval(x)

  /** Marks the annotated class, trait or object as a native JS entity.
   *
   *  Native JS entities are not implemented in Scala.js. They are facade types
   *  for native JS libraries.
   *
   *  In Scala.js 0.6.x, all types extending [[Any js.Any]] are native by
   *  default (unless they are annotated with [[annotation.ScalaJSDefined]]),
   *  but this will not be the case in the next major version anymore.
   *
   *  Only types extending [[Any js.Any]] can be annotated with `@js.native`.
   *  The body of all concrete members in a native JS class, trait or object
   *  must be `= js.native`.
   */
  class native extends scala.annotation.StaticAnnotation // scalastyle:ignore

  /** Denotes a method body as native JavaScript. For use in facade types:
   *
   *  {{{
   *  class MyJSClass extends js.Object {
   *    def myMethod(x: String): Int = js.native
   *  }
   *  }}}
   */
  def native: Nothing = sys.error("A method defined in a JavaScript raw " +
      "type of a Scala.js library has been called. This is most likely " +
      "because you tried to run Scala.js binaries on the JVM. Make sure you " +
      "are using the JVM version of the libraries.")

}
