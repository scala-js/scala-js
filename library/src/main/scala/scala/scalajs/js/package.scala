/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.scalajs

import scala.annotation.unchecked.uncheckedVariance

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

  /** Value of type A or the JS undefined value.
   *
   *  This type is actually strictly equivalent to `A | Unit`, since `Unit` is
   *  the type of the `undefined` value.
   *
   *  `js.UndefOr[A]` is the type of a value that can be either `undefined` or
   *  an `A`. It provides an API similar to that of [[scala.Option]] through
   *  the [[UndefOrOps]] implicit class, where `undefined` take the role of
   *  [[None]].
   *
   *  By extension, this type is also suited to typing optional fields in
   *  native JS types, i.e., fields that may not exist on the object.
   */
  type UndefOr[+A] = (A @uncheckedVariance) | Unit

  /** The undefined value. */
  @inline def undefined: js.UndefOr[Nothing] =
    ().asInstanceOf[js.UndefOr[Nothing]]

  /** Tests whether the given value is undefined. */
  @inline def isUndefined(v: scala.Any): Boolean =
    v.asInstanceOf[scala.AnyRef] eq undefined

  /** Returns the type of `x` as identified by `typeof x` in JavaScript. */
  def typeOf(x: scala.Any): String = throw new java.lang.Error("stub")

  /** Returns the constructor function of a JavaScript class.
   *
   *  The specified type parameter `T` must be a class type (i.e., valid for
   *  `classOf[T]`) and represent a class extending `js.Any` (not a trait nor
   *  an object).
   */
  def constructorOf[T <: js.Any]: js.Dynamic = throw new java.lang.Error("stub")

  /** Makes explicit an implicitly available [[js.ConstructorTag]]. */
  def constructorTag[T <: js.Any](
      implicit tag: js.ConstructorTag[T]): js.ConstructorTag[T] = {
    tag
  }

  /** Evaluates JavaScript code and returns the result. */
  @inline def eval(x: String): scala.Any =
    js.Dynamic.global.eval(x)

  /** Marks the annotated class, trait or object as a native JS entity.
   *
   *  Native JS entities are not implemented in Scala.js. They are facade types
   *  for native JS libraries.
   *
   *  Only types extending [[Any js.Any]] can be annotated with `@js.native`.
   *  The body of all concrete members in a native JS class, trait or object
   *  must be `= js.native`.
   */
  class native extends scala.annotation.StaticAnnotation

  /** Denotes a method body as native JavaScript. For use in facade types:
   *
   *  {{{
   *  class MyJSClass extends js.Object {
   *    def myMethod(x: String): Int = js.native
   *  }
   *  }}}
   */
  def native: Nothing = {
    throw new java.lang.Error(
        "A method defined in a JavaScript raw type of a Scala.js library has " +
        "been called. This is most likely because you tried to run Scala.js " +
        "binaries on the JVM. Make sure you are using the JVM version of the " +
        "libraries.")
  }

}
