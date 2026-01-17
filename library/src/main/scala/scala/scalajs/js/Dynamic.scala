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

/** All doc-comments marked as "MDN" are by Mozilla Contributors,
 *  distributed under the Creative Commons Attribution-ShareAlike license from
 *  https://developer.mozilla.org/en-US/docs/Web/Reference/API
 */
package scala.scalajs.js

import scala.language.dynamics

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** Dynamically typed JavaScript value.
 *
 *  Values of this trait accept all possible JavaScript operations in a
 *  dynamically typed way. You can read and write any field, call any method,
 *  apply any JavaScript operator to values of this type.
 */
@js.native
sealed trait Dynamic extends js.Any with scala.Dynamic {

  /** Calls a method of this object. */
  @JSBracketCall
  def applyDynamic(name: String)(args: js.Any*): js.Dynamic = js.native

  /** Reads a field of this object. */
  @JSBracketAccess
  def selectDynamic(name: String): js.Dynamic = js.native

  /** Writes a field of this object. */
  @JSBracketAccess
  def updateDynamic(name: String)(value: js.Any): Unit = js.native

  /** Calls this object as a callable. */
  def apply(args: js.Any*): js.Dynamic = js.native

  @JSOperator def unary_! : js.Dynamic = js.native // scalastyle:ignore

  @JSOperator def unary_+ : js.Dynamic = js.native // scalastyle:ignore
  @JSOperator def unary_- : js.Dynamic = js.native // scalastyle:ignore
  @JSOperator def unary_~ : js.Dynamic = js.native // scalastyle:ignore

  @JSOperator def +(that: js.Dynamic): js.Dynamic = js.native
  @JSOperator def -(that: js.Dynamic): js.Dynamic = js.native
  @JSOperator def *(that: js.Dynamic): js.Dynamic = js.native
  @JSOperator def /(that: js.Dynamic): js.Dynamic = js.native
  @JSOperator def %(that: js.Dynamic): js.Dynamic = js.native
  @JSOperator def <<(that: js.Dynamic): js.Dynamic = js.native
  @JSOperator def >>(that: js.Dynamic): js.Dynamic = js.native
  @JSOperator def >>>(that: js.Dynamic): js.Dynamic = js.native
  @JSOperator def &(that: js.Dynamic): js.Dynamic = js.native
  @JSOperator def |(that: js.Dynamic): js.Dynamic = js.native
  @JSOperator def ^(that: js.Dynamic): js.Dynamic = js.native

  @JSOperator def <(that: js.Dynamic): js.Dynamic = js.native
  @JSOperator def >(that: js.Dynamic): js.Dynamic = js.native
  @JSOperator def <=(that: js.Dynamic): js.Dynamic = js.native
  @JSOperator def >=(that: js.Dynamic): js.Dynamic = js.native

  @JSOperator def &&(that: js.Dynamic): js.Dynamic = js.native
  @JSOperator def ||(that: js.Dynamic): js.Dynamic = js.native

  /** <span class="badge badge-ecma2016" style="float: right;">ECMAScript 2016</span> */
  @JSOperator def **(that: js.Dynamic): js.Dynamic = js.native
}

/** Factory for dynamically typed JavaScript values. */
object Dynamic {

  /** Dynamic view of the global scope. */
  @js.native
  @JSGlobalScope
  object global extends js.Any with scala.Dynamic {

    /** Calls a top-level method (in the global scope). */
    @JSBracketCall
    def applyDynamic(name: String)(args: js.Any*): js.Dynamic = js.native

    /** Reads a top-level variable (in the global scope). */
    @JSBracketAccess
    def selectDynamic(name: String): js.Dynamic = js.native

    /** Writes to a top-level variable (in the global scope). */
    @JSBracketAccess
    def updateDynamic(name: String)(value: js.Any): Unit = js.native

    /* The following method is a protection against someone writing
     * `js.Dynamic.global(args)`. It that method were not there, that call
     * would silently desugar into
     * `js.Dynamic.global.applyDynamic("apply")(args)`, which is very
     * unexpected and will produce confusing run-time errors. Better to have
     * a straightforward compile-time error.
     */

    /** Cannot be called--provides a compile-time error instead of a silent
     *  run-time error if one tries to do `js.Dynamic.global(something)`.
     */
    @deprecated("The global scope cannot be called as function.", "forever")
    def apply(args: js.Any*): js.Dynamic = js.native
  }

  /** Instantiates a new object of a JavaScript class. */
  def newInstance(clazz: js.Dynamic)(args: js.Any*): js.Object with js.Dynamic =
    throw new java.lang.Error("stub")

  /** Creates a new object with a literal syntax.
   *
   *  For example,
   *  {{{
   *  js.Dynamic.literal(foo = 3, bar = "foobar")
   *  }}}
   *  returns the JavaScript object
   *  {{{
   *  {foo: 3, bar: "foobar"}
   *  }}}
   */
  object literal extends scala.Dynamic {

    /** Literal creation with named arguments.
     *
     *  Example:
     *  {{{
     *  js.Dynamic.literal(name1 = "value", name2 = "value")
     *  }}}
     */
    def applyDynamicNamed(name: String)(
        fields: (String, js.Any)*): js.Object with js.Dynamic = {
      js.special.objectLiteral(fields: _*).asInstanceOf[js.Object with js.Dynamic]
    }

    /* Note that the `def applyDynamic` could simply be `def apply`, but this
     * would make the `applyDynamicNamed` case fail, since a call with named
     * arguments would be routed to the `def apply`, rather than the dynamic
     * version.
     */

    /** Literal creation with tuples of key/value.
     *
     *  Example:
     *  {{{
     *  js.Dynamic.literal("name1" -> "value", "name2" -> "value")
     *  }}}
     */
    def applyDynamic(name: String)(
        fields: (String, js.Any)*): js.Object with js.Dynamic = {
      js.special.objectLiteral(fields: _*).asInstanceOf[js.Object with js.Dynamic]
    }
  }
}
