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

/**
 * All doc-comments marked as "MDN" are by Mozilla Contributors,
 * distributed under the Creative Commons Attribution-ShareAlike license from
 * https://developer.mozilla.org/en-US/docs/Web/Reference/API
 */
package scala.scalajs.js

import scala.scalajs.js
import scala.scalajs.js.annotation._

/**
 * The Function constructor creates a new Function object. In JavaScript every
 * function is actually a Function object.
 *
 * Function objects created with the Function constructor are parsed when the
 * function is created. This is less efficient than declaring a function and calling
 * it within your code, because functions declared with the function statement
 * are parsed with the rest of the code.
 *
 * All arguments passed to the function are treated as the names of the
 * identifiers of the parameters in the function to be created, in the order
 * in which they are passed.
 *
 * Note: Functions created with the Function constructor do not create closures
 * to their creation contexts; they always are created in the global scope.
 * When running them, they will only be able to access their own local
 * variables and global ones, not the ones from the scope in which the Function
 * constructor was called. This is different from using eval with code for
 * a function expression.
 *
 * Invoking the Function constructor as a function (without using the new
 * operator) has the same effect as invoking it as a constructor.
 *
 * MDN
 */
@js.native
@JSGlobal
class Function(args: String*) extends js.Object {
  /**
   * length is a property of a function object, and indicates how many arguments
   * the function expects, i.e. the number of formal parameters. This number
   * does not include the rest parameter. By contrast, arguments.length is local
   * to a function and provides the number of arguments actually passed to the
   * function.
   *
   * MDN
   */
  val length: Int = native

  /**
   * The call() method calls a function with a given this value and arguments
   * provided individually.
   *
   * You can assign a different this object when calling an existing function.
   * this refers to the current object, the calling object. With call, you
   * can write a method once and then inherit it in another object, without
   * having to rewrite the method for the new object.
   *
   * apply is very similar to call(), except for the type of arguments it supports.
   * You can use an arguments array instead of a named set of parameters. With
   * apply, you can use an array literal, for example,
   *
   * fun.apply(this, ['eat', 'bananas'])
   *
   * or an Array object, for example,
   *
   * fun.apply(this, new Array('eat', 'bananas')).
   *
   * MDN
   *
   * Scala.js-specific note: call() can be used instead of the apply() method
   * available in JavaScript. Simply use the :_* notation to expand a Seq as
   * variadic arguments, e.g.,
   *
   * {{{
   * someFun.call(thisArg, argSeq: _*)
   * }}}
   *
   */
  def call(thisArg: js.Any, argArray: js.Any*): js.Dynamic = js.native

  // Do not expose apply: use call(thisArg, argArray: _*) instead.
  // def apply[A](thisArg: Any, argArray: Array[A]): Dynamic = native
  // def apply(thisArg: Any): Dynamic = native

  /**
   * The bind() method creates a new function that, when called, has its this
   * keyword set to the provided value, with a given sequence of arguments
   * preceding any provided when the new function is called.
   *
   * MDN
   */
  def bind(thisArg: js.Any, argArray: js.Any*): js.Dynamic = js.native
}

@js.native
@JSGlobal
object Function extends js.Object {
  def apply(args: String*): js.Function = js.native
}

@js.native
trait Function0[+R] extends js.Function {
  def apply(): R
}

@js.native
trait Function1[-T1, +R] extends js.Function {
  def apply(arg1: T1): R
}

@js.native
trait Function2[-T1, -T2, +R] extends js.Function {
  def apply(arg1: T1, arg2: T2): R
}
