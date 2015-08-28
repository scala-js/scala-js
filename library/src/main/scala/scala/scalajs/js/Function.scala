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
@native
class Function(args: String*) extends Object {
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
  def call(thisArg: Any, argArray: Any*): Dynamic = native

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
  def bind(thisArg: Any, argArray: Any*): Dynamic = native
}

@native
object Function extends Object {
  def apply(args: String*): Function = native
}

// scalastyle:off line.size.limit

@native
trait Function0[+R] extends Function {
  def apply(): R
}

@native
trait Function1[-T1, +R] extends Function {
  def apply(arg1: T1): R
}

@native
trait Function2[-T1, -T2, +R] extends Function {
  def apply(arg1: T1, arg2: T2): R
}

@native
trait Function3[-T1, -T2, -T3, +R] extends Function {
  def apply(arg1: T1, arg2: T2, arg3: T3): R
}

@native
trait Function4[-T1, -T2, -T3, -T4, +R] extends Function {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4): R
}

@native
trait Function5[-T1, -T2, -T3, -T4, -T5, +R] extends Function {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5): R
}

@native
trait Function6[-T1, -T2, -T3, -T4, -T5, -T6, +R] extends Function {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6): R
}

@native
trait Function7[-T1, -T2, -T3, -T4, -T5, -T6, -T7, +R] extends Function {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7): R
}

@native
trait Function8[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, +R] extends Function {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8): R
}

@native
trait Function9[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, +R] extends Function {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9): R
}

@native
trait Function10[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, +R] extends Function {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10): R
}

@native
trait Function11[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, +R] extends Function {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11): R
}

@native
trait Function12[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, +R] extends Function {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12): R
}

@native
trait Function13[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, +R] extends Function {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13): R
}

@native
trait Function14[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, +R] extends Function {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14): R
}

@native
trait Function15[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, +R] extends Function {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15): R
}

@native
trait Function16[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, +R] extends Function {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16): R
}

@native
trait Function17[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, +R] extends Function {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16, arg17: T17): R
}

@native
trait Function18[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, +R] extends Function {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16, arg17: T17, arg18: T18): R
}

@native
trait Function19[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, +R] extends Function {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16, arg17: T17, arg18: T18, arg19: T19): R
}

@native
trait Function20[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, +R] extends Function {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16, arg17: T17, arg18: T18, arg19: T19, arg20: T20): R
}

@native
trait Function21[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, -T21, +R] extends Function {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16, arg17: T17, arg18: T18, arg19: T19, arg20: T20, arg21: T21): R
}

@native
trait Function22[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, -T21, -T22, +R] extends Function {
  def apply(arg1: T1, arg2: T2, arg3: T3, arg4: T4, arg5: T5, arg6: T6, arg7: T7, arg8: T8, arg9: T9, arg10: T10, arg11: T11, arg12: T12, arg13: T13, arg14: T14, arg15: T15, arg16: T16, arg17: T17, arg18: T18, arg19: T19, arg20: T20, arg21: T21, arg22: T22): R
}

// scalastyle:on line.size.limit
