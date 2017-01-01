/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


/**
 * All doc-comments marked as "MDN" are by Mozilla Contributors,
 * distributed under the Creative Commons Attribution-ShareAlike license from
 * https://developer.mozilla.org/en-US/docs/Web/Reference/API
 */
package scala.scalajs.js

import scala.language.implicitConversions

/** A JavaScript function where `this` is considered as a first parameter.
 *
 *  @see [[http://www.scala-js.org/doc/calling-javascript.html Calling JavaScript from Scala.js]]
 */
@native
trait ThisFunction extends Function

object ThisFunction {
  // scalastyle:off line.size.limit

  /* identity() is important! It prevents the tail-rec treatment in the absence
   * of SAM treatment. See js.Any.fromFunctionN for more details.
   */
  implicit def fromFunction1[T1, R](f: scala.Function1[T1, R]): ThisFunction0[T1, R] = identity((x1: T1) => f(x1))
  implicit def fromFunction2[T1, T2, R](f: scala.Function2[T1, T2, R]): ThisFunction1[T1, T2, R] = identity((x1: T1, x2: T2) => f(x1, x2))
  implicit def fromFunction3[T1, T2, T3, R](f: scala.Function3[T1, T2, T3, R]): ThisFunction2[T1, T2, T3, R] = identity((x1: T1, x2: T2, x3: T3) => f(x1, x2, x3))
  implicit def fromFunction4[T1, T2, T3, T4, R](f: scala.Function4[T1, T2, T3, T4, R]): ThisFunction3[T1, T2, T3, T4, R] = identity((x1: T1, x2: T2, x3: T3, x4: T4) => f(x1, x2, x3, x4))
  implicit def fromFunction5[T1, T2, T3, T4, T5, R](f: scala.Function5[T1, T2, T3, T4, T5, R]): ThisFunction4[T1, T2, T3, T4, T5, R] = identity((x1: T1, x2: T2, x3: T3, x4: T4, x5: T5) => f(x1, x2, x3, x4, x5))
  implicit def fromFunction6[T1, T2, T3, T4, T5, T6, R](f: scala.Function6[T1, T2, T3, T4, T5, T6, R]): ThisFunction5[T1, T2, T3, T4, T5, T6, R] = identity((x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6) => f(x1, x2, x3, x4, x5, x6))
  implicit def fromFunction7[T1, T2, T3, T4, T5, T6, T7, R](f: scala.Function7[T1, T2, T3, T4, T5, T6, T7, R]): ThisFunction6[T1, T2, T3, T4, T5, T6, T7, R] = identity((x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7) => f(x1, x2, x3, x4, x5, x6, x7))
  implicit def fromFunction8[T1, T2, T3, T4, T5, T6, T7, T8, R](f: scala.Function8[T1, T2, T3, T4, T5, T6, T7, T8, R]): ThisFunction7[T1, T2, T3, T4, T5, T6, T7, T8, R] = identity((x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8) => f(x1, x2, x3, x4, x5, x6, x7, x8))
  implicit def fromFunction9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](f: scala.Function9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R]): ThisFunction8[T1, T2, T3, T4, T5, T6, T7, T8, T9, R] = identity((x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9))
  implicit def fromFunction10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](f: scala.Function10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]): ThisFunction9[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R] = identity((x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10))
  implicit def fromFunction11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](f: scala.Function11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]): ThisFunction10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R] = identity((x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11))
  implicit def fromFunction12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](f: scala.Function12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]): ThisFunction11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R] = identity((x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12))
  implicit def fromFunction13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](f: scala.Function13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]): ThisFunction12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R] = identity((x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13))
  implicit def fromFunction14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](f: scala.Function14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]): ThisFunction13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R] = identity((x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14))
  implicit def fromFunction15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](f: scala.Function15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]): ThisFunction14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R] = identity((x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14, x15: T15) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15))
  implicit def fromFunction16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](f: scala.Function16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]): ThisFunction15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R] = identity((x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14, x15: T15, x16: T16) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16))
  implicit def fromFunction17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](f: scala.Function17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]): ThisFunction16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R] = identity((x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14, x15: T15, x16: T16, x17: T17) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17))
  implicit def fromFunction18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](f: scala.Function18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]): ThisFunction17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R] = identity((x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14, x15: T15, x16: T16, x17: T17, x18: T18) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18))
  implicit def fromFunction19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](f: scala.Function19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]): ThisFunction18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R] = identity((x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14, x15: T15, x16: T16, x17: T17, x18: T18, x19: T19) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19))
  implicit def fromFunction20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](f: scala.Function20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R]): ThisFunction19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R] = identity((x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14, x15: T15, x16: T16, x17: T17, x18: T18, x19: T19, x20: T20) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20))
  implicit def fromFunction21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](f: scala.Function21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R]): ThisFunction20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R] = identity((x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14, x15: T15, x16: T16, x17: T17, x18: T18, x19: T19, x20: T20, x21: T21) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21))
  implicit def fromFunction22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](f: scala.Function22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R]): ThisFunction21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R] = identity((x1: T1, x2: T2, x3: T3, x4: T4, x5: T5, x6: T6, x7: T7, x8: T8, x9: T9, x10: T10, x11: T11, x12: T12, x13: T13, x14: T14, x15: T15, x16: T16, x17: T17, x18: T18, x19: T19, x20: T20, x21: T21, x22: T22) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22))

  implicit def toFunction1[T1, R](f: ThisFunction0[T1, R]): scala.Function1[T1, R] = (x1) => f(x1)
  implicit def toFunction2[T1, T2, R](f: ThisFunction1[T1, T2, R]): scala.Function2[T1, T2, R] = (x1, x2) => f(x1, x2)
  implicit def toFunction3[T1, T2, T3, R](f: ThisFunction2[T1, T2, T3, R]): scala.Function3[T1, T2, T3, R] = (x1, x2, x3) => f(x1, x2, x3)
  implicit def toFunction4[T1, T2, T3, T4, R](f: ThisFunction3[T1, T2, T3, T4, R]): scala.Function4[T1, T2, T3, T4, R] = (x1, x2, x3, x4) => f(x1, x2, x3, x4)
  implicit def toFunction5[T1, T2, T3, T4, T5, R](f: ThisFunction4[T1, T2, T3, T4, T5, R]): scala.Function5[T1, T2, T3, T4, T5, R] = (x1, x2, x3, x4, x5) => f(x1, x2, x3, x4, x5)
  implicit def toFunction6[T1, T2, T3, T4, T5, T6, R](f: ThisFunction5[T1, T2, T3, T4, T5, T6, R]): scala.Function6[T1, T2, T3, T4, T5, T6, R] = (x1, x2, x3, x4, x5, x6) => f(x1, x2, x3, x4, x5, x6)
  implicit def toFunction7[T1, T2, T3, T4, T5, T6, T7, R](f: ThisFunction6[T1, T2, T3, T4, T5, T6, T7, R]): scala.Function7[T1, T2, T3, T4, T5, T6, T7, R] = (x1, x2, x3, x4, x5, x6, x7) => f(x1, x2, x3, x4, x5, x6, x7)
  implicit def toFunction8[T1, T2, T3, T4, T5, T6, T7, T8, R](f: ThisFunction7[T1, T2, T3, T4, T5, T6, T7, T8, R]): scala.Function8[T1, T2, T3, T4, T5, T6, T7, T8, R] = (x1, x2, x3, x4, x5, x6, x7, x8) => f(x1, x2, x3, x4, x5, x6, x7, x8)
  implicit def toFunction9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](f: ThisFunction8[T1, T2, T3, T4, T5, T6, T7, T8, T9, R]): scala.Function9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9)
  implicit def toFunction10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](f: ThisFunction9[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]): scala.Function10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  implicit def toFunction11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](f: ThisFunction10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]): scala.Function11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  implicit def toFunction12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](f: ThisFunction11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]): scala.Function12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)
  implicit def toFunction13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](f: ThisFunction12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]): scala.Function13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13)
  implicit def toFunction14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](f: ThisFunction13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]): scala.Function14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14)
  implicit def toFunction15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](f: ThisFunction14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]): scala.Function15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
  implicit def toFunction16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](f: ThisFunction15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]): scala.Function16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16)
  implicit def toFunction17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](f: ThisFunction16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]): scala.Function17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17)
  implicit def toFunction18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](f: ThisFunction17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]): scala.Function18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18)
  implicit def toFunction19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](f: ThisFunction18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]): scala.Function19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19)
  implicit def toFunction20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](f: ThisFunction19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R]): scala.Function20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20)
  implicit def toFunction21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](f: ThisFunction20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R]): scala.Function21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21)
  implicit def toFunction22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](f: ThisFunction21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R]): scala.Function22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R] = (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22) => f(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22)
  // scalastyle:on line.size.limit
}

@native
trait ThisFunction0[-T0, +R] extends ThisFunction {
  def apply(thisArg: T0): R
}

@native
trait ThisFunction1[-T0, -T1, +R] extends ThisFunction {
  def apply(thisArg: T0, arg1: T1): R
}

@native
trait ThisFunction2[-T0, -T1, -T2, +R] extends ThisFunction {
  def apply(thisArg: T0, arg1: T1, arg2: T2): R
}
