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

import scala.scalajs.js.annotation.JSName
import scala.language.implicitConversions


// scalastyle:off line.size.limit

/**
 *  A tuple "view" of 2 elements of a JavaScript [[Array]].
 *  Combines
 *  {{{
 *  0: T0;
 *  1: T1;
 *  }}}
 *  to
 *  {{{
 *  js.Tuple2[T0,T1]
 *  }}}
 *
 *  Supports implicit conversion to [[scala.Tuple2]].
 *  To use it, cast your array into a [[Tuple2]] using
 *  {{{
 *  val array = js.Array[Any](42, "foobar")
 *  val tuple2 = array.asInstanceOf[js.Tuple2[Int, String]]
 *  }}}
 *  or convert a Scala tuple
 *  {{{
 *  val obj: js.Tuple2[Int, String] = (42, "foobar")
 *  }}}
 */
@native
sealed trait Tuple2[+T1, +T2] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
}

object Tuple2 {
  @inline def apply[T1, T2](_1: T1, _2: T2): Tuple2[T1, T2] =
    Array(_1, _2).asInstanceOf[Tuple2[T1, T2]]

  @inline def unapply[T1, T2](t: Tuple2[T1, T2]): Option[(T1, T2)] =
    Some(t)

  @inline implicit def fromScalaTuple2[T1, T2](t: (T1, T2)): Tuple2[T1, T2] =
    apply(t._1, t._2)

  @inline implicit def toScalaTuple2[T1, T2](t: Tuple2[T1, T2]): (T1, T2) =
    (t._1, t._2)
}

/**
 *  A tuple "view" of 3 elements of a JavaScript [[Array]].
 *
 *  @see [[Tuple2]]
 */
@native
sealed trait Tuple3[+T1, +T2, +T3] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
  @JSName("2") val _3: T3 = native
}

object Tuple3 {
  @inline def apply[T1, T2, T3](_1: T1, _2: T2, _3: T3): Tuple3[T1, T2, T3] =
    Array(_1, _2, _3).asInstanceOf[Tuple3[T1, T2, T3]]

  @inline def unapply[T1, T2, T3](t: Tuple3[T1, T2, T3]): Option[(T1, T2, T3)] =
    Some(t)

  @inline implicit def fromScalaTuple3[T1, T2, T3](t: (T1, T2, T3)): Tuple3[T1, T2, T3] =
    apply(t._1, t._2, t._3)

  @inline implicit def toScalaTuple3[T1, T2, T3](t: Tuple3[T1, T2, T3]): (T1, T2, T3) =
    (t._1, t._2, t._3)
}

/**
 *  A tuple "view" of 4 elements of a JavaScript [[Array]].
 *
 *  @see [[Tuple2]]
 */
@native
sealed trait Tuple4[+T1, +T2, +T3, +T4] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
  @JSName("2") val _3: T3 = native
  @JSName("3") val _4: T4 = native
}

object Tuple4 {
  @inline def apply[T1, T2, T3, T4](_1: T1, _2: T2, _3: T3, _4: T4): Tuple4[T1, T2, T3, T4] =
    Array(_1, _2, _3, _4).asInstanceOf[Tuple4[T1, T2, T3, T4]]

  @inline def unapply[T1, T2, T3, T4](t: Tuple4[T1, T2, T3, T4]): Option[(T1, T2, T3, T4)] =
    Some(t)

  @inline implicit def fromScalaTuple4[T1, T2, T3, T4](t: (T1, T2, T3, T4)): Tuple4[T1, T2, T3, T4] =
    apply(t._1, t._2, t._3, t._4)

  @inline implicit def toScalaTuple4[T1, T2, T3, T4](t: Tuple4[T1, T2, T3, T4]): (T1, T2, T3, T4) =
    (t._1, t._2, t._3, t._4)
}


/**
 *  A tuple "view" of 5 elements of a JavaScript [[Array]].
 *
 *  @see [[Tuple2]]
 */
@native
sealed trait Tuple5[+T1, +T2, +T3, +T4, +T5] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
  @JSName("2") val _3: T3 = native
  @JSName("3") val _4: T4 = native
  @JSName("4") val _5: T5 = native
}

object Tuple5 {
  @inline def apply[T1, T2, T3, T4, T5](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5): Tuple5[T1, T2, T3, T4, T5] =
    Array(_1, _2, _3, _4, _5).asInstanceOf[Tuple5[T1, T2, T3, T4, T5]]

  @inline def unapply[T1, T2, T3, T4, T5](t: Tuple5[T1, T2, T3, T4, T5]): Option[(T1, T2, T3, T4, T5)] =
    Some(t)

  @inline implicit def fromScalaTuple5[T1, T2, T3, T4, T5](t: (T1, T2, T3, T4, T5)): Tuple5[T1, T2, T3, T4, T5] =
    apply(t._1, t._2, t._3, t._4, t._5)

  @inline implicit def toScalaTuple5[T1, T2, T3, T4, T5](t: Tuple5[T1, T2, T3, T4, T5]): (T1, T2, T3, T4, T5) =
    (t._1, t._2, t._3, t._4, t._5)
}

/**
 *  A tuple "view" of 6 elements of a JavaScript [[Array]].
 *
 *  @see [[Tuple2]]
 */
@native
sealed trait Tuple6[+T1, +T2, +T3, +T4, +T5, +T6] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
  @JSName("2") val _3: T3 = native
  @JSName("3") val _4: T4 = native
  @JSName("4") val _5: T5 = native
  @JSName("5") val _6: T6 = native
}

object Tuple6 {
  @inline def apply[T1, T2, T3, T4, T5, T6](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6): Tuple6[T1, T2, T3, T4, T5, T6] =
    Array(_1, _2, _3, _4, _5, _6).asInstanceOf[Tuple6[T1, T2, T3, T4, T5, T6]]

  @inline def unapply[T1, T2, T3, T4, T5, T6](t: Tuple6[T1, T2, T3, T4, T5, T6]): Option[(T1, T2, T3, T4, T5, T6)] =
    Some(t)

  @inline implicit def fromScalaTuple6[T1, T2, T3, T4, T5, T6](t: (T1, T2, T3, T4, T5, T6)): Tuple6[T1, T2, T3, T4, T5, T6] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6)

  @inline implicit def toScalaTuple6[T1, T2, T3, T4, T5, T6](t: Tuple6[T1, T2, T3, T4, T5, T6]): (T1, T2, T3, T4, T5, T6) =
    (t._1, t._2, t._3, t._4, t._5, t._6)
}

/**
 *  A tuple "view" of 7 elements of a JavaScript [[Array]].
 *
 *  @see [[Tuple2]]
 */
@native
sealed trait Tuple7[+T1, +T2, +T3, +T4, +T5, +T6, +T7] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
  @JSName("2") val _3: T3 = native
  @JSName("3") val _4: T4 = native
  @JSName("4") val _5: T5 = native
  @JSName("5") val _6: T6 = native
  @JSName("6") val _7: T7 = native
}

object Tuple7 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7): Tuple7[T1, T2, T3, T4, T5, T6, T7] =
    Array(_1, _2, _3, _4, _5, _6, _7).asInstanceOf[Tuple7[T1, T2, T3, T4, T5, T6, T7]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7](t: Tuple7[T1, T2, T3, T4, T5, T6, T7]): Option[(T1, T2, T3, T4, T5, T6, T7)] =
    Some(t)

  @inline implicit def fromScalaTuple7[T1, T2, T3, T4, T5, T6, T7](t: (T1, T2, T3, T4, T5, T6, T7)): Tuple7[T1, T2, T3, T4, T5, T6, T7] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7)

  @inline implicit def toScalaTuple7[T1, T2, T3, T4, T5, T6, T7](t: Tuple7[T1, T2, T3, T4, T5, T6, T7]): (T1, T2, T3, T4, T5, T6, T7) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7)
}

/**
 *  A tuple "view" of 8 elements of a JavaScript [[Array]].
 *
 *  @see [[Tuple2]]
 */
@native
sealed trait Tuple8[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
  @JSName("2") val _3: T3 = native
  @JSName("3") val _4: T4 = native
  @JSName("4") val _5: T5 = native
  @JSName("5") val _6: T6 = native
  @JSName("6") val _7: T7 = native
  @JSName("7") val _8: T8 = native
}

object Tuple8 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8): Tuple8[T1, T2, T3, T4, T5, T6, T7, T8] =
    Array(_1, _2, _3, _4, _5, _6, _7, _8).asInstanceOf[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8](t: Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]): Option[(T1, T2, T3, T4, T5, T6, T7, T8)] =
    Some(t)

  @inline implicit def fromScalaTuple8[T1, T2, T3, T4, T5, T6, T7, T8](t: (T1, T2, T3, T4, T5, T6, T7, T8)): Tuple8[T1, T2, T3, T4, T5, T6, T7, T8] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8)

  @inline implicit def toScalaTuple8[T1, T2, T3, T4, T5, T6, T7, T8](t: Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]): (T1, T2, T3, T4, T5, T6, T7, T8) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8)
}

/**
 *  A tuple "view" of 9 elements of a JavaScript [[Array]].
 *
 *  @see [[Tuple2]]
 */
@native
sealed trait Tuple9[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
  @JSName("2") val _3: T3 = native
  @JSName("3") val _4: T4 = native
  @JSName("4") val _5: T5 = native
  @JSName("5") val _6: T6 = native
  @JSName("6") val _7: T7 = native
  @JSName("7") val _8: T8 = native
  @JSName("8") val _9: T9 = native
}

object Tuple9 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9): Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9] =
    Array(_1, _2, _3, _4, _5, _6, _7, _8, _9).asInstanceOf[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9](t: Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
    Some(t)

  @inline implicit def fromScalaTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9)): Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)

  @inline implicit def toScalaTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](t: Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]): (T1, T2, T3, T4, T5, T6, T7, T8, T9) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)
}

/**
 *  A tuple "view" of 10 elements of a JavaScript [[Array]].
 *
 *  @see [[Tuple2]]
 */
@native
sealed trait Tuple10[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
  @JSName("2") val _3: T3 = native
  @JSName("3") val _4: T4 = native
  @JSName("4") val _5: T5 = native
  @JSName("5") val _6: T6 = native
  @JSName("6") val _7: T7 = native
  @JSName("7") val _8: T8 = native
  @JSName("8") val _9: T9 = native
  @JSName("9") val _10: T10 = native
}

object Tuple10 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10): Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] =
    Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10).asInstanceOf[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](t: Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] =
    Some(t)

  @inline implicit def fromScalaTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)): Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)

  @inline implicit def toScalaTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](t: Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)
}

/**
 *  A tuple "view" of 11 elements of a JavaScript [[Array]].
 *
 *  @see [[Tuple2]]
 */
@native
sealed trait Tuple11[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
  @JSName("2") val _3: T3 = native
  @JSName("3") val _4: T4 = native
  @JSName("4") val _5: T5 = native
  @JSName("5") val _6: T6 = native
  @JSName("6") val _7: T7 = native
  @JSName("7") val _8: T8 = native
  @JSName("8") val _9: T9 = native
  @JSName("9") val _10: T10 = native
  @JSName("10") val _11: T11 = native
}

object Tuple11 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11): Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] =
    Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11).asInstanceOf[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](t: Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] =
    Some(t)

  @inline implicit def fromScalaTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)): Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)

  @inline implicit def toScalaTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](t: Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)
}

/**
 *  A tuple "view" of 12 elements of a JavaScript [[Array]].
 *
 *  @see [[Tuple2]]
 */
@native
sealed trait Tuple12[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
  @JSName("2") val _3: T3 = native
  @JSName("3") val _4: T4 = native
  @JSName("4") val _5: T5 = native
  @JSName("5") val _6: T6 = native
  @JSName("6") val _7: T7 = native
  @JSName("7") val _8: T8 = native
  @JSName("8") val _9: T9 = native
  @JSName("9") val _10: T10 = native
  @JSName("10") val _11: T11 = native
  @JSName("11") val _12: T12 = native
}

object Tuple12 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12): Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] =
    Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12).asInstanceOf[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](t: Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] =
    Some(t)

  @inline implicit def fromScalaTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)): Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)

  @inline implicit def toScalaTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](t: Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)
}

/**
 *  A tuple "view" of 13 elements of a JavaScript [[Array]].
 *
 *  @see [[Tuple2]]
 */
@native
sealed trait Tuple13[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
  @JSName("2") val _3: T3 = native
  @JSName("3") val _4: T4 = native
  @JSName("4") val _5: T5 = native
  @JSName("5") val _6: T6 = native
  @JSName("6") val _7: T7 = native
  @JSName("7") val _8: T8 = native
  @JSName("8") val _9: T9 = native
  @JSName("9") val _10: T10 = native
  @JSName("10") val _11: T11 = native
  @JSName("11") val _12: T12 = native
  @JSName("12") val _13: T13 = native
}

object Tuple13 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13): Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] =
    Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13).asInstanceOf[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](t: Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] =
    Some(t)

  @inline implicit def fromScalaTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)): Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)

  @inline implicit def toScalaTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](t: Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)
}

/**
 *  A tuple "view" of 14 elements of a JavaScript [[Array]].
 *
 *  @see [[Tuple2]]
 */
@native
sealed trait Tuple14[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
  @JSName("2") val _3: T3 = native
  @JSName("3") val _4: T4 = native
  @JSName("4") val _5: T5 = native
  @JSName("5") val _6: T6 = native
  @JSName("6") val _7: T7 = native
  @JSName("7") val _8: T8 = native
  @JSName("8") val _9: T9 = native
  @JSName("9") val _10: T10 = native
  @JSName("10") val _11: T11 = native
  @JSName("11") val _12: T12 = native
  @JSName("12") val _13: T13 = native
  @JSName("13") val _14: T14 = native
}

object Tuple14 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14): Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] =
    Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14).asInstanceOf[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](t: Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] =
    Some(t)

  @inline implicit def fromScalaTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)): Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)

  @inline implicit def toScalaTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](t: Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)
}

/**
 *  A tuple "view" of 15 elements of a JavaScript [[Array]].
 *
 *  @see [[Tuple2]]
 */
@native
sealed trait Tuple15[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
  @JSName("2") val _3: T3 = native
  @JSName("3") val _4: T4 = native
  @JSName("4") val _5: T5 = native
  @JSName("5") val _6: T6 = native
  @JSName("6") val _7: T7 = native
  @JSName("7") val _8: T8 = native
  @JSName("8") val _9: T9 = native
  @JSName("9") val _10: T10 = native
  @JSName("10") val _11: T11 = native
  @JSName("11") val _12: T12 = native
  @JSName("12") val _13: T13 = native
  @JSName("13") val _14: T14 = native
  @JSName("14") val _15: T15 = native
}

object Tuple15 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15): Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] =
    Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15).asInstanceOf[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](t: Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] =
    Some(t)

  @inline implicit def fromScalaTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)): Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)

  @inline implicit def toScalaTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](t: Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)
}

/**
 *  A tuple "view" of 16 elements of a JavaScript [[Array]].
 *
 *  @see [[Tuple16]]
 */
@native
sealed trait Tuple16[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
  @JSName("2") val _3: T3 = native
  @JSName("3") val _4: T4 = native
  @JSName("4") val _5: T5 = native
  @JSName("5") val _6: T6 = native
  @JSName("6") val _7: T7 = native
  @JSName("7") val _8: T8 = native
  @JSName("8") val _9: T9 = native
  @JSName("9") val _10: T10 = native
  @JSName("10") val _11: T11 = native
  @JSName("11") val _12: T12 = native
  @JSName("12") val _13: T13 = native
  @JSName("13") val _14: T14 = native
  @JSName("14") val _15: T15 = native
  @JSName("15") val _16: T16 = native
}

object Tuple16 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16): Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] =
    Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16).asInstanceOf[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](t: Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] =
    Some(t)

  @inline implicit def fromScalaTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)): Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)

  @inline implicit def toScalaTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](t: Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)
}

/**
 *  A tuple "view" of 17 elements of a JavaScript [[Array]].
 *
 *  @see [[Tuple2]]
 */
@native
sealed trait Tuple17[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
  @JSName("2") val _3: T3 = native
  @JSName("3") val _4: T4 = native
  @JSName("4") val _5: T5 = native
  @JSName("5") val _6: T6 = native
  @JSName("6") val _7: T7 = native
  @JSName("7") val _8: T8 = native
  @JSName("8") val _9: T9 = native
  @JSName("9") val _10: T10 = native
  @JSName("10") val _11: T11 = native
  @JSName("11") val _12: T12 = native
  @JSName("12") val _13: T13 = native
  @JSName("13") val _14: T14 = native
  @JSName("14") val _15: T15 = native
  @JSName("15") val _16: T16 = native
  @JSName("16") val _17: T17 = native
}

object Tuple17 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17): Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] =
    Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17).asInstanceOf[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](t: Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] =
    Some(t)

  @inline implicit def fromScalaTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)): Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)

  @inline implicit def toScalaTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](t: Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)
}

/**
 *  A tuple "view" of 18 elements of a JavaScript [[Array]].
 *
 *  @see [[Tuple2]]
 */
@native
sealed trait Tuple18[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
  @JSName("2") val _3: T3 = native
  @JSName("3") val _4: T4 = native
  @JSName("4") val _5: T5 = native
  @JSName("5") val _6: T6 = native
  @JSName("6") val _7: T7 = native
  @JSName("7") val _8: T8 = native
  @JSName("8") val _9: T9 = native
  @JSName("9") val _10: T10 = native
  @JSName("10") val _11: T11 = native
  @JSName("11") val _12: T12 = native
  @JSName("12") val _13: T13 = native
  @JSName("13") val _14: T14 = native
  @JSName("14") val _15: T15 = native
  @JSName("15") val _16: T16 = native
  @JSName("16") val _17: T17 = native
  @JSName("17") val _18: T18 = native
}

object Tuple18 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18): Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] =
    Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18).asInstanceOf[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](t: Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] =
    Some(t)

  @inline implicit def fromScalaTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)): Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)

  @inline implicit def toScalaTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](t: Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)
}

/**
 *  A tuple "view" of 19 elements of a JavaScript [[Array]].
 *
 *  @see [[Tuple2]]
 */
@native
sealed trait Tuple19[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
  @JSName("2") val _3: T3 = native
  @JSName("3") val _4: T4 = native
  @JSName("4") val _5: T5 = native
  @JSName("5") val _6: T6 = native
  @JSName("6") val _7: T7 = native
  @JSName("7") val _8: T8 = native
  @JSName("8") val _9: T9 = native
  @JSName("9") val _10: T10 = native
  @JSName("10") val _11: T11 = native
  @JSName("11") val _12: T12 = native
  @JSName("12") val _13: T13 = native
  @JSName("13") val _14: T14 = native
  @JSName("14") val _15: T15 = native
  @JSName("15") val _16: T16 = native
  @JSName("16") val _17: T17 = native
  @JSName("17") val _18: T18 = native
  @JSName("18") val _19: T19 = native
}

object Tuple19 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18, _19: T19): Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] =
    Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19).asInstanceOf[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](t: Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] =
    Some(t)

  @inline implicit def fromScalaTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)): Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)

  @inline implicit def toScalaTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](t: Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)
}

/**
 *  A tuple "view" of 20 elements of a JavaScript [[Array]].
 *
 *  @see [[Tuple2]]
 */
@native
sealed trait Tuple20[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
  @JSName("2") val _3: T3 = native
  @JSName("3") val _4: T4 = native
  @JSName("4") val _5: T5 = native
  @JSName("5") val _6: T6 = native
  @JSName("6") val _7: T7 = native
  @JSName("7") val _8: T8 = native
  @JSName("8") val _9: T9 = native
  @JSName("9") val _10: T10 = native
  @JSName("10") val _11: T11 = native
  @JSName("11") val _12: T12 = native
  @JSName("12") val _13: T13 = native
  @JSName("13") val _14: T14 = native
  @JSName("14") val _15: T15 = native
  @JSName("15") val _16: T16 = native
  @JSName("16") val _17: T17 = native
  @JSName("17") val _18: T18 = native
  @JSName("18") val _19: T19 = native
  @JSName("19") val _20: T20 = native
}

object Tuple20 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18, _19: T19, _20: T20): Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] =
    Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20).asInstanceOf[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](t: Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)] =
    Some(t)

  @inline implicit def fromScalaTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)): Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)

  @inline implicit def toScalaTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](t: Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)
}

/**
 *  A tuple "view" of 21 elements of a JavaScript [[Array]].
 *
 *  @see [[Tuple2]]
 */
@native
sealed trait Tuple21[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20, +T21] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
  @JSName("2") val _3: T3 = native
  @JSName("3") val _4: T4 = native
  @JSName("4") val _5: T5 = native
  @JSName("5") val _6: T6 = native
  @JSName("6") val _7: T7 = native
  @JSName("7") val _8: T8 = native
  @JSName("8") val _9: T9 = native
  @JSName("9") val _10: T10 = native
  @JSName("10") val _11: T11 = native
  @JSName("11") val _12: T12 = native
  @JSName("12") val _13: T13 = native
  @JSName("13") val _14: T14 = native
  @JSName("14") val _15: T15 = native
  @JSName("15") val _16: T16 = native
  @JSName("16") val _17: T17 = native
  @JSName("17") val _18: T18 = native
  @JSName("18") val _19: T19 = native
  @JSName("19") val _20: T20 = native
  @JSName("20") val _21: T21 = native
}

object Tuple21 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18, _19: T19, _20: T20, _21: T21): Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] =
    Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21).asInstanceOf[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](t: Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)] =
    Some(t)

  @inline implicit def fromScalaTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)): Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)

  @inline implicit def toScalaTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](t: Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)
}

/**
 *  A tuple "view" of 22 elements of a JavaScript [[Array]].
 *
 *  @see [[Tuple2]]
 */
@native
sealed trait Tuple22[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20, +T21, +T22] extends Object {
  @JSName("0") val _1: T1 = native
  @JSName("1") val _2: T2 = native
  @JSName("2") val _3: T3 = native
  @JSName("3") val _4: T4 = native
  @JSName("4") val _5: T5 = native
  @JSName("5") val _6: T6 = native
  @JSName("6") val _7: T7 = native
  @JSName("7") val _8: T8 = native
  @JSName("8") val _9: T9 = native
  @JSName("9") val _10: T10 = native
  @JSName("10") val _11: T11 = native
  @JSName("11") val _12: T12 = native
  @JSName("12") val _13: T13 = native
  @JSName("13") val _14: T14 = native
  @JSName("14") val _15: T15 = native
  @JSName("15") val _16: T16 = native
  @JSName("16") val _17: T17 = native
  @JSName("17") val _18: T18 = native
  @JSName("18") val _19: T19 = native
  @JSName("19") val _20: T20 = native
  @JSName("20") val _21: T21 = native
  @JSName("21") val _22: T22 = native
}

object Tuple22 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18, _19: T19, _20: T20, _21: T21, _22: T22): Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] =
    Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22).asInstanceOf[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](t: Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)] =
    Some(t)

  @inline implicit def fromScalaTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)): Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)

  @inline implicit def toScalaTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](t: Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)
}

// scalastyle:on line.size.limit
