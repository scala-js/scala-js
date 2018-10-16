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

/* Definitions for js.Tuple4 to js.Tuple22 that do not show in doc */
package scala.scalajs.js

import scala.language.implicitConversions

import scala.scalajs.js
import scala.scalajs.js.annotation._

// scalastyle:off line.size.limit

/**
 *  A tuple "view" of 4 elements of a JavaScript [[js.Array]].
 *
 *  @see [[js.Tuple2]]
 */
sealed trait Tuple4[+T1, +T2, +T3, +T4] extends js.Object {
  @JSName("0") val _1: T1
  @JSName("1") val _2: T2
  @JSName("2") val _3: T3
  @JSName("3") val _4: T4
}

object Tuple4 {
  @inline def apply[T1, T2, T3, T4](_1: T1, _2: T2, _3: T3, _4: T4): js.Tuple4[T1, T2, T3, T4] =
    js.Array(_1, _2, _3, _4).asInstanceOf[js.Tuple4[T1, T2, T3, T4]]

  @inline def unapply[T1, T2, T3, T4](t: js.Tuple4[T1, T2, T3, T4]): Option[(T1, T2, T3, T4)] =
    Some(t)

  @inline implicit def fromScalaTuple4[T1, T2, T3, T4](t: (T1, T2, T3, T4)): js.Tuple4[T1, T2, T3, T4] =
    apply(t._1, t._2, t._3, t._4)

  @inline implicit def toScalaTuple4[T1, T2, T3, T4](t: js.Tuple4[T1, T2, T3, T4]): (T1, T2, T3, T4) =
    (t._1, t._2, t._3, t._4)
}

/**
 *  A tuple "view" of 5 elements of a JavaScript [[js.Array]].
 *
 *  @see [[js.Tuple2]]
 */
sealed trait Tuple5[+T1, +T2, +T3, +T4, +T5] extends js.Object {
  @JSName("0") val _1: T1
  @JSName("1") val _2: T2
  @JSName("2") val _3: T3
  @JSName("3") val _4: T4
  @JSName("4") val _5: T5
}

object Tuple5 {
  @inline def apply[T1, T2, T3, T4, T5](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5): js.Tuple5[T1, T2, T3, T4, T5] =
    js.Array(_1, _2, _3, _4, _5).asInstanceOf[js.Tuple5[T1, T2, T3, T4, T5]]

  @inline def unapply[T1, T2, T3, T4, T5](t: js.Tuple5[T1, T2, T3, T4, T5]): Option[(T1, T2, T3, T4, T5)] =
    Some(t)

  @inline implicit def fromScalaTuple5[T1, T2, T3, T4, T5](t: (T1, T2, T3, T4, T5)): js.Tuple5[T1, T2, T3, T4, T5] =
    apply(t._1, t._2, t._3, t._4, t._5)

  @inline implicit def toScalaTuple5[T1, T2, T3, T4, T5](t: js.Tuple5[T1, T2, T3, T4, T5]): (T1, T2, T3, T4, T5) =
    (t._1, t._2, t._3, t._4, t._5)
}

/**
 *  A tuple "view" of 6 elements of a JavaScript [[js.Array]].
 *
 *  @see [[js.Tuple2]]
 */
sealed trait Tuple6[+T1, +T2, +T3, +T4, +T5, +T6] extends js.Object {
  @JSName("0") val _1: T1
  @JSName("1") val _2: T2
  @JSName("2") val _3: T3
  @JSName("3") val _4: T4
  @JSName("4") val _5: T5
  @JSName("5") val _6: T6
}

object Tuple6 {
  @inline def apply[T1, T2, T3, T4, T5, T6](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6): js.Tuple6[T1, T2, T3, T4, T5, T6] =
    js.Array(_1, _2, _3, _4, _5, _6).asInstanceOf[js.Tuple6[T1, T2, T3, T4, T5, T6]]

  @inline def unapply[T1, T2, T3, T4, T5, T6](t: js.Tuple6[T1, T2, T3, T4, T5, T6]): Option[(T1, T2, T3, T4, T5, T6)] =
    Some(t)

  @inline implicit def fromScalaTuple6[T1, T2, T3, T4, T5, T6](t: (T1, T2, T3, T4, T5, T6)): js.Tuple6[T1, T2, T3, T4, T5, T6] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6)

  @inline implicit def toScalaTuple6[T1, T2, T3, T4, T5, T6](t: js.Tuple6[T1, T2, T3, T4, T5, T6]): (T1, T2, T3, T4, T5, T6) =
    (t._1, t._2, t._3, t._4, t._5, t._6)
}

/**
 *  A tuple "view" of 7 elements of a JavaScript [[js.Array]].
 *
 *  @see [[js.Tuple2]]
 */
sealed trait Tuple7[+T1, +T2, +T3, +T4, +T5, +T6, +T7] extends js.Object {
  @JSName("0") val _1: T1
  @JSName("1") val _2: T2
  @JSName("2") val _3: T3
  @JSName("3") val _4: T4
  @JSName("4") val _5: T5
  @JSName("5") val _6: T6
  @JSName("6") val _7: T7
}

object Tuple7 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7): js.Tuple7[T1, T2, T3, T4, T5, T6, T7] =
    js.Array(_1, _2, _3, _4, _5, _6, _7).asInstanceOf[js.Tuple7[T1, T2, T3, T4, T5, T6, T7]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7](t: js.Tuple7[T1, T2, T3, T4, T5, T6, T7]): Option[(T1, T2, T3, T4, T5, T6, T7)] =
    Some(t)

  @inline implicit def fromScalaTuple7[T1, T2, T3, T4, T5, T6, T7](t: (T1, T2, T3, T4, T5, T6, T7)): js.Tuple7[T1, T2, T3, T4, T5, T6, T7] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7)

  @inline implicit def toScalaTuple7[T1, T2, T3, T4, T5, T6, T7](t: js.Tuple7[T1, T2, T3, T4, T5, T6, T7]): (T1, T2, T3, T4, T5, T6, T7) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7)
}

/**
 *  A tuple "view" of 8 elements of a JavaScript [[js.Array]].
 *
 *  @see [[js.Tuple2]]
 */
sealed trait Tuple8[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8] extends js.Object {
  @JSName("0") val _1: T1
  @JSName("1") val _2: T2
  @JSName("2") val _3: T3
  @JSName("3") val _4: T4
  @JSName("4") val _5: T5
  @JSName("5") val _6: T6
  @JSName("6") val _7: T7
  @JSName("7") val _8: T8
}

object Tuple8 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8): js.Tuple8[T1, T2, T3, T4, T5, T6, T7, T8] =
    js.Array(_1, _2, _3, _4, _5, _6, _7, _8).asInstanceOf[js.Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8](t: js.Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]): Option[(T1, T2, T3, T4, T5, T6, T7, T8)] =
    Some(t)

  @inline implicit def fromScalaTuple8[T1, T2, T3, T4, T5, T6, T7, T8](t: (T1, T2, T3, T4, T5, T6, T7, T8)): js.Tuple8[T1, T2, T3, T4, T5, T6, T7, T8] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8)

  @inline implicit def toScalaTuple8[T1, T2, T3, T4, T5, T6, T7, T8](t: js.Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]): (T1, T2, T3, T4, T5, T6, T7, T8) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8)
}

/**
 *  A tuple "view" of 9 elements of a JavaScript [[js.Array]].
 *
 *  @see [[js.Tuple2]]
 */
sealed trait Tuple9[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9] extends js.Object {
  @JSName("0") val _1: T1
  @JSName("1") val _2: T2
  @JSName("2") val _3: T3
  @JSName("3") val _4: T4
  @JSName("4") val _5: T5
  @JSName("5") val _6: T6
  @JSName("6") val _7: T7
  @JSName("7") val _8: T8
  @JSName("8") val _9: T9
}

object Tuple9 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9): js.Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9] =
    js.Array(_1, _2, _3, _4, _5, _6, _7, _8, _9).asInstanceOf[js.Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9](t: js.Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
    Some(t)

  @inline implicit def fromScalaTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9)): js.Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)

  @inline implicit def toScalaTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](t: js.Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]): (T1, T2, T3, T4, T5, T6, T7, T8, T9) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)
}

/**
 *  A tuple "view" of 10 elements of a JavaScript [[js.Array]].
 *
 *  @see [[js.Tuple2]]
 */
sealed trait Tuple10[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10] extends js.Object {
  @JSName("0") val _1: T1
  @JSName("1") val _2: T2
  @JSName("2") val _3: T3
  @JSName("3") val _4: T4
  @JSName("4") val _5: T5
  @JSName("5") val _6: T6
  @JSName("6") val _7: T7
  @JSName("7") val _8: T8
  @JSName("8") val _9: T9
  @JSName("9") val _10: T10
}

object Tuple10 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10): js.Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] =
    js.Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10).asInstanceOf[js.Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](t: js.Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] =
    Some(t)

  @inline implicit def fromScalaTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)): js.Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)

  @inline implicit def toScalaTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](t: js.Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)
}

/**
 *  A tuple "view" of 11 elements of a JavaScript [[js.Array]].
 *
 *  @see [[js.Tuple2]]
 */
sealed trait Tuple11[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11] extends js.Object {
  @JSName("0") val _1: T1
  @JSName("1") val _2: T2
  @JSName("2") val _3: T3
  @JSName("3") val _4: T4
  @JSName("4") val _5: T5
  @JSName("5") val _6: T6
  @JSName("6") val _7: T7
  @JSName("7") val _8: T8
  @JSName("8") val _9: T9
  @JSName("9") val _10: T10
  @JSName("10") val _11: T11
}

object Tuple11 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11): js.Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] =
    js.Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11).asInstanceOf[js.Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](t: js.Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] =
    Some(t)

  @inline implicit def fromScalaTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)): js.Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)

  @inline implicit def toScalaTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](t: js.Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)
}

/**
 *  A tuple "view" of 12 elements of a JavaScript [[js.Array]].
 *
 *  @see [[js.Tuple2]]
 */
sealed trait Tuple12[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12] extends js.Object {
  @JSName("0") val _1: T1
  @JSName("1") val _2: T2
  @JSName("2") val _3: T3
  @JSName("3") val _4: T4
  @JSName("4") val _5: T5
  @JSName("5") val _6: T6
  @JSName("6") val _7: T7
  @JSName("7") val _8: T8
  @JSName("8") val _9: T9
  @JSName("9") val _10: T10
  @JSName("10") val _11: T11
  @JSName("11") val _12: T12
}

object Tuple12 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12): js.Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] =
    js.Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12).asInstanceOf[js.Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](t: js.Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] =
    Some(t)

  @inline implicit def fromScalaTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)): js.Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)

  @inline implicit def toScalaTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](t: js.Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)
}

/**
 *  A tuple "view" of 13 elements of a JavaScript [[js.Array]].
 *
 *  @see [[js.Tuple2]]
 */
sealed trait Tuple13[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13] extends js.Object {
  @JSName("0") val _1: T1
  @JSName("1") val _2: T2
  @JSName("2") val _3: T3
  @JSName("3") val _4: T4
  @JSName("4") val _5: T5
  @JSName("5") val _6: T6
  @JSName("6") val _7: T7
  @JSName("7") val _8: T8
  @JSName("8") val _9: T9
  @JSName("9") val _10: T10
  @JSName("10") val _11: T11
  @JSName("11") val _12: T12
  @JSName("12") val _13: T13
}

object Tuple13 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13): js.Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] =
    js.Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13).asInstanceOf[js.Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](t: js.Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] =
    Some(t)

  @inline implicit def fromScalaTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)): js.Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)

  @inline implicit def toScalaTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](t: js.Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)
}

/**
 *  A tuple "view" of 14 elements of a JavaScript [[js.Array]].
 *
 *  @see [[js.Tuple2]]
 */
sealed trait Tuple14[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14] extends js.Object {
  @JSName("0") val _1: T1
  @JSName("1") val _2: T2
  @JSName("2") val _3: T3
  @JSName("3") val _4: T4
  @JSName("4") val _5: T5
  @JSName("5") val _6: T6
  @JSName("6") val _7: T7
  @JSName("7") val _8: T8
  @JSName("8") val _9: T9
  @JSName("9") val _10: T10
  @JSName("10") val _11: T11
  @JSName("11") val _12: T12
  @JSName("12") val _13: T13
  @JSName("13") val _14: T14
}

object Tuple14 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14): js.Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] =
    js.Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14).asInstanceOf[js.Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](t: js.Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] =
    Some(t)

  @inline implicit def fromScalaTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)): js.Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)

  @inline implicit def toScalaTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](t: js.Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)
}

/**
 *  A tuple "view" of 15 elements of a JavaScript [[js.Array]].
 *
 *  @see [[js.Tuple2]]
 */
sealed trait Tuple15[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15] extends js.Object {
  @JSName("0") val _1: T1
  @JSName("1") val _2: T2
  @JSName("2") val _3: T3
  @JSName("3") val _4: T4
  @JSName("4") val _5: T5
  @JSName("5") val _6: T6
  @JSName("6") val _7: T7
  @JSName("7") val _8: T8
  @JSName("8") val _9: T9
  @JSName("9") val _10: T10
  @JSName("10") val _11: T11
  @JSName("11") val _12: T12
  @JSName("12") val _13: T13
  @JSName("13") val _14: T14
  @JSName("14") val _15: T15
}

object Tuple15 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15): js.Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] =
    js.Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15).asInstanceOf[js.Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](t: js.Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] =
    Some(t)

  @inline implicit def fromScalaTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)): js.Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)

  @inline implicit def toScalaTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](t: js.Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)
}

/**
 *  A tuple "view" of 16 elements of a JavaScript [[js.Array]].
 *
 *  @see [[js.Tuple16]]
 */
sealed trait Tuple16[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16] extends js.Object {
  @JSName("0") val _1: T1
  @JSName("1") val _2: T2
  @JSName("2") val _3: T3
  @JSName("3") val _4: T4
  @JSName("4") val _5: T5
  @JSName("5") val _6: T6
  @JSName("6") val _7: T7
  @JSName("7") val _8: T8
  @JSName("8") val _9: T9
  @JSName("9") val _10: T10
  @JSName("10") val _11: T11
  @JSName("11") val _12: T12
  @JSName("12") val _13: T13
  @JSName("13") val _14: T14
  @JSName("14") val _15: T15
  @JSName("15") val _16: T16
}

object Tuple16 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16): js.Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] =
    js.Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16).asInstanceOf[js.Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](t: js.Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] =
    Some(t)

  @inline implicit def fromScalaTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)): js.Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)

  @inline implicit def toScalaTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](t: js.Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)
}

/**
 *  A tuple "view" of 17 elements of a JavaScript [[js.Array]].
 *
 *  @see [[js.Tuple2]]
 */
sealed trait Tuple17[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17] extends js.Object {
  @JSName("0") val _1: T1
  @JSName("1") val _2: T2
  @JSName("2") val _3: T3
  @JSName("3") val _4: T4
  @JSName("4") val _5: T5
  @JSName("5") val _6: T6
  @JSName("6") val _7: T7
  @JSName("7") val _8: T8
  @JSName("8") val _9: T9
  @JSName("9") val _10: T10
  @JSName("10") val _11: T11
  @JSName("11") val _12: T12
  @JSName("12") val _13: T13
  @JSName("13") val _14: T14
  @JSName("14") val _15: T15
  @JSName("15") val _16: T16
  @JSName("16") val _17: T17
}

object Tuple17 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17): js.Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] =
    js.Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17).asInstanceOf[js.Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](t: js.Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] =
    Some(t)

  @inline implicit def fromScalaTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)): js.Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)

  @inline implicit def toScalaTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](t: js.Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)
}

/**
 *  A tuple "view" of 18 elements of a JavaScript [[js.Array]].
 *
 *  @see [[js.Tuple2]]
 */
sealed trait Tuple18[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18] extends js.Object {
  @JSName("0") val _1: T1
  @JSName("1") val _2: T2
  @JSName("2") val _3: T3
  @JSName("3") val _4: T4
  @JSName("4") val _5: T5
  @JSName("5") val _6: T6
  @JSName("6") val _7: T7
  @JSName("7") val _8: T8
  @JSName("8") val _9: T9
  @JSName("9") val _10: T10
  @JSName("10") val _11: T11
  @JSName("11") val _12: T12
  @JSName("12") val _13: T13
  @JSName("13") val _14: T14
  @JSName("14") val _15: T15
  @JSName("15") val _16: T16
  @JSName("16") val _17: T17
  @JSName("17") val _18: T18
}

object Tuple18 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18): js.Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] =
    js.Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18).asInstanceOf[js.Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](t: js.Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] =
    Some(t)

  @inline implicit def fromScalaTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)): js.Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)

  @inline implicit def toScalaTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](t: js.Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)
}

/**
 *  A tuple "view" of 19 elements of a JavaScript [[js.Array]].
 *
 *  @see [[js.Tuple2]]
 */
sealed trait Tuple19[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19] extends js.Object {
  @JSName("0") val _1: T1
  @JSName("1") val _2: T2
  @JSName("2") val _3: T3
  @JSName("3") val _4: T4
  @JSName("4") val _5: T5
  @JSName("5") val _6: T6
  @JSName("6") val _7: T7
  @JSName("7") val _8: T8
  @JSName("8") val _9: T9
  @JSName("9") val _10: T10
  @JSName("10") val _11: T11
  @JSName("11") val _12: T12
  @JSName("12") val _13: T13
  @JSName("13") val _14: T14
  @JSName("14") val _15: T15
  @JSName("15") val _16: T16
  @JSName("16") val _17: T17
  @JSName("17") val _18: T18
  @JSName("18") val _19: T19
}

object Tuple19 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18, _19: T19): js.Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] =
    js.Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19).asInstanceOf[js.Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](t: js.Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] =
    Some(t)

  @inline implicit def fromScalaTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)): js.Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)

  @inline implicit def toScalaTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](t: js.Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)
}

/**
 *  A tuple "view" of 20 elements of a JavaScript [[js.Array]].
 *
 *  @see [[js.Tuple2]]
 */
sealed trait Tuple20[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20] extends js.Object {
  @JSName("0") val _1: T1
  @JSName("1") val _2: T2
  @JSName("2") val _3: T3
  @JSName("3") val _4: T4
  @JSName("4") val _5: T5
  @JSName("5") val _6: T6
  @JSName("6") val _7: T7
  @JSName("7") val _8: T8
  @JSName("8") val _9: T9
  @JSName("9") val _10: T10
  @JSName("10") val _11: T11
  @JSName("11") val _12: T12
  @JSName("12") val _13: T13
  @JSName("13") val _14: T14
  @JSName("14") val _15: T15
  @JSName("15") val _16: T16
  @JSName("16") val _17: T17
  @JSName("17") val _18: T18
  @JSName("18") val _19: T19
  @JSName("19") val _20: T20
}

object Tuple20 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18, _19: T19, _20: T20): js.Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] =
    js.Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20).asInstanceOf[js.Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](t: js.Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)] =
    Some(t)

  @inline implicit def fromScalaTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)): js.Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)

  @inline implicit def toScalaTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](t: js.Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)
}

/**
 *  A tuple "view" of 21 elements of a JavaScript [[js.Array]].
 *
 *  @see [[js.Tuple2]]
 */
sealed trait Tuple21[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20, +T21] extends js.Object {
  @JSName("0") val _1: T1
  @JSName("1") val _2: T2
  @JSName("2") val _3: T3
  @JSName("3") val _4: T4
  @JSName("4") val _5: T5
  @JSName("5") val _6: T6
  @JSName("6") val _7: T7
  @JSName("7") val _8: T8
  @JSName("8") val _9: T9
  @JSName("9") val _10: T10
  @JSName("10") val _11: T11
  @JSName("11") val _12: T12
  @JSName("12") val _13: T13
  @JSName("13") val _14: T14
  @JSName("14") val _15: T15
  @JSName("15") val _16: T16
  @JSName("16") val _17: T17
  @JSName("17") val _18: T18
  @JSName("18") val _19: T19
  @JSName("19") val _20: T20
  @JSName("20") val _21: T21
}

object Tuple21 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18, _19: T19, _20: T20, _21: T21): js.Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] =
    js.Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21).asInstanceOf[js.Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](t: js.Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)] =
    Some(t)

  @inline implicit def fromScalaTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)): js.Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)

  @inline implicit def toScalaTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](t: js.Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)
}

/**
 *  A tuple "view" of 22 elements of a JavaScript [[js.Array]].
 *
 *  @see [[js.Tuple2]]
 */
sealed trait Tuple22[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20, +T21, +T22] extends js.Object {
  @JSName("0") val _1: T1
  @JSName("1") val _2: T2
  @JSName("2") val _3: T3
  @JSName("3") val _4: T4
  @JSName("4") val _5: T5
  @JSName("5") val _6: T6
  @JSName("6") val _7: T7
  @JSName("7") val _8: T8
  @JSName("8") val _9: T9
  @JSName("9") val _10: T10
  @JSName("10") val _11: T11
  @JSName("11") val _12: T12
  @JSName("12") val _13: T13
  @JSName("13") val _14: T14
  @JSName("14") val _15: T15
  @JSName("15") val _16: T16
  @JSName("16") val _17: T17
  @JSName("17") val _18: T18
  @JSName("18") val _19: T19
  @JSName("19") val _20: T20
  @JSName("20") val _21: T21
  @JSName("21") val _22: T22
}

object Tuple22 {
  @inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9, _10: T10, _11: T11, _12: T12, _13: T13, _14: T14, _15: T15, _16: T16, _17: T17, _18: T18, _19: T19, _20: T20, _21: T21, _22: T22): js.Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] =
    js.Array(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22).asInstanceOf[js.Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]

  @inline def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](t: js.Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]): Option[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)] =
    Some(t)

  @inline implicit def fromScalaTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](t: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)): js.Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] =
    apply(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)

  @inline implicit def toScalaTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](t: js.Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]): (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) =
    (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)
}

// scalastyle:on line.size.limit
