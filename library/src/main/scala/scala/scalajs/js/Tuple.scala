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

import scala.language.implicitConversions

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** A tuple "view" of 2 elements of a JavaScript [[js.Array]].
 *
 *  Supports implicit conversions to and from [[scala.Tuple2]].
 *
 *  To use it, cast your array into a [[js.Tuple2]] using
 *  {{{
 *  val array = js.Array[Any](42, "foobar")
 *  val tuple2 = array.asInstanceOf[js.Tuple2[Int, String]]
 *  }}}
 *  or convert a Scala tuple
 *  {{{
 *  val obj: js.Tuple2[Int, String] = (42, "foobar")
 *  }}}
 */
sealed trait Tuple2[+T1, +T2] extends js.Object {
  @JSName("0") val _1: T1
  @JSName("1") val _2: T2
}

object Tuple2 {
  @inline def apply[T1, T2](_1: T1, _2: T2): js.Tuple2[T1, T2] =
    js.Array(_1, _2).asInstanceOf[js.Tuple2[T1, T2]]

  @inline def unapply[T1, T2](t: js.Tuple2[T1, T2]): Some[(T1, T2)] =
    Some(t)

  // For binary compatibility
  @inline protected def unapply[T1, T2, Dummy](t: js.Tuple2[T1, T2]): Option[(T1, T2)] =
    Some(t)

  @inline implicit def fromScalaTuple2[T1, T2](t: (T1, T2)): js.Tuple2[T1, T2] =
    apply(t._1, t._2)

  @inline implicit def toScalaTuple2[T1, T2](t: js.Tuple2[T1, T2]): (T1, T2) =
    (t._1, t._2)
}

/** A tuple "view" of 3 elements of a JavaScript [[js.Array]].
 *
 *  @see [[js.Tuple2]]
 */
sealed trait Tuple3[+T1, +T2, +T3] extends js.Object {
  @JSName("0") val _1: T1
  @JSName("1") val _2: T2
  @JSName("2") val _3: T3
}

object Tuple3 {
  @inline def apply[T1, T2, T3](_1: T1, _2: T2, _3: T3): js.Tuple3[T1, T2, T3] =
    js.Array(_1, _2, _3).asInstanceOf[js.Tuple3[T1, T2, T3]]

  @inline def unapply[T1, T2, T3](t: js.Tuple3[T1, T2, T3]): Some[(T1, T2, T3)] =
    Some(t)

  // For binary compatibility
  @inline protected def unapply[T1, T2, T3, Dummy](t: js.Tuple3[T1, T2, T3]): Option[(T1, T2, T3)] =
    Some(t)

  @inline implicit def fromScalaTuple3[T1, T2, T3](t: (T1, T2, T3)): js.Tuple3[T1, T2, T3] =
    apply(t._1, t._2, t._3)

  @inline implicit def toScalaTuple3[T1, T2, T3](t: js.Tuple3[T1, T2, T3]): (T1, T2, T3) =
    (t._1, t._2, t._3)
}
