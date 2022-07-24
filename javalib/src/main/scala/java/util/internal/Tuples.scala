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

package java.util.internal

@inline
final class Tuple2[+T1, +T2](val _1: T1, val _2: T2)

@inline
final class Tuple3[+T1, +T2, +T3](val _1: T1, val _2: T2, val _3: T3)

@inline
final class Tuple4[+T1, +T2, +T3, +T4](val _1: T1, val _2: T2, val _3: T3, val _4: T4)

@inline
final class Tuple8[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8](
    val _1: T1, val _2: T2, val _3: T3, val _4: T4, val _5: T5, val _6: T6, val _7: T7, val _8: T8)
