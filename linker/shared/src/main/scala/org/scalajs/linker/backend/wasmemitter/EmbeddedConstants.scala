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

package org.scalajs.linker.backend.wasmemitter

object EmbeddedConstants {
  /* Values returned by the `jsValueType` helper.
   *
   * 0: false
   * 1: true
   * 2: string
   * 3: number
   * 4: undefined
   * 5: everything else
   *
   * This encoding has the following properties:
   *
   * - false and true also return their value as the appropriate i32.
   * - the types implementing `Comparable` are consecutive from 0 to 3.
   */

  final val JSValueTypeFalse = 0
  final val JSValueTypeTrue = 1
  final val JSValueTypeString = 2
  final val JSValueTypeNumber = 3
  final val JSValueTypeUndefined = 4
  final val JSValueTypeBigInt = 5
  final val JSValueTypeSymbol = 6
  final val JSValueTypeOther = 7

  // Values for `typeData.kind`

  final val KindVoid = 0
  final val KindBoolean = 1
  final val KindChar = 2
  final val KindByte = 3
  final val KindShort = 4
  final val KindInt = 5
  final val KindLong = 6
  final val KindFloat = 7
  final val KindDouble = 8
  final val KindArray = 9
  final val KindObject = 10 // j.l.Object
  final val KindBoxedUnit = 11
  final val KindBoxedBoolean = 12
  final val KindBoxedCharacter = 13
  final val KindBoxedByte = 14
  final val KindBoxedShort = 15
  final val KindBoxedInteger = 16
  final val KindBoxedLong = 17
  final val KindBoxedFloat = 18
  final val KindBoxedDouble = 19
  final val KindBoxedString = 20
  final val KindClass = 21
  final val KindInterface = 22
  final val KindJSType = 23

  final val KindLastPrimitive = KindDouble
}
