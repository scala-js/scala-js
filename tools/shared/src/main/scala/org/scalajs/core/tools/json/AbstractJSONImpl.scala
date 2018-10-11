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

package org.scalajs.core.tools.json

import java.io.{Reader, Writer}

/** A JSON implementation. Has a representation type and methods to convert
 *  this type to/from primitives, lists and maps.
 *
 *  Further, it can write/read a value of this type to a string.
 */
private[json] trait AbstractJSONImpl {

  type Repr

  def fromString(x: String): Repr
  def fromNumber(x: Number): Repr
  def fromBoolean(x: Boolean): Repr
  def fromList(x: List[Repr]): Repr
  def fromMap(x: Map[String, Repr]): Repr

  def toString(x: Repr): String
  def toNumber(x: Repr): Number
  def toBoolean(x: Repr): Boolean
  def toList(x: Repr): List[Repr]
  def toMap(x: Repr): Map[String, Repr]

  def serialize(x: Repr): String
  def serialize(x: Repr, writer: Writer): Unit

  def deserialize(str: String): Repr
  def deserialize(reader: Reader): Repr

}
