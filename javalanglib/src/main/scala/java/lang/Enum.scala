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

package java.lang

abstract class Enum[E <: Enum[E]] protected (_name: String, _ordinal: Int)
    extends Comparable[E] with java.io.Serializable {

  final def name(): String = _name

  final def ordinal(): Int = _ordinal

  override def toString(): String = _name

  @inline
  override final def equals(that: Any): scala.Boolean = super.equals(that)

  @inline
  override final def hashCode(): Int = super.hashCode()

  override protected final def clone(): AnyRef =
    throw new CloneNotSupportedException("Enums are not cloneable")

  final def compareTo(o: E): Int = Integer.compare(_ordinal, o.ordinal)

  // Not implemented:
  // final def getDeclaringClass(): Class[E]

  override protected final def finalize(): Unit = ()
}

// Not implemented:
// def valueOf[T <: Enum[T]](enumType: Class[T], name:String): T
