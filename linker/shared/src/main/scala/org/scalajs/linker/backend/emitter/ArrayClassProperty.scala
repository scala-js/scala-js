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

package org.scalajs.linker.backend.emitter

import org.scalajs.ir.OriginalName

/** Represents a property of one of the special `ArrayClass`es.
 *
 *  These properties live in the same namespace as Scala field and method
 *  names, because the `ArrayClass`es extend `j.l.Object`. Therefore, they
 *  must take part in the global property minification algorithm.
 */
final class ArrayClassProperty(val nonMinifiedName: String)
    extends Comparable[ArrayClassProperty] {

  val originalName: OriginalName = OriginalName(nonMinifiedName)

  def compareTo(that: ArrayClassProperty): Int =
    this.nonMinifiedName.compareTo(that.nonMinifiedName)

  override def toString(): String = s"ArrayClassProperty($nonMinifiedName)"
}

object ArrayClassProperty {
  /** `ArrayClass.u`: the underlying array of typed array. */
  val u: ArrayClassProperty = new ArrayClassProperty("u")

  /** `ArrayClass.get()`: gets one element. */
  val get: ArrayClassProperty = new ArrayClassProperty("get")

  /** `ArrayClass.set()`: sets one element. */
  val set: ArrayClassProperty = new ArrayClassProperty("set")

  /** `ArrayClass.copyTo()`: copies from that array to another array. */
  val copyTo: ArrayClassProperty = new ArrayClassProperty("copyTo")

  /** Abuse: `jl.Class.data`: the underlying `TypeData` of the class.
   *
   *  That property also lives in the same namespace as Scala field and method
   *  names.
   */
  val data: ArrayClassProperty = new ArrayClassProperty("data")
}
