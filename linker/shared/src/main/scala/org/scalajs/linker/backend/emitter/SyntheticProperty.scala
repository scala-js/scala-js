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

/** Represents a synthetic property of some Scala classes.
 *
 *  A synthetic property does not exist as `FieldDef` or `MethodDef` in the IR,
 *  but is added by the Emitter on some classes that belong to the Scala class
 *  hierarchy.
 *
 *  Currently, these only include properties of the `ArrayClass`es, as well as
 *  the magic `jl.Class.data` property.
 *
 *  These properties live in the same namespace as Scala field and method
 *  names, because the `ArrayClass`es extend `j.l.Object`. Therefore, they
 *  must take part in the global property minification algorithm.
 */
final class SyntheticProperty(val nonMinifiedName: String)
    extends Comparable[SyntheticProperty] {

  val originalName: OriginalName = OriginalName(nonMinifiedName)

  def compareTo(that: SyntheticProperty): Int =
    this.nonMinifiedName.compareTo(that.nonMinifiedName)

  override def toString(): String = s"SyntheticProperty($nonMinifiedName)"
}

object SyntheticProperty {

  /** `ArrayClass.u`: the underlying array or typed array. */
  val u: SyntheticProperty = new SyntheticProperty("u")

  /** `ArrayClass.get()`: gets one element. */
  val get: SyntheticProperty = new SyntheticProperty("get")

  /** `ArrayClass.set()`: sets one element. */
  val set: SyntheticProperty = new SyntheticProperty("set")

  /** `ArrayClass.copyTo()`: copies from that array to another array. */
  val copyTo: SyntheticProperty = new SyntheticProperty("copyTo")

  /** `jl.Class.data`: the underlying `TypeData` of the class. */
  val data: SyntheticProperty = new SyntheticProperty("data")
}
