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

package java.util

import scala.collection.mutable

class LinkedHashSet[E] extends HashSet[E] with Set[E]
                                          with Cloneable
                                          with Serializable {
  def this(initialCapacity: Int, loadFactor: Float) =
    this()

  def this(initialCapacity: Int) =
    this()

  def this(c: java.util.Collection[_ <: E]) = {
    this()
    addAll(c)
  }

  override protected val inner: mutable.Set[Box[E]] =
    new mutable.LinkedHashSet[Box[E]]()

}
