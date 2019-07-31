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

class LinkedHashSet[E] private[util] (inner: LinkedHashMap[E, Any])
    extends HashSet[E](inner) with Set[E] with Cloneable with Serializable {

  def this(initialCapacity: Int, loadFactor: Float) =
    this(new LinkedHashMap[E, Any](initialCapacity, loadFactor))

  def this(initialCapacity: Int) =
    this(new LinkedHashMap[E, Any](initialCapacity))

  def this() =
    this(new LinkedHashMap[E, Any]())

  def this(c: java.util.Collection[_ <: E]) = {
    this(c.size())
    addAll(c)
  }

}
