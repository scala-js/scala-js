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

package org.scalajs.linker.frontend.optimizer

import language.higherKinds

private[optimizer] trait AbsCollOps {
  type ParIterable[V] <: AnyRef
  type Addable[V] <: AnyRef

  def parThreshold: Long

  def emptyParIterable[V]: ParIterable[V]
  def emptyAddable[V]: Addable[V]

  // Operations on ParIterable
  def prepAdd[V](it: ParIterable[V]): Addable[V]
  def add[V](addable: Addable[V], v: V): Unit
  def finishAdd[V](addable: Addable[V]): ParIterable[V]
  def count[V](it: ParIterable[V])(f: V => Boolean): Int
  def foreach[V, U](it: ParIterable[V])(f: V => U): Unit
  def filter[V](it: ParIterable[V])(f: V => Boolean): ParIterable[V]

}
