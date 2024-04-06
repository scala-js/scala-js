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

import scala.collection.mutable

private[optimizer] object SeqCollOps extends AbsCollOps {
  type ParIterable[V] = mutable.ListBuffer[V]
  type Addable[V] = mutable.ListBuffer[V]

  def parThreshold: Long = Long.MaxValue // no parallelism

  def emptyParIterable[V]: ParIterable[V] = mutable.ListBuffer.empty
  def emptyAddable[V]: Addable[V] = mutable.ListBuffer.empty

  // Operations on ParIterable
  def prepAdd[V](it: ParIterable[V]): Addable[V] = it
  def add[V](addable: Addable[V], v: V): Unit = addable += v
  def finishAdd[V](addable: Addable[V]): ParIterable[V] = addable
  def count[V](it: ParIterable[V])(f: V => Boolean): Int = it.count(f)
  def foreach[V, U](it: ParIterable[V])(f: V => U): Unit = it.foreach(f)

  def filter[V](it: ParIterable[V])(f: V => Boolean): ParIterable[V] =
    it.filter(f)
}
