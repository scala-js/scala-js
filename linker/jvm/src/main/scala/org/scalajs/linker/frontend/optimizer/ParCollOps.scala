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

import scala.annotation.tailrec

import scala.collection.parallel.mutable.ParArray
import scala.collection.parallel._

import java.util.concurrent.atomic._

private[optimizer] object ParCollOps extends AbsCollOps {
  type ParIterable[V] = ParArray[V]
  type Addable[V] = AtomicReference[List[V]]

  def parThreshold: Long = 1 // max parallelism

  def emptyParIterable[V]: ParIterable[V] = ParArray.empty
  def emptyAddable[V]: Addable[V] = new AtomicReference[List[V]](Nil)

  // Operations on ParIterable
  def prepAdd[V](it: ParIterable[V]): Addable[V] =
    new AtomicReference(it.toList)

  @tailrec
  def add[V](addable: Addable[V], v: V): Unit = {
    val oldV = addable.get
    val newV = v :: oldV
    if (!addable.compareAndSet(oldV, newV))
      add(addable, v)
  }

  def finishAdd[V](addable: Addable[V]): ParIterable[V] =
    addable.getAndSet(Nil).toParArray

  def count[V](it: ParIterable[V])(f: V => Boolean): Int =
    it.count(f)

  def foreach[V, U](it: ParIterable[V])(f: V => U): Unit =
    it.foreach(f)

  def filter[V](it: ParIterable[V])(f: V => Boolean): ParIterable[V] =
    it.filter(f)
}
