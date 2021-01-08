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

import scala.collection.mutable

private[optimizer] trait AbsCollOps {
  type Map[K, V] <: mutable.Map[K, V]
  type ParMap[K, V] <: AnyRef
  type AccMap[K, V] <: AnyRef
  type ParIterable[V] <: AnyRef
  type Addable[V] <: AnyRef

  def emptyAccMap[K, V]: AccMap[K, V]
  def emptyMap[K, V]: Map[K, V]
  def emptyParMap[K, V]: ParMap[K, V]
  def emptyParIterable[V]: ParIterable[V]
  def emptyAddable[V]: Addable[V]

  // Operations on ParMap
  def isEmpty[K, V](map: ParMap[K, V]): Boolean
  def forceGet[K, V](map: ParMap[K, V], k: K): V
  def get[K, V](map: ParMap[K, V], k: K): Option[V]
  def put[K, V](map: ParMap[K, V], k: K, v: V): Unit
  def remove[K, V](map: ParMap[K, V], k: K): Option[V]
  def retain[K, V](map: ParMap[K, V])(p: (K, V) => Boolean): Unit
  def valuesForeach[K, V, U](map: ParMap[K, V])(f: V => U): Unit

  // Operations on AccMap
  def acc[K, V](map: AccMap[K, V], k: K, v: V): Unit
  def getAcc[K, V](map: AccMap[K, V], k: K): ParIterable[V]
  def parFlatMapKeys[A, B](map: AccMap[A, _])(f: A => Option[B]): ParIterable[B]

  // Operations on ParIterable
  def prepAdd[V](it: ParIterable[V]): Addable[V]
  def add[V](addable: Addable[V], v: V): Unit
  def finishAdd[V](addable: Addable[V]): ParIterable[V]
  def count[V](it: ParIterable[V])(f: V => Boolean): Int
  def foreach[V, U](it: ParIterable[V])(f: V => U): Unit
  def filter[V](it: ParIterable[V])(f: V => Boolean): ParIterable[V]

}
