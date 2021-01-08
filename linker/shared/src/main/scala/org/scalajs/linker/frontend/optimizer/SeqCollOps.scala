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

import scala.collection.{GenTraversableOnce, GenIterable}
import scala.collection.mutable

import org.scalajs.ir.Names.{ClassName, MethodName}
import org.scalajs.ir.Trees.MemberNamespace

import org.scalajs.linker.standard._
import org.scalajs.linker.CollectionsCompat.MutableMapCompatOps

private[optimizer] object SeqCollOps extends AbsCollOps {
  type Map[K, V] = mutable.Map[K, V]
  type ParMap[K, V] = mutable.Map[K, V]
  type AccMap[K, V] = mutable.Map[K, mutable.ListBuffer[V]]
  type ParIterable[V] = mutable.ListBuffer[V]
  type Addable[V] = mutable.ListBuffer[V]

  def emptyAccMap[K, V]: AccMap[K, V] = mutable.Map.empty
  def emptyMap[K, V]: Map[K, V] = mutable.Map.empty
  def emptyParMap[K, V]: ParMap[K, V] = mutable.Map.empty
  def emptyParIterable[V]: ParIterable[V] = mutable.ListBuffer.empty
  def emptyAddable[V]: Addable[V] = mutable.ListBuffer.empty

  // Operations on ParMap
  def isEmpty[K, V](map: ParMap[K, V]): Boolean = map.isEmpty
  def forceGet[K, V](map: ParMap[K, V], k: K): V = map(k)
  def get[K, V](map: ParMap[K, V], k: K): Option[V] = map.get(k)
  def put[K, V](map: ParMap[K, V], k: K, v: V): Unit = map.put(k, v)
  def remove[K, V](map: ParMap[K, V], k: K): Option[V] = map.remove(k)

  def retain[K, V](map: ParMap[K, V])(p: (K, V) => Boolean): Unit =
    map.filterInPlace(p)

  def valuesForeach[K, V, U](map: ParMap[K, V])(f: V => U): Unit =
    map.values.foreach(f)

  // Operations on AccMap
  def acc[K, V](map: AccMap[K, V], k: K, v: V): Unit =
    map.getOrElseUpdate(k, mutable.ListBuffer.empty) += v

  def getAcc[K, V](map: AccMap[K, V], k: K): ParIterable[V] =
    map.getOrElse(k, emptyParIterable)

  def parFlatMapKeys[A, B](map: AccMap[A, _])(f: A => Option[B]): ParIterable[B] =
    emptyParIterable[B] ++= map.keys.flatMap(f(_))

  // Operations on ParIterable
  def prepAdd[V](it: ParIterable[V]): Addable[V] = it
  def add[V](addable: Addable[V], v: V): Unit = addable += v
  def finishAdd[V](addable: Addable[V]): ParIterable[V] = addable
  def count[V](it: ParIterable[V])(f: V => Boolean): Int = it.count(f)
  def foreach[V, U](it: ParIterable[V])(f: V => U): Unit = it.foreach(f)

  def filter[V](it: ParIterable[V])(f: V => Boolean): ParIterable[V] =
    it.filter(f)
}
