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

import scala.collection.concurrent.TrieMap
import scala.collection.parallel.mutable.{ParTrieMap, ParArray}
import scala.collection.parallel._

import java.util.concurrent.atomic._

private[optimizer] object ParCollOps extends AbsCollOps {
  type Map[K, V] = TrieMap[K, V]
  type ParMap[K, V] = ParTrieMap[K, V]
  type AccMap[K, V] = TrieMap[K, Addable[V]]
  type ParIterable[V] = ParArray[V]
  type Addable[V] = AtomicReference[List[V]]

  def emptyAccMap[K, V]: AccMap[K, V] = TrieMap.empty
  def emptyMap[K, V]: Map[K, V] = TrieMap.empty
  def emptyParMap[K, V]: ParMap[K, V] =  ParTrieMap.empty
  def emptyParIterable[V]: ParIterable[V] = ParArray.empty
  def emptyAddable[V]: Addable[V] = new AtomicReference[List[V]](Nil)

  // Operations on ParMap
  def isEmpty[K, V](map: ParMap[K, V]): Boolean = map.isEmpty
  def forceGet[K, V](map: ParMap[K, V], k: K): V = map(k)
  def get[K, V](map: ParMap[K, V], k: K): Option[V] = map.get(k)
  def put[K, V](map: ParMap[K, V], k: K, v: V): Unit = map.put(k, v)
  def remove[K, V](map: ParMap[K, V], k: K): Option[V] = map.remove(k)

  def retain[K, V](map: ParMap[K, V])(p: (K, V) => Boolean): Unit = {
    map.foreach { case (k, v) =>
      if (!p(k, v))
        map.remove(k)
    }
  }

  def valuesForeach[K, V, U](map: ParMap[K, V])(f: V => U): Unit =
    map.values.foreach(f)

  // Operations on AccMap
  def acc[K, V](map: AccMap[K, V], k: K, v: V): Unit =
    add(map.getOrElseUpdate(k, emptyAddable), v)

  def getAcc[K, V](map: AccMap[K, V], k: K): ParIterable[V] =
    map.get(k).fold(emptyParIterable[V])(finishAdd(_))

  def parFlatMapKeys[A, B](map: AccMap[A, _])(f: A => Option[B]): ParIterable[B] =
    map.keys.flatMap(f(_)).toParArray

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
