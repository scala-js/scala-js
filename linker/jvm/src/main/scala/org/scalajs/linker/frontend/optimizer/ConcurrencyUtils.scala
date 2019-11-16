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

import java.util.concurrent.atomic._

private[optimizer] object ConcurrencyUtils {

  /** An atomic accumulator supports adding single elements and retrieving and
   *  deleting all contained elements */
  type AtomicAcc[T] = AtomicReference[List[T]]

  object AtomicAcc {
    @inline final def empty[T]: AtomicAcc[T] =
      new AtomicReference[List[T]](Nil)
    @inline final def apply[T](l: List[T]): AtomicAcc[T] =
      new AtomicReference(l)
  }

  implicit class AtomicAccOps[T] private[ConcurrencyUtils] (
      private val self: AtomicAcc[T])
      extends AnyVal {

    @inline final def size: Int = self.get.size

    @inline
    final def +=(x: T): Unit = AtomicAccOps.append(self, x)

    @inline
    final def removeAll(): List[T] = AtomicAccOps.removeAll(self)
  }

  object AtomicAccOps {
    @inline
    @tailrec
    private final def append[T](acc: AtomicAcc[T], x: T): Boolean = {
      val oldV = acc.get
      val newV = x :: oldV
      acc.compareAndSet(oldV, newV) || append(acc, x)
    }

    @inline
    private final def removeAll[T](acc: AtomicAcc[T]): List[T] =
      acc.getAndSet(Nil)
  }

  type TrieSet[T] = TrieMap[T, Null]

  implicit class TrieSetOps[T] private[ConcurrencyUtils] (
      private val self: TrieSet[T])
      extends AnyVal {

    @inline final def +=(x: T): Unit = self.put(x, null)
  }

  object TrieSet {
    @inline final def empty[T]: TrieSet[T] = TrieMap.empty
  }

  implicit class TrieMapOps[K, V] private[ConcurrencyUtils] (
      private val self: TrieMap[K, V])
      extends AnyVal {

    @inline final def getOrPut(k: K, default: => V): V = {
      self.get(k).getOrElse {
        val v = default
        self.putIfAbsent(k, v).getOrElse(v)
      }
    }
  }

}
