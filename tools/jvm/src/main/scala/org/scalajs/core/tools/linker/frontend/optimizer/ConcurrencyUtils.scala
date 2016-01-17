/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.frontend.optimizer

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

  implicit class AtomicAccOps[T](val acc: AtomicAcc[T]) extends AnyVal {
    @inline final def size: Int = acc.get.size

    @inline
    final def +=(x: T): Unit = AtomicAccOps.append(acc, x)

    @inline
    final def removeAll(): List[T] = AtomicAccOps.removeAll(acc)
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

  implicit class TrieSetOps[T](val set: TrieSet[T]) extends AnyVal {
    @inline final def +=(x: T): Unit = set.put(x, null)
  }

  object TrieSet {
    @inline final def empty[T]: TrieSet[T] = TrieMap.empty
  }

  implicit class TrieMapOps[K,V](val map: TrieMap[K,V]) extends AnyVal {
    @inline final def getOrPut(k: K, default: => V): V = {
      map.get(k).getOrElse {
        val v = default
        map.putIfAbsent(k, v).getOrElse(v)
      }
    }
  }

}
