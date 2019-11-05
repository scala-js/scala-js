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
import scala.collection.concurrent.TrieMap
import scala.collection.parallel.mutable.{ParTrieMap, ParArray}
import scala.collection.parallel._

import java.util.concurrent.atomic._

import org.scalajs.ir.Names.{ClassName, MethodName}
import org.scalajs.ir.Trees.MemberNamespace

import org.scalajs.linker.standard._

import ConcurrencyUtils._

final class ParIncOptimizer(config: CommonPhaseConfig)
    extends GenIncOptimizer(config) {

  private[optimizer] object CollOps extends GenIncOptimizer.AbsCollOps {
    type Map[K, V] = TrieMap[K, V]
    type ParMap[K, V] = ParTrieMap[K, V]
    type AccMap[K, V] = TrieMap[K, AtomicAcc[V]]
    type ParIterable[V] = ParArray[V]
    type Addable[V] = AtomicAcc[V]

    def emptyAccMap[K, V]: AccMap[K, V] = TrieMap.empty
    def emptyMap[K, V]: Map[K, V] = TrieMap.empty
    def emptyParMap[K, V]: ParMap[K, V] =  ParTrieMap.empty
    def emptyParIterable[V]: ParIterable[V] = ParArray.empty

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
      map.getOrPut(k, AtomicAcc.empty) += v

    def getAcc[K, V](map: AccMap[K, V], k: K): ParIterable[V] =
      map.get(k).fold[Iterable[V]](Nil)(_.removeAll()).toParArray

    def parFlatMapKeys[A, B](map: AccMap[A, _])(
        f: A => Option[B]): ParIterable[B] =
      map.keys.flatMap(f(_)).toParArray

    // Operations on ParIterable
    def prepAdd[V](it: ParIterable[V]): Addable[V] =
      AtomicAcc(it.toList)

    def add[V](addable: Addable[V], v: V): Unit =
      addable += v

    def finishAdd[V](addable: Addable[V]): ParIterable[V] =
      addable.removeAll().toParArray

    def foreach[V, U](it: ParIterable[V])(f: V => U): Unit =
      it.foreach(f)

    def filter[V](it: ParIterable[V])(f: V => Boolean): ParIterable[V] =
      it.filter(f)
  }

  private val _interfaces = TrieMap.empty[ClassName, InterfaceType]
  private[optimizer] def getInterface(encodedName: ClassName): InterfaceType =
    _interfaces.getOrPut(encodedName, new ParInterfaceType(encodedName))

  private val methodsToProcess: AtomicAcc[MethodImpl] = AtomicAcc.empty
  private[optimizer] def scheduleMethod(method: MethodImpl): Unit =
    methodsToProcess += method

  private[optimizer] def newMethodImpl(owner: MethodContainer,
      encodedName: MethodName): MethodImpl = {
    new ParMethodImpl(owner, encodedName)
  }

  private[optimizer] def processAllTaggedMethods(): Unit = {
    val methods = methodsToProcess.removeAll().toParArray
    logProcessingMethods(methods.count(!_.deleted))
    for (method <- methods)
      method.process()
  }

  private class ParInterfaceType(encName: ClassName)
      extends InterfaceType(encName) {

    private val ancestorsAskers = TrieSet.empty[MethodImpl]
    private val dynamicCallers = TrieMap.empty[MethodName, TrieSet[MethodImpl]]

    private val staticCallers =
      Array.fill(MemberNamespace.Count)(TrieMap.empty[MethodName, TrieSet[MethodImpl]])

    private var _ancestors: List[ClassName] = encodedName :: Nil

    private val _instantiatedSubclasses: TrieSet[Class] = TrieSet.empty

    /** PROCESS PASS ONLY. Concurrency safe except with
     *  [[addInstantiatedSubclass]] and [[removeInstantiatedSubclass]]
     */
    def instantiatedSubclasses: Iterable[Class] =
      _instantiatedSubclasses.keys

    /** UPDATE PASS ONLY. Concurrency safe except with
     *  [[instantiatedSubclasses]]
     */
    def addInstantiatedSubclass(x: Class): Unit =
      _instantiatedSubclasses += x

    /** UPDATE PASS ONLY. Concurrency safe except with
     *  [[instantiatedSubclasses]]
     */
    def removeInstantiatedSubclass(x: Class): Unit =
      _instantiatedSubclasses -= x

    /** PROCESS PASS ONLY. Concurrency safe except with [[ancestors_=]] */
    def ancestors: List[ClassName] = _ancestors

    /** UPDATE PASS ONLY. Not concurrency safe. */
    def ancestors_=(v: List[ClassName]): Unit = {
      if (v != _ancestors) {
        _ancestors = v
        ancestorsAskers.keysIterator.foreach(_.tag())
        ancestorsAskers.clear()
      }
    }

    /** PROCESS PASS ONLY. Concurrency safe except with [[ancestors_=]]. */
    def registerAskAncestors(asker: MethodImpl): Unit =
      ancestorsAskers += asker

    /** PROCESS PASS ONLY. */
    def registerDynamicCaller(methodName: MethodName, caller: MethodImpl): Unit =
      dynamicCallers.getOrPut(methodName, TrieSet.empty) += caller

    /** PROCESS PASS ONLY. */
    def registerStaticCaller(namespace: MemberNamespace, methodName: MethodName,
        caller: MethodImpl): Unit = {
      staticCallers(namespace.ordinal)
        .getOrPut(methodName, TrieSet.empty) += caller
    }

    /** UPDATE PASS ONLY. */
    def unregisterDependee(dependee: MethodImpl): Unit = {
      ancestorsAskers -= dependee
      dynamicCallers.valuesIterator.foreach(_ -= dependee)
      staticCallers.foreach(_.valuesIterator.foreach(_ -= dependee))
    }

    /** UPDATE PASS ONLY. */
    def tagDynamicCallersOf(methodName: MethodName): Unit =
      dynamicCallers.remove(methodName).foreach(_.keysIterator.foreach(_.tag()))

    /** UPDATE PASS ONLY. */
    def tagStaticCallersOf(namespace: MemberNamespace,
        methodName: MethodName): Unit = {
      staticCallers(namespace.ordinal)
        .remove(methodName)
        .foreach(_.keysIterator.foreach(_.tag()))
    }
  }

  private class ParMethodImpl(owner: MethodContainer, encodedName: MethodName)
      extends MethodImpl(owner, encodedName) {

    private val bodyAskers = TrieSet.empty[MethodImpl]

    /** PROCESS PASS ONLY. */
    def registerBodyAsker(asker: MethodImpl): Unit =
      bodyAskers += asker

    /** UPDATE PASS ONLY. */
    def unregisterDependee(dependee: MethodImpl): Unit =
      bodyAskers -= dependee

    /** UPDATE PASS ONLY. */
    def tagBodyAskers(): Unit = {
      bodyAskers.keysIterator.foreach(_.tag())
      bodyAskers.clear()
    }

    private val _registeredTo = AtomicAcc.empty[Unregisterable]
    private val tagged = new AtomicBoolean(false)

    protected def registeredTo(intf: Unregisterable): Unit =
      _registeredTo += intf

    protected def unregisterFromEverywhere(): Unit = {
      _registeredTo.removeAll().foreach(_.unregisterDependee(this))
    }

    protected def protectTag(): Boolean = !tagged.getAndSet(true)
    protected def resetTag(): Unit = tagged.set(false)

  }

}
