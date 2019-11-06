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

final class IncOptimizer(config: CommonPhaseConfig)
    extends GenIncOptimizer(config) {

  private[optimizer] object CollOps extends GenIncOptimizer.AbsCollOps {
    type Map[K, V] = mutable.Map[K, V]
    type ParMap[K, V] = mutable.Map[K, V]
    type AccMap[K, V] = mutable.Map[K, mutable.ListBuffer[V]]
    type ParIterable[V] = mutable.ListBuffer[V]
    type Addable[V] = mutable.ListBuffer[V]

    def emptyAccMap[K, V]: AccMap[K, V] = mutable.Map.empty
    def emptyMap[K, V]: Map[K, V] = mutable.Map.empty
    def emptyParMap[K, V]: ParMap[K, V] = mutable.Map.empty
    def emptyParIterable[V]: ParIterable[V] = mutable.ListBuffer.empty

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

    def parFlatMapKeys[A, B](map: AccMap[A, _])(
        f: A => Option[B]): ParIterable[B] =
        emptyParIterable[B] ++= map.keys.flatMap(f(_))

    // Operations on ParIterable
    def prepAdd[V](it: ParIterable[V]): Addable[V] = it
    def add[V](addable: Addable[V], v: V): Unit = addable += v
    def finishAdd[V](addable: Addable[V]): ParIterable[V] = addable
    def foreach[V, U](it: ParIterable[V])(f: V => U): Unit = it.foreach(f)

    def filter[V](it: ParIterable[V])(f: V => Boolean): ParIterable[V] =
      it.filter(f)
  }

  private val _interfaces = mutable.Map.empty[ClassName, InterfaceType]
  private[optimizer] def getInterface(className: ClassName): InterfaceType =
    _interfaces.getOrElseUpdate(className, new SeqInterfaceType(className))

  private val methodsToProcess = mutable.ListBuffer.empty[MethodImpl]
  private[optimizer] def scheduleMethod(method: MethodImpl): Unit =
    methodsToProcess += method

  private[optimizer] def newMethodImpl(owner: MethodContainer,
      methodName: MethodName): MethodImpl = {
    new SeqMethodImpl(owner, methodName)
  }

  private[optimizer] def processAllTaggedMethods(): Unit = {
    logProcessingMethods(methodsToProcess.count(!_.deleted))
    for (method <- methodsToProcess)
      method.process()
    methodsToProcess.clear()
  }

  private class SeqInterfaceType(className: ClassName)
      extends InterfaceType(className) {

    private val ancestorsAskers = mutable.Set.empty[MethodImpl]
    private val dynamicCallers = mutable.Map.empty[MethodName, mutable.Set[MethodImpl]]

    private val staticCallers =
      Array.fill(MemberNamespace.Count)(mutable.Map.empty[MethodName, mutable.Set[MethodImpl]])

    private var _ancestors: List[ClassName] = className :: Nil

    private var _instantiatedSubclasses: Set[Class] = Set.empty

    def instantiatedSubclasses: Iterable[Class] = _instantiatedSubclasses

    def addInstantiatedSubclass(x: Class): Unit =
      _instantiatedSubclasses += x

    def removeInstantiatedSubclass(x: Class): Unit =
      _instantiatedSubclasses -= x

    def ancestors: List[ClassName] = _ancestors

    def ancestors_=(v: List[ClassName]): Unit = {
      if (v != _ancestors) {
        _ancestors = v
        ancestorsAskers.foreach(_.tag())
        ancestorsAskers.clear()
      }
    }

    def registerAskAncestors(asker: MethodImpl): Unit =
      ancestorsAskers += asker

    def registerDynamicCaller(methodName: MethodName, caller: MethodImpl): Unit =
      dynamicCallers.getOrElseUpdate(methodName, mutable.Set.empty) += caller

    def registerStaticCaller(namespace: MemberNamespace, methodName: MethodName,
        caller: MethodImpl): Unit = {
      staticCallers(namespace.ordinal)
        .getOrElseUpdate(methodName, mutable.Set.empty) += caller
    }

    def unregisterDependee(dependee: MethodImpl): Unit = {
      ancestorsAskers -= dependee
      dynamicCallers.values.foreach(_ -= dependee)
      staticCallers.foreach(_.values.foreach(_ -= dependee))
    }

    def tagDynamicCallersOf(methodName: MethodName): Unit =
      dynamicCallers.remove(methodName).foreach(_.foreach(_.tag()))

    def tagStaticCallersOf(namespace: MemberNamespace,
        methodName: MethodName): Unit = {
      staticCallers(namespace.ordinal)
        .remove(methodName)
        .foreach(_.foreach(_.tag()))
    }
  }

  private class SeqMethodImpl(owner: MethodContainer, methodName: MethodName)
      extends MethodImpl(owner, methodName) {

    private val bodyAskers = mutable.Set.empty[MethodImpl]

    def registerBodyAsker(asker: MethodImpl): Unit =
      bodyAskers += asker

    def unregisterDependee(dependee: MethodImpl): Unit =
      bodyAskers -= dependee

    def tagBodyAskers(): Unit = {
      bodyAskers.foreach(_.tag())
      bodyAskers.clear()
    }

    private var _registeredTo: List[Unregisterable] = Nil
    private var tagged = false

    protected def registeredTo(intf: Unregisterable): Unit =
      _registeredTo ::= intf

    protected def unregisterFromEverywhere(): Unit = {
      _registeredTo.foreach(_.unregisterDependee(this))
      _registeredTo = Nil
    }

    protected def protectTag(): Boolean = {
      val res = !tagged
      tagged = true
      res
    }
    protected def resetTag(): Unit = tagged = false

  }

}
