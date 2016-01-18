/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.frontend.optimizer

import scala.collection.{GenTraversableOnce, GenIterable}
import scala.collection.concurrent.TrieMap
import scala.collection.parallel.mutable.{ParTrieMap, ParArray}

import java.util.concurrent.atomic._

import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.javascript.ESLevel

import ConcurrencyUtils._

final class ParIncOptimizer(semantics: Semantics, esLevel: ESLevel,
    considerPositions: Boolean)
    extends GenIncOptimizer(semantics, esLevel, considerPositions) {

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
    def put[K, V](map: ParMap[K, V], k: K, v: V): Unit = map.put(k, v)
    def remove[K, V](map: ParMap[K, V], k: K): Option[V] = map.remove(k)

    def retain[K, V](map: ParMap[K, V])(p: (K, V) => Boolean): Unit = {
      map.foreach { case (k, v) =>
        if (!p(k, v))
          map.remove(k)
      }
    }

    // Operations on AccMap
    def acc[K, V](map: AccMap[K, V], k: K, v: V): Unit =
      map.getOrPut(k, AtomicAcc.empty) += v

    def getAcc[K, V](map: AccMap[K, V], k: K): GenIterable[V] =
      map.get(k).fold[Iterable[V]](Nil)(_.removeAll()).toParArray

    def parFlatMapKeys[A, B](map: AccMap[A, _])(
        f: A => GenTraversableOnce[B]): GenIterable[B] =
      map.keys.flatMap(f).toParArray

    // Operations on ParIterable
    def prepAdd[V](it: ParIterable[V]): Addable[V] =
      AtomicAcc(it.toList)

    def add[V](addable: Addable[V], v: V): Unit =
      addable += v

    def finishAdd[V](addable: Addable[V]): ParIterable[V] =
      addable.removeAll().toParArray
  }

  private val _interfaces = TrieMap.empty[String, InterfaceType]
  private[optimizer] def getInterface(encodedName: String): InterfaceType =
    _interfaces.getOrPut(encodedName, new ParInterfaceType(encodedName))

  private val methodsToProcess: AtomicAcc[MethodImpl] = AtomicAcc.empty
  private[optimizer] def scheduleMethod(method: MethodImpl): Unit =
    methodsToProcess += method

  private[optimizer] def newMethodImpl(owner: MethodContainer,
      encodedName: String): MethodImpl = new ParMethodImpl(owner, encodedName)

  private[optimizer] def processAllTaggedMethods(): Unit = {
    val methods = methodsToProcess.removeAll().toParArray
    logProcessingMethods(methods.count(!_.deleted))
    for (method <- methods)
      method.process()
  }

  private class ParInterfaceType(encName: String) extends InterfaceType(encName) {
    private val ancestorsAskers = TrieSet.empty[MethodImpl]
    private val dynamicCallers = TrieMap.empty[String, TrieSet[MethodImpl]]
    private val staticCallers = TrieMap.empty[String, TrieSet[MethodImpl]]
    private val callersOfStatic = TrieMap.empty[String, TrieSet[MethodImpl]]

    private var _ancestors: List[String] = encodedName :: Nil

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
    def ancestors: List[String] = _ancestors

    /** UPDATE PASS ONLY. Not concurrency safe. */
    def ancestors_=(v: List[String]): Unit = {
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
    def registerDynamicCaller(methodName: String, caller: MethodImpl): Unit =
      dynamicCallers.getOrPut(methodName, TrieSet.empty) += caller

    /** PROCESS PASS ONLY. */
    def registerStaticCaller(methodName: String, caller: MethodImpl): Unit =
      staticCallers.getOrPut(methodName, TrieSet.empty) += caller

    /** PROCESS PASS ONLY. */
    def registerCallerOfStatic(methodName: String, caller: MethodImpl): Unit =
      callersOfStatic.getOrPut(methodName, TrieSet.empty) += caller

    /** UPDATE PASS ONLY. */
    def unregisterDependee(dependee: MethodImpl): Unit = {
      ancestorsAskers -= dependee
      dynamicCallers.valuesIterator.foreach(_ -= dependee)
      staticCallers.valuesIterator.foreach(_ -= dependee)
      callersOfStatic.valuesIterator.foreach(_ -= dependee)
    }

    /** UPDATE PASS ONLY. */
    def tagDynamicCallersOf(methodName: String): Unit =
      dynamicCallers.remove(methodName).foreach(_.keysIterator.foreach(_.tag()))

    /** UPDATE PASS ONLY. */
    def tagStaticCallersOf(methodName: String): Unit =
      staticCallers.remove(methodName).foreach(_.keysIterator.foreach(_.tag()))

    /** UPDATE PASS ONLY. */
    def tagCallersOfStatic(methodName: String): Unit =
      callersOfStatic.remove(methodName).foreach(_.keysIterator.foreach(_.tag()))
  }

  private class ParMethodImpl(owner: MethodContainer,
      encodedName: String) extends MethodImpl(owner, encodedName) {

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

object ParIncOptimizer {
  val factory: GenIncOptimizer.OptimizerFactory = new ParIncOptimizer(_, _, _)
}
