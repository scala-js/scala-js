/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.frontend.optimizer

import scala.collection.{GenTraversableOnce, GenIterable}
import scala.collection.mutable

import org.scalajs.core.tools.sem.Semantics
import org.scalajs.core.tools.javascript.ESLevel

final class IncOptimizer(semantics: Semantics, esLevel: ESLevel,
    considerPositions: Boolean)
    extends GenIncOptimizer(semantics, esLevel, considerPositions) {

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
    def put[K, V](map: ParMap[K, V], k: K, v: V): Unit = map.put(k, v)
    def remove[K, V](map: ParMap[K, V], k: K): Option[V] = map.remove(k)

    def retain[K, V](map: ParMap[K, V])(p: (K, V) => Boolean): Unit =
      map.retain(p)

    // Operations on AccMap
    def acc[K, V](map: AccMap[K, V], k: K, v: V): Unit =
      map.getOrElseUpdate(k, mutable.ListBuffer.empty) += v

    def getAcc[K, V](map: AccMap[K, V], k: K): GenIterable[V] =
      map.getOrElse(k, Nil)

    def parFlatMapKeys[A, B](map: AccMap[A, _])(
        f: A => GenTraversableOnce[B]): GenIterable[B] =
      map.keys.flatMap(f).toList

    // Operations on ParIterable
    def prepAdd[V](it: ParIterable[V]): Addable[V] = it
    def add[V](addable: Addable[V], v: V): Unit = addable += v
    def finishAdd[V](addable: Addable[V]): ParIterable[V] = addable
  }

  private val _interfaces = mutable.Map.empty[String, InterfaceType]
  private[optimizer] def getInterface(encodedName: String): InterfaceType =
    _interfaces.getOrElseUpdate(encodedName, new SeqInterfaceType(encodedName))

  private val methodsToProcess = mutable.ListBuffer.empty[MethodImpl]
  private[optimizer] def scheduleMethod(method: MethodImpl): Unit =
    methodsToProcess += method

  private[optimizer] def newMethodImpl(owner: MethodContainer,
      encodedName: String): MethodImpl = new SeqMethodImpl(owner, encodedName)

  private[optimizer] def processAllTaggedMethods(): Unit = {
    logProcessingMethods(methodsToProcess.count(!_.deleted))
    for (method <- methodsToProcess)
      method.process()
    methodsToProcess.clear()
  }

  private class SeqInterfaceType(encName: String) extends InterfaceType(encName) {
    private val ancestorsAskers = mutable.Set.empty[MethodImpl]
    private val dynamicCallers = mutable.Map.empty[String, mutable.Set[MethodImpl]]
    private val staticCallers = mutable.Map.empty[String, mutable.Set[MethodImpl]]
    private val callersOfStatic = mutable.Map.empty[String, mutable.Set[MethodImpl]]

    private var _ancestors: List[String] = encodedName :: Nil

    private var _instantiatedSubclasses: Set[Class] = Set.empty

    def instantiatedSubclasses: Iterable[Class] = _instantiatedSubclasses

    def addInstantiatedSubclass(x: Class): Unit =
      _instantiatedSubclasses += x

    def removeInstantiatedSubclass(x: Class): Unit =
      _instantiatedSubclasses -= x

    def ancestors: List[String] = _ancestors

    def ancestors_=(v: List[String]): Unit = {
      if (v != _ancestors) {
        _ancestors = v
        ancestorsAskers.foreach(_.tag())
        ancestorsAskers.clear()
      }
    }

    def registerAskAncestors(asker: MethodImpl): Unit =
      ancestorsAskers += asker

    def registerDynamicCaller(methodName: String, caller: MethodImpl): Unit =
      dynamicCallers.getOrElseUpdate(methodName, mutable.Set.empty) += caller

    def registerStaticCaller(methodName: String, caller: MethodImpl): Unit =
      staticCallers.getOrElseUpdate(methodName, mutable.Set.empty) += caller

    def registerCallerOfStatic(methodName: String, caller: MethodImpl): Unit =
      callersOfStatic.getOrElseUpdate(methodName, mutable.Set.empty) += caller

    def unregisterDependee(dependee: MethodImpl): Unit = {
      ancestorsAskers -= dependee
      dynamicCallers.values.foreach(_ -= dependee)
      staticCallers.values.foreach(_ -= dependee)
      callersOfStatic.values.foreach(_ -= dependee)
    }

    def tagDynamicCallersOf(methodName: String): Unit =
      dynamicCallers.remove(methodName).foreach(_.foreach(_.tag()))

    def tagStaticCallersOf(methodName: String): Unit =
      staticCallers.remove(methodName).foreach(_.foreach(_.tag()))

    def tagCallersOfStatic(methodName: String): Unit =
      callersOfStatic.remove(methodName).foreach(_.foreach(_.tag()))
  }

  private class SeqMethodImpl(owner: MethodContainer,
      encodedName: String) extends MethodImpl(owner, encodedName) {

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

object IncOptimizer {
  val factory: GenIncOptimizer.OptimizerFactory = new IncOptimizer(_, _, _)
}
