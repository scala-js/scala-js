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

package org.scalajs.linker.analyzer

import scala.annotation.tailrec
import scala.collection.mutable

import org.scalajs.ir.Names.{ClassName, ObjectClass}
import org.scalajs.ir.Trees.MemberNamespace

object ModuleAnalyzer {
  def analyze(analysis: Analysis): ModuleAnalysis = {
    val analyzer = new ModuleAnalyzer(analysis)
    analyzer.analyze()
    analyzer
  }

  trait ModuleAnalysis {
    val moduleDeps: scala.collection.Map[Int, Set[Int]]
    def moduleForClass(name: ClassName): Option[Int]
  }

  private final class Node(val clazz: ClassName, val index: Int, var compId: Int)
  private sealed trait Result
  private final case class Partial(lowlink: Int, nodes: Vector[Node], deps: Set[Int]) extends Result
  private final case class Component(id: Int) extends Result
}

private class ModuleAnalyzer(analysis: Analysis) extends ModuleAnalyzer.ModuleAnalysis {
  import ModuleAnalyzer._

  private[this] var nextIndex = 0
  private[this] val visitedNodes = mutable.Map.empty[ClassName, Node]
  val moduleDeps = mutable.Map.empty[Int, Set[Int]]

  def moduleForClass(name: ClassName): Option[Int] =
    visitedNodes.get(name).map(_.compId)

  def analyze(): Unit = {
    /* Whether this class generates code.
     * 
     * This is different from whether it is required. We might still need
     * information about it (e.g. load spec) but not actually generate any
     * definition.
     */
    def generatesCode(i: Analysis.ClassInfo): Boolean = {
      def hasReachableMethods = 
        MemberNamespace.all.exists(ns => i.methodInfos(ns).valuesIterator.exists(_.isReachable))

      if (i.kind.isAnyNonNativeClass) {
        i.isModuleAccessed ||
        i.areInstanceTestsUsed ||
        i.isDataAccessed ||
        i.isAnyStaticFieldUsed ||
        i.isAnyPrivateJSFieldUsed ||
        hasReachableMethods
      } else {
        i.isDataAccessed || i.isAnyStaticFieldUsed || hasReachableMethods
      }
    }

    analysis.classInfos
      .valuesIterator
      .filter(generatesCode)
      .filter(i => !visitedNodes.contains(i.className))
      .foreach(i => strongconnect(i.className))
  }

  private def strongconnect(clazz: ClassName): Result = {
    val v = newNode(clazz)

    val result = dependencies(clazz).foldLeft(
      Partial(v.index, Vector(v), Set.empty)
    ) { (acc, dep) =>
      val Partial(thisLowlink, thisNodes, thisDeps) = acc

      visitedNodes.get(dep).fold {
        strongconnect(dep) match {
          case Partial(thatLowlink, thatNodes, thatDeps) =>
            Partial(math.min(thisLowlink, thatLowlink),
                thisNodes ++ thatNodes, thisDeps ++ thatDeps)

          case Component(id) =>
            Partial(thisLowlink, thisNodes, thisDeps + id)
        }
      } { w =>
        if (w.compId == -1) {
          // This is a back link.
          Partial(math.min(thisLowlink, w.index), thisNodes, thisDeps)
        } else {
          Partial(thisLowlink, thisNodes, thisDeps + w.compId)
        }
      }
    }

    if (result.lowlink == v.index) {
      // This node is the root node of a component/module.
      val Partial(id, nodes, deps) = result
      nodes.foreach(_.compId = id)
      moduleDeps.put(id, deps)

      Component(id)
    } else {
      result
    }
  }

  private def dependencies(clazz: ClassName) = {
    val deps = analysis.classInfos(clazz).classDependencies

    /* Artifically depend on j.l.Object.
     * This makes sure we depend on the CoreJSLib (identified by j.l.Object).
     */
    if (clazz != ObjectClass) deps + ObjectClass
    else deps
  }

  private def newNode(clazz: ClassName): Node = {
    assert(!visitedNodes.contains(clazz))

    val i = nextIndex
    nextIndex += 1
    val node = new Node(clazz, i, -1)
    visitedNodes += clazz -> node
    node
  }
}
