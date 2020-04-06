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
  def analyze(classes: Map[ClassName, Set[(ClassName, String)]]): ModuleAnalysis = {
    val analyzer = new ModuleAnalyzer(classes)
    analyzer.analyze()
    analyzer
  }

  trait ModuleAnalysis {
    def modules: Seq[Module]
    def moduleForClass(name: ClassName): Option[Int]
  }

  final class Module(val id: Int, val deps: Map[Int, Set[String]], val classes: Seq[ClassName])

  private final class Node(val clazz: ClassName, val index: Int, var compId: Int)
  private sealed trait Result
  private final case class Partial(lowlink: Int, nodes: Vector[Node], deps: Map[Int, Set[String]]) extends Result
  private final case class Component(id: Int) extends Result
}

private class ModuleAnalyzer(classes: Map[ClassName, Set[(ClassName, String)]])
    extends ModuleAnalyzer.ModuleAnalysis {
  import ModuleAnalyzer._

  private[this] var nextIndex = 0
  private[this] val visitedNodes = mutable.Map.empty[ClassName, Node]
  var modules: List[Module] = Nil

  def moduleForClass(name: ClassName): Option[Int] =
    visitedNodes.get(name).map(_.compId)

  def analyze(): Unit = {
    classes
      .keysIterator
      .filter(!visitedNodes.contains(_))
      .foreach(strongconnect(_))
  }

  private def strongconnect(clazz: ClassName): Result = {
    val v = newNode(clazz)

    val result = classes(clazz).foldLeft(
      Partial(v.index, Vector(v), Map.empty)
    ) { case (acc, (clazzDep, fieldDep)) =>
      val Partial(thisLowlink, thisNodes, thisDeps) = acc

      visitedNodes.get(clazzDep).fold {
        strongconnect(clazzDep) match {
          case Partial(thatLowlink, thatNodes, thatDeps) =>
            Partial(math.min(thisLowlink, thatLowlink),
                thisNodes ++ thatNodes, thisDeps ++ thatDeps)

          case Component(id) =>
            val newDeps = thisDeps.updated(id, thisDeps.getOrElse(id, Set.empty) + fieldDep)
            Partial(thisLowlink, thisNodes, newDeps)
        }
      } { w =>
        if (w.compId == -1) {
          // This is a back link.
          Partial(math.min(thisLowlink, w.index), thisNodes, thisDeps)
        } else {
          val id = w.compId
          val newDeps = thisDeps.updated(id, thisDeps.getOrElse(id, Set.empty) + fieldDep)
          Partial(thisLowlink, thisNodes, newDeps)
        }
      }
    }

    if (result.lowlink == v.index) {
      // This node is the root node of a component/module.
      val Partial(id, nodes, deps) = result
      nodes.foreach(_.compId = id)
      val classes = nodes.map(_.clazz)

      modules ::= new Module(id, deps, classes)

      Component(id)
    } else {
      result
    }
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
