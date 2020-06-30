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

package org.scalajs.linker.frontend.modulesplitter

import scala.collection.mutable

import org.scalajs.ir.Names.ClassName

import org.scalajs.logging.Logger

import org.scalajs.linker.analyzer.Infos
import org.scalajs.linker.interface.{ModuleInitializer, ModuleKind}
import org.scalajs.linker.interface.unstable.ModuleInitializerImpl
import org.scalajs.linker.frontend.LinkingUnit
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID

private[modulesplitter] final class MinModuleSplitter extends ModuleSplitter {
  def split(unit: LinkingUnit, symbolRequirements: SymbolRequirement,
      logger: Logger): ModuleSet = {
    require(unit.coreSpec.moduleKind != ModuleKind.NoModule)

    new MinModuleAnalyzer(unit, symbolRequirements).analyze()
  }
}

private object MinModuleAnalyzer {
  private final class Node(val clazz: ClassName, val index: Int, var compId: Int)
  private sealed trait Result
  private final case class Partial(lowlink: Int, nodes: Vector[Node], deps: Set[Int]) extends Result
  private final case class Component(id: Int) extends Result
}

private class MinModuleAnalyzer(unit: LinkingUnit, symbolRequirements: SymbolRequirement) {
  import MinModuleAnalyzer._

  private val moduleAnalyzer = new ModuleAnalyzer(unit, symbolRequirements)
  import moduleAnalyzer._

  private[this] var nextIndex = 0
  private[this] val visitedNodes = mutable.Map.empty[ClassName, Node]
  private[this] val internalModules = mutable.Map.empty[Int, ModuleSet.Module]

  def analyze(): ModuleSet = {
    unit.classDefs.iterator
      .filter(_.hasAnyDefinitions)
      .map(_.className)
      .filter(!visitedNodes.contains(_))
      .foreach(strongconnect(_))

    val abstractClasses = unit.classDefs
      .filter(c => !visitedNodes.contains(c.className))

    val badAbstractClasses = abstractClasses.filter(_.hasAnyDefinitions)
    assert(badAbstractClasses.isEmpty, {
      val names = badAbstractClasses.map(_.fullName).mkString(", ")
      "Found abstract classes that contain code: $names"
    })

    val modules = (
        publicModules() ++
        internalModules.values.toList
    )

    new ModuleSet(unit.coreSpec, modules, abstractClasses)
  }
  
  private def publicModules(): List[ModuleSet.Module] = {
    for {
      id <- allModules
    } yield {
      val internalDeps = staticDependencies(id)
        .map(className => internalModules(visitedNodes(className).compId).id)

      new ModuleSet.Module(
        id = id,
        internalDeps = internalDeps,
        externalDeps = externalDependencies(id).toList,
        public = true,
        classDefs = Nil,
        topLevelExports = topLevelExports(id),
        initializers = moduleInitializers(id),
      )
    }
  }

  private def strongconnect(clazz: ClassName): Result = {
    try {
      strongconnectInternal(clazz)
    } catch {
      case e: RuntimeException =>
        throw new RuntimeException(clazz.nameString + ": " + e.getMessage, e)
    }
  }

  private def strongconnectInternal(clazz: ClassName): Result = {
    val v = newNode(clazz)

    var lowlink = v.index
    var nodes = Vector(v)
    var deps = Set.empty[Int]

    // For all dependencies
    for (dep <- staticDependencies(clazz)) {
      visitedNodes.get(dep).fold {
        // We have not visited this dependency. It is part of our spanning tree.
        strongconnect(dep) match {
          case Partial(thatLowlink, thatNodes, thatDeps) =>
            lowlink = math.min(lowlink, thatLowlink)
            nodes ++= thatNodes
            deps = deps.union(thatDeps)

          case Component(id) =>
            deps += id
        }
      } { w =>
        // We have already visited this node.
        if (w.compId == -1) {
          // This is a back link.
          lowlink = math.min(lowlink, w.index)
        } else {
          deps += w.compId
        }
      }
    }

    if (lowlink == v.index) {
      // This node is the root node of a component/module.
      val id = v.index
      nodes.foreach(_.compId = id)

      newModule(id, deps, nodes.map(_.clazz))

      Component(id)
    } else {
      Partial(lowlink, nodes, deps)
    }
  }

  private def newNode(clazz: ClassName): Node = {
    assert(!visitedNodes.contains(clazz))

    val i = nextIndex
    nextIndex += 1
    val node = new Node(clazz, i, -1)
    visitedNodes(clazz) = node
    node
  }

  private def newModule(id: Int, deps: Set[Int], classes: Seq[ClassName]): Unit = {
    val moduleName =
      if (deps.isEmpty) "root"
      else if (classes.size == 1) classes.head.nameString
      else id.toString  // TODO: This needs to be better.

    internalModules(id) = new ModuleSet.Module(
      id = new ModuleID(moduleName),
      internalDeps = deps.map(internalModules(_).id),
      externalDeps = classes.flatMap(externalDependencies(_)).distinct.toList,
      public = false,
      classDefs = classes.map(linkedClassesByName(_)).toList,
      topLevelExports = Nil,
      initializers = Nil,
    )
  }
}
