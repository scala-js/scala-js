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

import scala.annotation.tailrec
import scala.collection.mutable

import org.scalajs.ir.Names.{ClassName, ObjectClass}
import org.scalajs.linker.standard.ModuleSet.ModuleID

/** Build smallest possible modules.
 *
 *  Generates a module per class, except when there are circular dependencies.
 *
 *  In practice, this means it generates a module per strongly connected
 *  component of the (static) dependency graph.
 */
private[modulesplitter] final class MinModuleAnalyzer extends ModuleAnalyzer {
  def analyze(info: ModuleAnalyzer.DependencyInfo): ModuleAnalyzer.Analysis = {
    val run = new MinModuleAnalyzer.Run(info)
    run.analyze()
    run
  }
}

private object MinModuleAnalyzer {
  private final class Node(val className: ClassName, val index: Int) {
    var lowlink: Int = index
    var moduleIndex: Int = -1
  }

  private class Run(info: ModuleAnalyzer.DependencyInfo)
      extends ModuleAnalyzer.Analysis {

    private[this] var nextIndex = 0
    private[this] val nodes = mutable.Map.empty[ClassName, Node]
    private[this] val stack = mutable.ArrayBuffer.empty[Node]
    private[this] val moduleIndexToID = mutable.Map.empty[Int, ModuleID]
    private[this] val toConnect = mutable.Queue.empty[ClassName]

    def moduleForClass(className: ClassName): Option[ModuleID] =
      nodes.get(className).map(n => moduleIndexToID(n.moduleIndex))

    def analyze(): Unit = {
      info.publicModuleDependencies
        .valuesIterator
        .flatten
        .filter(!nodes.contains(_))
        .foreach(strongconnect(_))

      assert(stack.isEmpty)

      while (toConnect.nonEmpty) {
        val clazz = toConnect.dequeue()
        if (!nodes.contains(clazz))
          strongconnect(clazz)
      }
    }

    private def strongconnect(className: ClassName): Node = {
      /* Tarjan's algorithm for strongly connected components.
       *
       * The intuition is as follows: We determine a single spanning tree using
       * a DFS (recursive calls to `strongconnect`).
       *
       * Whenever we find a back-edge (i.e. an edge to a node already visited),
       * we know that the current sub-branch (up to that node) is strongly
       * connected. This is because it can be "cycled through" through the cycle
       * we just discovered.
       *
       * A strongly connected component is identified by the lowest index node
       * that is part of it. This makes it easy to propagate and merge
       * components.
       *
       * More:
       * https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
       */
      assert(!nodes.contains(className))

      val node = new Node(className, nextIndex)
      nextIndex += 1

      nodes(className) = node
      stack += node

      val classInfo = info.classDependencies(className)

      /* Dynamic dependencies do not affect the import graph: It is OK to have
       * cyclic, dynamic dependencies (because we never generate top-level
       * awaits).
       *
       * However, we need to make sure the dynamic dependency is actually put
       * into a module. For this, we schedule it to be connected later (we
       * cannot connect it immediately, otherwise we'd mess up the
       * stack/spanning tree state).
       */
      classInfo.dynamicDependencies
        // avoid enqueuing things we've already reached anyways.
        .filter(!nodes.contains(_))
        .foreach(toConnect.enqueue(_))

      for (depName <- classInfo.staticDependencies) {
        nodes.get(depName).fold {
          // We have not visited this dependency. It is part of our spanning tree.
          val depNode = strongconnect(depName)
          node.lowlink = math.min(node.lowlink, depNode.lowlink)
        } { depNode =>
          // We have already visited this node.
          if (depNode.moduleIndex == -1) {
            // This is a back link.
            node.lowlink = math.min(node.lowlink, depNode.index)
          }
        }
      }

      if (node.lowlink == node.index) {
        // This node is the root node of a component/module.
        val moduleIndex = node.index

        var name = node.className

        @tailrec
        def pop(): Unit = {
          val n = stack.remove(stack.size - 1)
          n.moduleIndex = moduleIndex

          /* Take the lexicographically smallest name as a stable name of the
           * module, with the exception of j.l.Object which identifies the root
           * module.
           *
           * We do this, because it is simple and stable (i.e. does not depend
           * on traversal order).
           */
          if (name != ObjectClass) {
            if (n.className == ObjectClass)
              name = ObjectClass
            else if (n.className.compareTo(name) < 0)
              name = n.className
          }

          if (n ne node)
            pop()
        }

        pop()

        /* Build a module ID that doesn't collide with others.
         *
         * We observe:
         * - Class names are unique, so they never collide with each other.
         * - Appending a dot ('.') to a class name results in an illegal class name.
         *
         * So we append dots until we hit a ModuleID not used by a public module.
         *
         * Note that this is stable, because it does not depend on the order we
         * iterate over nodes.
         */
        var moduleID = ModuleID(name.nameString)
        while (info.publicModuleDependencies.contains(moduleID))
          moduleID = ModuleID(moduleID.id + ".")

        moduleIndexToID(moduleIndex) = moduleID
      }

      node
    }
  }
}
