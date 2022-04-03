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

import org.scalajs.ir.Names.ClassName
import org.scalajs.linker.standard.ModuleSet.ModuleID

private object StrongConnect {
  private final class Node(val className: ClassName, val index: Int) {
    var lowlink: Int = index
    var moduleIndex: Int = -1
  }
}

/** Analyzer to find strongly connected components. */
private abstract class StrongConnect(info: ModuleAnalyzer.DependencyInfo) {
  import StrongConnect.Node

  private[this] var nextIndex = 0
  private[this] val nodes = mutable.Map.empty[ClassName, Node]
  private[this] val stack = mutable.ArrayBuffer.empty[Node]
  private[this] val toConnect = mutable.Queue.empty[ClassName]

  final def analyze(): Unit = {
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

  protected final def moduleIndex(className: ClassName): Option[Int] =
    nodes.get(className).map(_.moduleIndex)

  /** Extension point; called once for each strongly connected component (during analyze). */
  protected def emitModule(moduleIndex: Int, classNames: List[ClassName]): Unit

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

      val classNames = List.newBuilder[ClassName]

      @tailrec
      def pop(): Unit = {
        val n = stack.remove(stack.size - 1)
        n.moduleIndex = moduleIndex

        classNames += n.className

        if (n ne node)
          pop()
      }

      pop()

      emitModule(moduleIndex, classNames.result())
    }

    node
  }
}
