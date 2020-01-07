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

package org.scalajs.linker.backend.closure

import org.scalajs.ir
import ir.Position.NoPosition

import org.scalajs.linker.backend.javascript.{Trees => js}
import org.scalajs.linker.backend.javascript.Trees.Tree
import org.scalajs.linker.backend.javascript.JSBuilder

import com.google.javascript.rhino._
import com.google.javascript.jscomp._

import scala.collection.mutable

import java.net.URI

private[closure] class ClosureModuleBuilder(
    relativizeBaseURI: Option[URI] = None) extends JSBuilder {

  private val transformer = new ClosureAstTransformer(relativizeBaseURI)
  private val treeBuf = List.newBuilder[Node]

  def addJSTree(tree: Tree): Unit = {
    /* Top-level `js.Block`s must be explicitly flattened here.
     * Our `js.Block`s do not have the same semantics as GCC's `BLOCK`s: GCC's
     * impose strict scoping for `let`s, `const`s and `class`es, while ours are
     * only a means of putting together several statements in one `js.Tree`
     * (in fact, they automatically flatten themselves out upon construction).
     */
    tree match {
      case js.Block(stats) =>
        treeBuf ++= transformer.transformBlockStats(stats)(NoPosition)
      case js.Skip() =>
        // ignore
      case _ =>
        treeBuf += transformer.transformStat(tree)(NoPosition)
    }
  }

  def complete(): Unit = ()

  def result(): JSModule = {
    val module = new JSModule("Scala.js")

    val trees = treeBuf.result()
    if (trees.nonEmpty) {
      val root = transformer.setNodePosition(IR.script(trees: _*), NoPosition)
      module.add(new CompilerInput(new SyntheticAst(root)))
    }

    module
  }
}
