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

package org.scalajs.core.tools.linker.backend.closure

import org.scalajs.core.ir
import ir.Position.NoPosition

import org.scalajs.core.tools.javascript.{Trees => js}
import org.scalajs.core.tools.javascript.Trees.Tree
import org.scalajs.core.tools.javascript.JSTreeBuilder

import com.google.javascript.rhino._
import com.google.javascript.jscomp._

import scala.collection.mutable

import java.net.URI

private[closure] class ClosureAstBuilder(
    relativizeBaseURI: Option[URI] = None) extends JSTreeBuilder {

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

  lazy val closureAST: SourceAst = {
    val root =
      transformer.setNodePosition(IR.script(treeBuf.result(): _*), NoPosition)
    treeBuf.clear()
    new ClosureAstBuilder.ScalaJSSourceAst(root)
  }

}

private object ClosureAstBuilder {
  // Dummy Source AST class

  private class ScalaJSSourceAst(root: Node) extends SourceAst {
    def getAstRoot(compiler: AbstractCompiler): Node = root
    def clearAst(): Unit = () // Just for GC. Nonsensical here.
    def getInputId(): InputId = root.getInputId()
    def getSourceFile(): SourceFile =
      root.getStaticSourceFile().asInstanceOf[SourceFile]
    def setSourceFile(file: SourceFile): Unit =
      if (getSourceFile() ne file) throw new IllegalStateException
  }
}
