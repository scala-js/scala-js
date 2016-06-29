package org.scalajs.core.tools.linker.backend.closure

import org.scalajs.core.ir
import ir.Position.NoPosition

import org.scalajs.core.tools.javascript.Trees.Tree
import org.scalajs.core.tools.javascript.JSTreeBuilder

import com.google.javascript.rhino._
import com.google.javascript.jscomp._

import scala.collection.mutable

import java.net.URI

private[closure] class ClosureAstBuilder(
    relativizeBaseURI: Option[URI] = None) extends JSTreeBuilder {

  private val transformer = new ClosureAstTransformer(relativizeBaseURI)
  private val treeBuf = mutable.ListBuffer.empty[Node]

  def addJSTree(tree: Tree): Unit =
    treeBuf += transformer.transformStat(tree)(NoPosition)

  lazy val closureAST: SourceAst = {
    val root = transformer.setNodePosition(IR.script(treeBuf: _*), NoPosition)

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
