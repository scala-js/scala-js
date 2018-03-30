package org.scalajs.linker.backend.closure

import org.scalajs.ir
import ir.Position.NoPosition

import org.scalajs.linker.backend.javascript.Trees.Tree
import org.scalajs.linker.backend.javascript.JSBuilder

import com.google.javascript.rhino._
import com.google.javascript.jscomp._

import scala.collection.mutable

import java.net.URI

private[closure] class ClosureModuleBuilder(
    relativizeBaseURI: Option[URI] = None) extends JSBuilder {

  private val transformer = new ClosureAstTransformer(relativizeBaseURI)
  private val treeBuf = mutable.ListBuffer.empty[Node]
  private val module = new JSModule("Scala.js")

  def addJSTree(tree: Tree): Unit =
    treeBuf += transformer.transformStat(tree)(NoPosition)

  def addStatement(originalLocation: URI, code: String): Unit = {
    flushTrees()
    val path = URIUtil.sourceURIToString(relativizeBaseURI, originalLocation)
    module.add(new CompilerInput(SourceFile.fromCode(path, code)))
  }

  def complete(): Unit = flushTrees()

  def result(): JSModule = {
    complete()
    module
  }

  private def flushTrees(): Unit = {
    if (treeBuf.nonEmpty) {
      val root = transformer.setNodePosition(IR.script(treeBuf: _*), NoPosition)
      treeBuf.clear()

      val ast = new ClosureModuleBuilder.ScalaJSSourceAst(root)
      module.add(new CompilerInput(ast, ast.getInputId(), false))
    }
  }
}

private object ClosureModuleBuilder {
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
