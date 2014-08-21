package scala.scalajs.tools.optimizer

import scala.scalajs.ir
import ir.Position.NoPosition

import com.google.javascript.rhino._
import com.google.javascript.rhino.jstype.{StaticSourceFile, SimpleSourceFile}
import com.google.javascript.jscomp._

import scala.collection.mutable

import java.net.URI

class ClosureAstBuilder(
    relativizeBaseURI: Option[URI] = None) extends JSTreeBuilder {

  private val transformer = new ClosureAstTransformer(relativizeBaseURI)
  private val treeBuf = mutable.ListBuffer.empty[Node]

  def addJSTree(tree: ir.Trees.Tree): Unit =
    treeBuf += transformer.transformStat(tree)(NoPosition)

  lazy val closureAST: SourceAst = {
    val root = transformer.setNodePosition(IR.script(treeBuf: _*), NoPosition)

    treeBuf.clear()

    new ClosureAstBuilder.ScalaJSSourceAst(root)
  }

}

object ClosureAstBuilder {
  // Dummy Source AST class

  class ScalaJSSourceAst(root: Node) extends SourceAst {
    def getAstRoot(compiler: AbstractCompiler): Node = root
    def clearAst(): Unit = () // Just for GC. Nonsensical here.
    def getInputId(): InputId = root.getInputId()
    def getSourceFile(): SourceFile =
      root.getStaticSourceFile().asInstanceOf[SourceFile]
    def setSourceFile(file: SourceFile): Unit =
      if (getSourceFile() ne file) throw new IllegalStateException
  }
}
