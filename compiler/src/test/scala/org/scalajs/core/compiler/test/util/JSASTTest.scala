package org.scalajs.core.compiler.test.util

import language.implicitConversions

import scala.tools.nsc._
import scala.reflect.internal.util.SourceFile

import scala.util.control.ControlThrowable

import org.junit.Assert._

import org.scalajs.core.compiler.{ScalaJSPlugin, JSTreeExtractors}
import JSTreeExtractors.jse
import org.scalajs.core.ir
import ir.{Trees => js}

abstract class JSASTTest extends DirectTest {

  private var lastAST: JSAST = _

  class JSAST(val clDefs: List[js.Tree]) {
    type Pat = PartialFunction[js.Tree, Unit]

    class PFTraverser(pf: Pat) extends ir.Traversers.Traverser {
      private case object Found extends ControlThrowable

      private[this] var finding = false

      def find: Boolean = {
        finding = true
        try {
          clDefs.map(traverse)
          false
        } catch {
          case Found => true
        }
      }

      def traverse(): Unit = {
        finding = false
        clDefs.map(traverse)
      }

      override def traverse(tree: js.Tree): Unit = {
        if (finding && pf.isDefinedAt(tree))
          throw Found

        if (!finding)
          pf.lift(tree)

        super.traverse(tree)
      }
    }

    def has(trgName: String)(pf: Pat): this.type = {
      val tr = new PFTraverser(pf)
      assertTrue(s"AST should have $trgName", tr.find)
      this
    }

    def hasNot(trgName: String)(pf: Pat): this.type = {
      val tr = new PFTraverser(pf)
      assertFalse(s"AST should not have $trgName", tr.find)
      this
    }

    def traverse(pf: Pat): this.type = {
      val tr = new PFTraverser(pf)
      tr.traverse()
      this
    }

    def show: this.type = {
      clDefs foreach println _
      this
    }

  }

  implicit def string2ast(str: String): JSAST = stringAST(str)

  override def newScalaJSPlugin(global: Global): ScalaJSPlugin = {
    new ScalaJSPlugin(global) {
      override def generatedJSAST(cld: List[js.Tree]): Unit = {
        lastAST = new JSAST(cld)
      }
    }
  }

  def stringAST(code: String): JSAST = stringAST(defaultGlobal)(code)
  def stringAST(global: Global)(code: String): JSAST = {
    compileString(global)(code)
    lastAST
  }

  def sourceAST(source: SourceFile): JSAST = sourceAST(defaultGlobal)(source)
  def sourceAST(global: Global)(source: SourceFile): JSAST = {
    compileSources(global)(source)
    lastAST
  }

}
