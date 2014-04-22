package scala.scalajs.compiler.test.util

import language.implicitConversions

import scala.tools.nsc._

import scala.util.control.ControlThrowable

import org.junit.Assert._

import scala.scalajs.compiler.{ScalaJSPlugin, JSTreeExtractors}
import JSTreeExtractors.jse
import scala.scalajs.ir
import ir.{Trees => js}

abstract class JSASTTest extends DirectTest {

  private var lastAST: JSAST = _

  abstract class JSAST {
    val clDefs: List[js.Tree]

    type Pat = PartialFunction[js.Tree, Unit]

    class PFTraverser(pf: Pat) extends ir.Transformers.Transformer {
      private case object Found extends ControlThrowable

      private[this] var finding = false

      def find: Boolean = {
        finding = true
        try {
          clDefs map transformStat _
          false
        } catch {
          case Found => true
        }
      }

      def traverse: Unit = {
        finding = false
        clDefs map transformStat _
      }

      private def visit(tr: js.Tree)(after: => js.Tree) = {
        if (finding && pf.isDefinedAt(tr))
          throw Found

        if (!finding)
          pf.lift(tr)

        after
      }

      override def transformStat(tr: js.Tree) = visit(tr) {
        super.transformStat(tr)
      }

      override def transformExpr(tr: js.Tree) = visit(tr) {
        super.transformExpr(tr)
      }

      override def transformDef(tr: js.Tree) = visit(tr) {
        super.transformDef(tr)
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
      tr.traverse
      this
    }

    def show: this.type = {
      clDefs foreach println _
      this
    }

  }

  implicit def string2ast(str: String): JSAST = stringAST(str)

  override def newScalaJSPlugin(global: Global) = new ScalaJSPlugin(global) {
    override def generatedJSAST(cld: List[js.Tree]): Unit = {
      lastAST = new JSAST { val trees = jsAddons; val clDefs = cld }
    }
  }

  def stringAST(code: String): JSAST = stringAST(defaultGlobal)(code)

  def stringAST(global: Global)(code: String): JSAST = {
    compileString(global)(code)
    lastAST
  }

}
