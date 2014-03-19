package scala.scalajs.compiler.test.util

import scala.tools.nsc._
import scala.scalajs.compiler.{ScalaJSPlugin, JSTrees, JSTreeExtractors}

import scala.util.control.ControlThrowable

import org.junit.Assert._

import language.implicitConversions

abstract class JSASTTest extends DirectTest {

  private var lastAST: JSAST = _

  abstract class JSAST {
    val trees: JSTrees with JSTreeExtractors
    val clDefs: List[trees.js.Tree]

    type Pat = (trees.js.type, trees.jse.type) =>
      PartialFunction[trees.js.Tree, Unit]

    class PFTraverser(pat: Pat) extends trees.js.Transformer {
      private case object Found extends ControlThrowable

      val pf = pat(trees.js, trees.jse)

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

      private def visit(tr: trees.js.Tree)(after: =>trees.js.Tree) = {
        if (finding && pf.isDefinedAt(tr))
          throw Found

        if (!finding)
          pf.lift(tr)

        after
      }

      override def transformStat(tr: trees.js.Tree) = visit(tr) {
        super.transformStat(tr)
      }

      override def transformExpr(tr: trees.js.Tree) = visit(tr) {
        super.transformExpr(tr)
      }

      override def transformDef(tr: trees.js.Tree) = visit(tr) {
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
    override def generatedJSAST(cld: List[jsAddons.js.Tree]): Unit = {
      lastAST = new JSAST { val trees = jsAddons; val clDefs = cld }
    }
  }

  def stringAST(code: String): JSAST = stringAST(defaultGlobal)(code)

  def stringAST(global: Global)(code: String): JSAST = {
    compileString(global)(code)
    lastAST
  }

}
