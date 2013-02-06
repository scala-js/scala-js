/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package js

import scala.collection.mutable.ListBuffer

import scalajs.JSGlobal

/** Generate JavaScript code and output it to disk
 *
 *  @author Sébastien Doeraene
 */
abstract class GenJSCode extends SubComponent {
  val global: JSGlobal

  import global._

  val phaseName = "jscode"

  override def newPhase(p: Phase) = new JSCodePhase(p)

  class JSCodePhase(prev: Phase) extends StdPhase(prev) {

    import scala.tools.jsm.{ Names => JSNames }
    import scala.tools.jsm.ast.{ Trees => js }

    override def name = phaseName
    override def description = "Generate JavaScript code from ASTs"
    override def erasedTypes = true

    // Accumulator for the generated classes -----------------------------------

    val generatedClasses = new ListBuffer[js.ClassDef]

    // Some bridges with JS code -----------------------------------------------

    implicit class PositionJSHelper(pos: Position) {
      def toJSPos: scala.util.parsing.input.Position = {
        new scala.util.parsing.input.Position {
          override def line = pos.line
          override def column = pos.column
          override protected def lineContents = pos.lineContent
        }
      }
    }

    def identName(name: TermName) =
      JSNames.identName(name.toString)

    def propName(name: TermName) =
      JSNames.propName(name.toString)

    // Top-level apply ---------------------------------------------------------

    override def apply(cunit: CompilationUnit) {
      def gen(tree: Tree) {
        tree match {
          case EmptyTree            => ()
          case PackageDef(_, stats) => stats foreach gen
          case cd: ClassDef         => genClass(cd)
        }
      }

      gen(cunit.body)
    }

    // Generate a class --------------------------------------------------------

    def genClass(cd: ClassDef): js.ClassDef = {
      implicit val jspos = cd.pos.toJSPos
      val ClassDef(mods, name, _, impl) = cd

      println(cd)

      val generatedMethods = new ListBuffer[js.MethodDef]

      def gen(tree: Tree) {
        tree match {
          case EmptyTree => ()
          case _: ModuleDef =>
            abort("Modules should have been eliminated by refchecks: " + tree)
          case ValDef(mods, name, tpt, rhs) =>
            () // fields are added in constructors
          case dd: DefDef => generatedMethods += genMethod(dd)
          case Template(_, _, body) => body foreach gen
          case _ => abort("Illegal tree in gen: " + tree)
        }
      }

      gen(impl)

      val result = js.ClassDef(identName(name), Nil, generatedMethods.toList)
      new scala.tools.jsm.ast.Printers.TreePrinter(
          new java.io.PrintWriter(Console.out, true)).printTree(result)
      result
    }

    // Generate a method -------------------------------------------------------

    def genMethod(dd: DefDef): js.MethodDef = {
      implicit val jspos = dd.pos.toJSPos
      val DefDef(mods, name, _, vparamss, _, body) = dd

      assert(vparamss.isEmpty || vparamss.tail.isEmpty,
          "Malformed parameter list: " + vparamss)
      val params = if(vparamss.isEmpty) Nil else vparamss.head

      val jsParams =
        for (param <- params)
          yield identName(param.name)

      js.MethodDef(propName(name), jsParams, js.Block(Nil))
    }
  }
}
