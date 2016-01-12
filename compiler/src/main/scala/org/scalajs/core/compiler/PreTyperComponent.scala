/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author Nicolas Stucki
 */

package org.scalajs.core.compiler

import scala.tools.nsc
import nsc._

/** This `jspretyper` phase prepares a fix for issue SI-9487 in the case of
 *  anonymous classes that extend js.Any.
 *
 *  During `typer`, due to a bug (SI-9487), Scalac transfroms some public method
 *  definitions of a quite specific syntactic form of anonymous classes into
 *  private methods. This affects both methods and field accessors. This phase
 *  identifies any anonymous class and adds a `@WasPublicBeforeTyper` annotation
 *  on its public methods and fields. After the `typer` in `jsinterop` the
 *  anonymous classes are fixed if they extend js.Any using the annotations as
 *  reference.
 *
 *  As an example:
 *  {{{
 *  class $anon extends ... {
 *    val foo = ???
 *    var bar = ???
 *    def baz = ???
 *    private val foo2 = ???
 *    private var bar2 = ???
 *    private def baz2 = ???
 *  }
 *  }}}
 *
 *  Would become:
 *  {{{
 *  class $anon extends ... {
 *    @WasPublicBeforeTyper val foo = ???
 *    @WasPublicBeforeTyper var bar = ???
 *    @WasPublicBeforeTyper def baz = ???
 *    private val foo2 = ???
 *    private var bar2 = ???
 *    private def baz2 = ???
 *  }
 *  }}}
 *
 *  And after `typer` (if has SI-9487) will be:
 *  {{{
 *  class $anon extends ... {
 *    @WasPublicBeforeTyper private[this] var foo = ???
 *    private <stable> <accessor> def foo = ??? // Needs fix
 *
 *    @WasPublicBeforeTyper private[this] var bar = ???
 *    private <accessor> def bar = ??? // Needs fix
 *    private <accessor> def bar_=(...) = ??? // Needs fix
 *
 *    @WasPublicBeforeTyper private def baz = ??? // Needs fix
 *    ...
 *  }
 *  }}}
 *
 *  @author Nicolas Stucki
 */
abstract class PreTyperComponent extends plugins.PluginComponent
    with transform.Transform with PluginComponent210Compat {

  import global._

  val phaseName: String = "jspretyper"
  override def description: String =
    "capture pre-typer only tree info (for Scala.js)"

  override protected def newTransformer(unit: CompilationUnit): Transformer =
    new PreTyperTransformer(unit)

  class PreTyperTransformer(unit: CompilationUnit) extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case tree: ClassDef if needsAnnotations(tree) =>
        val newBody = tree.impl.body.map {
          case vdef: ValDef if needsAnnotations(vdef) =>
            treeCopy.ValDef(vdef, withWasPublic(vdef.mods), vdef.name,
                vdef.tpt, transform(vdef.rhs))

          case ddef: DefDef if needsAnnotations(ddef) =>
            treeCopy.DefDef(ddef, withWasPublic(ddef.mods), ddef.name,
                ddef.tparams, ddef.vparamss, ddef.tpt, transform(ddef.rhs))

          case member => transform(member)
        }
        val newImpl =
          treeCopy.Template(tree.impl, tree.impl.parents, tree.impl.self, newBody)
        treeCopy.ClassDef(tree, tree.mods, tree.name, tree.tparams, newImpl)

      case tree: Template =>
        /* Avoid filtering out members that are EmptyTree during this transform.
         *
         * run/macro-term-declared-in-trait is an example of code where they
         * should not be cleaned.
         */
        val newBody = tree.body.map(transform)
        treeCopy.Template(tree, tree.parents, tree.self, newBody)

      case _ => super.transform(tree)
    }
  }

  private def needsAnnotations(classDef: ClassDef): Boolean = {
    classDef.name == nme.ANON_CLASS_NAME.toTypeName &&
    classDef.impl.body.exists {
      case vdef: ValDef => needsAnnotations(vdef)
      case ddef: DefDef => needsAnnotations(ddef)
      case _ => false
    }
  }

  private def needsAnnotations(vdef: ValDef): Boolean =
    vdef.mods.isPublic

  private def needsAnnotations(ddef: DefDef): Boolean =
    ddef.mods.isPublic && ddef.name != nme.CONSTRUCTOR

  private def withWasPublic(mods: Modifiers): Modifiers =
    mods.withAnnotations(List(anonymousClassMethodWasPublicAnnotation))

  private val scalajs = newTermName("scalajs")
  private val js = newTermName("js")
  private val wasPublicBeforeTyper = newTypeName("WasPublicBeforeTyper")

  private def anonymousClassMethodWasPublicAnnotation: Tree = {
    val runtimePackage = Select(Select(Select(Select(Ident(nme.ROOTPKG),
        nme.scala_), scalajs), js), nme.annotation)
    val cls = Select(runtimePackage, wasPublicBeforeTyper)
    Apply(Select(New(cls), nme.CONSTRUCTOR), Nil)
  }
}
