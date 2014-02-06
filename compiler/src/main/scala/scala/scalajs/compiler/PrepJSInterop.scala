/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author Tobias Schlatter
 */

package scala.scalajs.compiler

import scala.tools.nsc
import nsc._

import scala.collection.immutable.ListMap

/** Prepares classes extending js.Any for JavaScript interop
 *
 * This phase does two things:
 * - Annotate subclasses of js.Any to be treated specially
 * - Handle extension methods to subclasses of js.Any
 *
 * @author Tobias Schlatter
 */
abstract class PrepJSInterop extends plugins.PluginComponent with transform.Transform {
  val jsAddons: JSGlobalAddons {
    val global: PrepJSInterop.this.global.type
  }

  import global._
  import jsAddons._
  import definitions._
  import rootMirror._
  import jsDefinitions._

  val phaseName = "jsinterop"

  override def newPhase(p: nsc.Phase) = new JSInteropPhase(p)
  class JSInteropPhase(prev: nsc.Phase) extends Phase(prev) {
    override def name = phaseName
    override def description = "Prepare ASTs for JavaScript interop"
  }

  override protected def newTransformer(unit: CompilationUnit) =
    new JSInteropTransformer(unit)

  class JSInteropTransformer(unit: CompilationUnit) extends Transformer {

    var inJSAnyMod = false
    var inJSAnyCls = false
    var inScalaCls = false
    /** are we inside a subclass of scala.Enumeration */
    var inScalaEnum = false
    /** are we inside the implementation of scala.Enumeration? */
    var inEnumImpl = false

    def jsAnyClassOnly = !inJSAnyCls && allowJSAny
    def allowImplDef   = !inJSAnyCls && !inJSAnyMod
    def allowJSAny     = !inScalaCls
    def inJSAny        = inJSAnyMod || inJSAnyCls

    override def transform(tree: Tree): Tree = tree match {
      // Catch special case of ClassDef in ModuleDef
      case cldef: ClassDef if jsAnyClassOnly && isJSAny(cldef) =>
        transformJSAny(cldef)

      // Catch forbidden implDefs
      case idef: ImplDef if !allowImplDef =>
        unit.error(idef.pos, "Traits, classes and objects extending js.Any " +
            "may not have inner traits, classes or objects")
        super.transform(tree)

      // Handle js.Anys
      case idef: ImplDef if isJSAny(idef) =>
        transformJSAny(idef)

      // Catch the definition of scala.Enumeration itself
      case cldef: ClassDef if cldef.symbol == ScalaEnumClass =>
        enterEnumImpl { super.transform(cldef) }

      // Catch Scala Enumerations to transform calls to scala.Enumeration.Value
      case cldef: ClassDef if isScalaEnum(cldef) =>
        enterScalaCls {
          enterScalaEnum {
            super.transform(cldef)
          }
        }
      case idef: ImplDef if isScalaEnum(idef) =>
        enterScalaEnum { super.transform(idef) }

      // Catch (Scala) ClassDefs to forbid js.Anys
      case cldef: ClassDef =>
        enterScalaCls { super.transform(cldef) }

      // Catch ValDefs in enumerations with simple calls to Value
      case ValDef(mods, name, tpt, ScalaEnumValNoName(optPar)) if inScalaEnum =>
        val nrhs = ScalaEnumValName(tree.symbol.owner, tree.symbol, optPar)
        treeCopy.ValDef(tree, mods, name, transform(tpt), nrhs)

      // Catch Select on Enumeration.Value we couldn't transform but need to
      // we ignore the implementation of scala.Enumeration itself
      case ScalaEnumValNoName(_) if !inEnumImpl =>
        unit.warning(tree.pos,
                     """Couldn't transform call to Enumeration.Value.
                       |The resulting program is unlikely to function properly as this
                       |operation requires reflection.""".stripMargin)
        super.transform(tree)

      case _ => super.transform(tree)
    }

    /**
     * Performs checks and rewrites specific to classes / objects extending
     * js.Any
     */
    private def transformJSAny(implDef: ImplDef) = {
      implDef match {
        // Check that we are not an anonymous class
        case cldef: ClassDef if cldef.symbol.isAnonymousClass =>
          unit.error(implDef.pos, "Anonymous classes may not " +
              "extend js.Any")

        // Check that we do not have a case modifier
        case implDef if implDef.mods.hasFlag(Flag.CASE) =>
          unit.error(implDef.pos, "Classes and objects extending " +
              "js.Any may not have a case modifier")

        // Check if we may have a js.Any here
        case _: ClassDef if !allowJSAny && !jsAnyClassOnly =>
          unit.error(implDef.pos, "Classes extending js.Any may not be " +
              "defined inside a class or trait")

        case _: ModuleDef if !allowJSAny =>
          unit.error(implDef.pos, "Objects extending js.Any may not be " +
              "defined inside a class or trait")

        // Check that this is not a class extending js.GlobalScope
        case _: ClassDef if isJSGlobalScope(implDef) &&
          implDef.symbol != JSGlobalScopeClass =>
          unit.error(implDef.pos, "Only objects may extend js.GlobalScope")

        // Check that primary ctor of a ClassDef is no-arg
        // FIXME temporarily disabled until we have better handling.
        //case cldef: ClassDef if !primCtorNoArg(cldef) =>
        //  unit.error(cldef.pos, "The primary constructor of a class extending "+
        //      "js.Any may only have a single, empty argument list")

        case _ =>
          // We cannot use implDef.symbol directly, since the symbol
          // of a module is not its type's symbol but the value it declares
          val sym = implDef.symbol.tpe.typeSymbol

          sym.setAnnotations(rawJSAnnot :: sym.annotations)

      }

      if (implDef.isInstanceOf[ModuleDef])
        enterJSAnyMod { super.transform(implDef) }
      else
        enterJSAnyCls { super.transform(implDef) }
    }

    private def enterJSAnyCls[T](body: =>T) = {
      val old = inJSAnyCls
      inJSAnyCls = true
      val res = body
      inJSAnyCls = old
      res
    }

    private def enterJSAnyMod[T](body: =>T) = {
      val old = inJSAnyMod
      inJSAnyMod = true
      val res = body
      inJSAnyMod = old
      res
    }

    private def enterScalaCls[T](body: =>T) = {
      val old = inScalaCls
      inScalaCls = true
      val res = body
      inScalaCls = old
      res
    }

    private def enterScalaEnum[T](body: =>T) = {
      val old = inScalaEnum
      inScalaEnum = true
      val res = body
      inScalaEnum = old
      res
    }

    private def enterEnumImpl[T](body: =>T) = {
      val old = inEnumImpl
      inEnumImpl = true
      val res = body
      inEnumImpl = old
      res
    }

  }

  private def isJSAny(implDef: ImplDef) = isScalaJSDefined &&
    (implDef.symbol.tpe.typeSymbol isSubClass JSAnyClass)

  private def isJSGlobalScope(implDef: ImplDef) = isScalaJSDefined &&
    (implDef.symbol.tpe.typeSymbol isSubClass JSGlobalScopeClass)

  private def isScalaEnum(implDef: ImplDef) =
    implDef.symbol.tpe.typeSymbol isSubClass ScalaEnumClass

  /**
   * Extractor object for calls to scala.Enumeration.Value that do not have an
   * explicit name in the parameters
   *
   * Extracts:
   * - `sel: Select` where sel.symbol is Enumeration.Value (no param)
   * - Apply(meth, List(param)) where meth.symbol is Enumeration.Value(i: Int)
   */
  private object ScalaEnumValNoName {
    private val valueSym =
      getMemberMethod(ScalaEnumClass, newTermName("Value"))
    private val valueNoPar  = valueSym suchThat { _.paramss == List() }
    private val valueIntPar = valueSym suchThat {
      _.tpe.params.map(_.tpe.typeSymbol) == List(IntClass)
    }

    def unapply(t: Tree) = t match {
      case sel: Select if sel.symbol == valueNoPar =>
        Some(None)
      case Apply(meth, List(param)) if meth.symbol == valueIntPar =>
        Some(Some(param))
      case _ =>
        None
    }
  }

  /**
   * Construct a call to Enumeration.Value
   * @param thisSym  ClassSymbol of enclosing class
   * @param nameOrig Symbol of ValDef where this call will be placed
   * 			       (determines the string passed to Value)
   * @param intParam Optional tree with Int passed to Value
   * @return Typed tree with appropriate call to Value
   */
  private def ScalaEnumValName(
      thisSym: Symbol,
      nameOrig: Symbol,
      intParam: Option[Tree]) = {
    val name = nameOrig.asTerm.getterName.encoded
    val params = intParam.toList :+ Literal(Constant(name))
    typer.typed {
      Apply(Select(This(thisSym),newTermName("Value")), params)
    }
  }

  private def rawJSAnnot =
    Annotation(RawJSTypeAnnot.tpe, List.empty, ListMap.empty)

  private val ScalaEnumClass = getRequiredClass("scala.Enumeration")

  /** checks if the primary constructor of the ClassDef `cldef` does not
   *  take any arguments
   */
  private def primCtorNoArg(cldef: ClassDef) =
    getPrimCtor(cldef.symbol.tpe).map(_.paramss == List(List())).getOrElse(true)

  /** return the MethodSymbol of the primary constructor of the given type
   *  if it exists
   */
  private def getPrimCtor(tpe: Type) =
    tpe.declaration(nme.CONSTRUCTOR).alternatives.collectFirst {
      case ctor: MethodSymbol if ctor.isPrimaryConstructor => ctor
    }

}
