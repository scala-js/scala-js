/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author Tobias Schlatter
 */

package scala.scalajs.compiler

import scala.tools.nsc
import nsc._

import scala.collection.immutable.ListMap
import scala.collection.mutable

/** Prepares classes extending js.Any for JavaScript interop
 *
 * This phase does:
 * - Sanity checks for js.Any hierarchy
 * - Annotate subclasses of js.Any to be treated specially
 * - Rewrite calls to scala.Enumeration.Value (include name string)
 * - Create JSExport methods: Dummy methods that are propagated
 *   through the whole compiler chain to mark exports. This allows
 *   exports to have the same semantics than methods.
 *
 * @author Tobias Schlatter
 */
abstract class PrepJSInterop extends plugins.PluginComponent
                                with PrepJSExports
                                with transform.Transform
                                with Compat210Component {
  val jsAddons: JSGlobalAddons {
    val global: PrepJSInterop.this.global.type
  }

  val scalaJSOpts: ScalaJSOptions

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

  private object jsnme {
    val hasNext  = newTermName("hasNext")
    val next     = newTermName("next")
    val nextName = newTermName("nextName")
    val x        = newTermName("x")
    val Value    = newTermName("Value")
    val Val      = newTermName("Val")
  }

  private object jstpnme {
    val scala_ = newTypeName("scala") // not defined in 2.10's tpnme
  }

  class JSInteropTransformer(unit: CompilationUnit) extends Transformer {

    // Force evaluation of JSDynamicLiteral: Strangely, we are unable to find
    // nested objects in the JSCode phase (probably after flatten).
    // Therefore we force the symbol of js.Dynamic.literal here in order to
    // have access to it in JSCode.
    JSDynamicLiteral

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

    /** DefDefs in class templates that export methods to JavaScript */
    val exporters = mutable.Map.empty[Symbol, mutable.ListBuffer[Tree]]

    override def transform(tree: Tree): Tree = postTransform { tree match {
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

      // Catch DefDefs in JSAny to forbid setters with non-unit return type
      case ddef: DefDef if inJSAny && isNonJSScalaSetter(ddef.symbol) =>
        unit.error(tree.pos, "Setters that do not return Unit are " +
            "not allowed in types extending js.Any")
        super.transform(ddef)

      // Catch ValDefs in enumerations with simple calls to Value
      case ValDef(mods, name, tpt, ScalaEnumValue.NoName(optPar)) if inScalaEnum =>
        val nrhs = ScalaEnumValName(tree.symbol.owner, tree.symbol, optPar)
        treeCopy.ValDef(tree, mods, name, transform(tpt), nrhs)

      // Catch Select on Enumeration.Value we couldn't transform but need to
      // we ignore the implementation of scala.Enumeration itself
      case ScalaEnumValue.NoName(_) if !inEnumImpl =>
        unit.warning(tree.pos,
                     """Couldn't transform call to Enumeration.Value.
                       |The resulting program is unlikely to function properly as this
                       |operation requires reflection.""".stripMargin)
        super.transform(tree)

      case ScalaEnumValue.NullName() if !inEnumImpl =>
        unit.warning(tree.pos,
                     """Passing null as name to Enumeration.Value
                       |requires reflection at runtime. The resulting
                       |program is unlikely to function properly.""".stripMargin)
        super.transform(tree)

      case ScalaEnumVal.NoName(_) if !inEnumImpl =>
        unit.warning(tree.pos,
                     """Calls to the non-string constructors of Enumeration.Val
                       |require reflection at runtime. The resulting
                       |program is unlikely to function properly.""".stripMargin)
        super.transform(tree)

      case ScalaEnumVal.NullName() if !inEnumImpl =>
        unit.warning(tree.pos,
                     """Passing null as name to a constructor of Enumeration.Val
                       |requires reflection at runtime. The resulting
                       |program is unlikely to function properly.""".stripMargin)
        super.transform(tree)

      // Catch calls to Predef.classOf[T]. These should NEVER reach this phase
      // but unfortunately do. In normal cases, the typer phase replaces these
      // calls by a literal constant of the given type. However, when we compile
      // the scala library itself and Predef.scala is in the sources, this does
      // not happen.
      //
      // The trees reach this phase under the form:
      //
      //   scala.this.Predef.classOf[T]
      //
      // If we encounter such a tree, depending on the plugin options, we fail
      // here or silently fix those calls.
      case TypeApply(
          classOfTree @ Select(Select(This(jstpnme.scala_), nme.Predef), nme.classOf),
          List(tpeArg)) =>
        if (scalaJSOpts.fixClassOf) {
          // Replace call by literal constant containing type
          if (typer.checkClassType(tpeArg)) {
            typer.typed { Literal(Constant(tpeArg.tpe.dealias.widen)) }
          } else {
            unit.error(tpeArg.pos, s"Type ${tpeArg} is not a class type")
            EmptyTree
          }
        } else {
          unit.error(classOfTree.pos,
              """This classOf resulted in an unresolved classOf in the jscode
                |phase. This is most likely a bug in the Scala compiler. ScalaJS
                |is probably able to work around this bug. Enable the workaround
                |by passing the fixClassOf option to the plugin.""".stripMargin)
          EmptyTree
        }

      // Exporter generation
      case ddef: DefDef =>
        // Generate exporters for this ddef if required
        exporters.getOrElseUpdate(ddef.symbol.owner,
            mutable.ListBuffer.empty) ++= genExportMember(ddef)

        super.transform(tree)

      // Module export sanity check (export generated in JSCode phase)
      case modDef: ModuleDef =>
        val sym = modDef.symbol

        def condErr(msg: String) = {
          for ((_, pos) <- jsInterop.exportsOf(sym)) {
            currentUnit.error(pos, msg)
          }
        }

        if (!sym.isPublic)
          condErr("You may not export an non-public object")
        else if (sym.isLocalToBlock)
          condErr("You may not export a local object")
        else if (!sym.owner.hasPackageFlag)
          condErr("You may not export a nested object")

        super.transform(modDef)

      // Fix for issue with calls to js.Dynamic.x()
      // Rewrite (obj: js.Dynamic).x(...) to obj.applyDynamic("x")(...)
      case Select(Select(trg, jsnme.x), nme.apply) if isJSDynamic(trg) =>
        val newTree = atPos(tree.pos) {
          Apply(
              Select(super.transform(trg), newTermName("applyDynamic")),
              List(Literal(Constant("x")))
          )
        }
        typer.typed(newTree, Mode.FUNmode, tree.tpe)


      // Fix for issue with calls to js.Dynamic.x()
      // Rewrite (obj: js.Dynamic).x to obj.selectDynamic("x")
      case Select(trg, jsnme.x) if isJSDynamic(trg) =>
        val newTree = atPos(tree.pos) {
          Apply(
              Select(super.transform(trg), newTermName("selectDynamic")),
              List(Literal(Constant("x")))
          )
        }
        typer.typed(newTree, Mode.FUNmode, tree.tpe)

      case _ => super.transform(tree)
    } }

    private def postTransform(tree: Tree) = tree match {
      case Template(parents, self, body) =>
        val clsSym = tree.symbol.owner
        val exports = exporters.get(clsSym).toIterable.flatten
        // Add exports to the template
        treeCopy.Template(tree, parents, self, body ++ exports)

      case memDef: MemberDef =>
        val sym = memDef.symbol
        if (sym.isLocalToBlock) {
          for ((_, pos) <- jsInterop.exportsOf(sym)) {
            currentUnit.error(pos, "You may not export a local definition")
          }
        }
        memDef

      case _ => tree
    }

    /**
     * Performs checks and rewrites specific to classes / objects extending
     * js.Any
     */
    private def transformJSAny(implDef: ImplDef) = {
      val sym = implDef.symbol

      lazy val badParent = sym.info.parents.find(t => !(t <:< JSAnyClass.tpe))
      def inScalaJSJSPackage =
        sym.enclosingPackage == ScalaJSJSPackage ||
        sym.enclosingPackage == ScalaJSJSPrimPackage

      implDef match {
        // Check that we do not have a case modifier
        case _ if implDef.mods.hasFlag(Flag.CASE) =>
          unit.error(implDef.pos, "Classes and objects extending " +
              "js.Any may not have a case modifier")

        // Check that we do not extends a trait that does not extends js.Any
        case _ if !inScalaJSJSPackage && !badParent.isEmpty &&
          !isJSLambda(sym) =>
          val badName = {
            val names = (badParent.get.typeSymbol.fullName, sym.fullName).zipped
            names.dropWhile(scala.Function.tupled(_ == _)).unzip._1.mkString
          }
          unit.error(implDef.pos, s"${sym.nameString} extends ${badName} " +
              "which does not extend js.Any.")

        // Check that we are not an anonymous class
        case cldef: ClassDef
          if cldef.symbol.isAnonymousClass && !isJSLambda(sym) =>
          unit.error(implDef.pos, "Anonymous classes may not " +
              "extend js.Any")

        // Check if we may have a js.Any here
        case cldef: ClassDef if !allowJSAny && !jsAnyClassOnly &&
          !isJSLambda(sym) =>
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
          // We cannot use sym directly, since the symbol
          // of a module is not its type's symbol but the value it declares
          val tSym = sym.tpe.typeSymbol

          tSym.setAnnotations(rawJSAnnot :: sym.annotations)

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

  def isJSAny(sym: Symbol): Boolean =
    sym.tpe.typeSymbol isSubClass JSAnyClass

  private def isJSAny(implDef: ImplDef): Boolean = isJSAny(implDef.symbol)

  private def isJSGlobalScope(implDef: ImplDef) =
    implDef.symbol.tpe.typeSymbol isSubClass JSGlobalScopeClass

  private def isJSLambda(sym: Symbol) = sym.isAnonymousClass &&
    AllJSFunctionClasses.exists(sym.tpe.typeSymbol isSubClass _)

  private def isScalaEnum(implDef: ImplDef) =
    implDef.symbol.tpe.typeSymbol isSubClass ScalaEnumClass

  private def isJSDynamic(tree: Tree) = tree.tpe.typeSymbol == JSDynamicClass

  /**
   * is this symbol a setter that has a non-unit return type
   *
   * these setters don't make sense in JS (in JS, assignment returns
   * the assigned value) and are therefore not allowed in facade types
   */
  private def isNonJSScalaSetter(sym: Symbol) = nme.isSetterName(sym.name) && {
    sym.tpe.paramss match {
      case List(List(arg)) =>
        !isScalaRepeatedParamType(arg.tpe) &&
        sym.tpe.resultType.typeSymbol != UnitClass
      case _ => false
    }
  }

  trait ScalaEnumFctExtractors {
    protected val methSym: Symbol

    protected def resolve(ptpes: Symbol*) = {
      val res = methSym suchThat {
        _.tpe.params.map(_.tpe.typeSymbol) == ptpes.toList
      }
      assert(res != NoSymbol)
      res
    }

    protected val noArg    = resolve()
    protected val nameArg  = resolve(StringClass)
    protected val intArg   = resolve(IntClass)
    protected val fullMeth = resolve(IntClass, StringClass)

    /**
     * Extractor object for calls to the targeted symbol that do not have an
     * explicit name in the parameters
     *
     * Extracts:
     * - `sel: Select` where sel.symbol is targeted symbol (no arg)
     * - Apply(meth, List(param)) where meth.symbol is targeted symbol (i: Int)
     */
    object NoName {
      def unapply(t: Tree) = t match {
        case sel: Select if sel.symbol == noArg =>
          Some(None)
        case Apply(meth, List(param)) if meth.symbol == intArg =>
          Some(Some(param))
        case _ =>
          None
      }
    }

    object NullName {
      def unapply(tree: Tree) = tree match {
        case Apply(meth, List(Literal(Constant(null)))) =>
          meth.symbol == nameArg
        case Apply(meth, List(_, Literal(Constant(null)))) =>
          meth.symbol == fullMeth
        case _ => false
      }
    }

  }

  private object ScalaEnumValue extends {
    protected val methSym = getMemberMethod(ScalaEnumClass, jsnme.Value)
  } with ScalaEnumFctExtractors

  private object ScalaEnumVal extends {
    protected val methSym = {
      val valSym = getMemberClass(ScalaEnumClass, jsnme.Val)
      valSym.tpe.member(nme.CONSTRUCTOR)
    }
  } with ScalaEnumFctExtractors

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

    val defaultName = nameOrig.asTerm.getterName.encoded


    // Construct the following tree
    //
    //   if (nextName != null && nextName.hasNext)
    //     nextName.next()
    //   else
    //     <defaultName>
    //
    val nextNameTree = Select(This(thisSym), jsnme.nextName)
    val nullCompTree =
      Apply(Select(nextNameTree, nme.NE), Literal(Constant(null)) :: Nil)
    val hasNextTree = Select(nextNameTree, jsnme.hasNext)
    val condTree = Apply(Select(nullCompTree, nme.ZAND), hasNextTree :: Nil)
    val nameTree = If(condTree,
        Apply(Select(nextNameTree, jsnme.next), Nil),
        Literal(Constant(defaultName)))
    val params = intParam.toList :+ nameTree

    typer.typed {
      Apply(Select(This(thisSym), jsnme.Value), params)
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
