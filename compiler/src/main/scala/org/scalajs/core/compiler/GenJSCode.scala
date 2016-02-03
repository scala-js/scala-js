/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package org.scalajs.core.compiler

import scala.language.implicitConversions

import scala.annotation.switch

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import scala.tools.nsc._

import scala.annotation.tailrec

import org.scalajs.core.ir
import ir.{Trees => js, Types => jstpe, ClassKind, Hashers}
import ir.Trees.OptimizerHints

import util.ScopedVar
import ScopedVar.withScopedVars

/** Generate JavaScript code and output it to disk
 *
 *  @author Sébastien Doeraene
 */
abstract class GenJSCode extends plugins.PluginComponent
                            with TypeKinds
                            with JSEncoding
                            with GenJSExports
                            with GenJSFiles
                            with PluginComponent210Compat {

  val jsAddons: JSGlobalAddons {
    val global: GenJSCode.this.global.type
  }

  val scalaJSOpts: ScalaJSOptions

  import global._
  import jsAddons._
  import rootMirror._
  import definitions._
  import jsDefinitions._
  import jsInterop.{jsNameOf, fullJSNameOf}
  import JSTreeExtractors._

  import treeInfo.hasSynthCaseSymbol

  import platform.isMaybeBoxed

  val phaseName: String = "jscode"
  override val description: String = "generate JavaScript code from ASTs"

  /** testing: this will be called when ASTs are generated */
  def generatedJSAST(clDefs: List[js.Tree]): Unit

  /** Implicit conversion from nsc Position to ir.Position. */
  implicit def pos2irPos(pos: Position): ir.Position = {
    if (pos == NoPosition) ir.Position.NoPosition
    else {
      val source = pos2irPosCache.toIRSource(pos.source)
      // nsc positions are 1-based but IR positions are 0-based
      ir.Position(source, pos.line-1, pos.column-1)
    }
  }

  private[this] object pos2irPosCache { // scalastyle:ignore
    import scala.reflect.internal.util._

    private[this] var lastNscSource: SourceFile = null
    private[this] var lastIRSource: ir.Position.SourceFile = null

    def toIRSource(nscSource: SourceFile): ir.Position.SourceFile = {
      if (nscSource != lastNscSource) {
        lastIRSource = convert(nscSource)
        lastNscSource = nscSource
      }
      lastIRSource
    }

    private[this] def convert(nscSource: SourceFile): ir.Position.SourceFile = {
      nscSource.file.file match {
        case null =>
          new java.net.URI(
              "virtualfile",       // Pseudo-Scheme
              nscSource.file.path, // Scheme specific part
              null                 // Fragment
          )
        case file =>
          val srcURI = file.toURI
          def matches(pat: java.net.URI) = pat.relativize(srcURI) != srcURI

          scalaJSOpts.sourceURIMaps.collectFirst {
            case ScalaJSOptions.URIMap(from, to) if matches(from) =>
              val relURI = from.relativize(srcURI)
              to.fold(relURI)(_.resolve(relURI))
          } getOrElse srcURI
      }
    }

    def clear(): Unit = {
      lastNscSource = null
      lastIRSource = null
    }
  }

  /** Materialize implicitly an ir.Position from an implicit nsc Position. */
  implicit def implicitPos2irPos(implicit pos: Position): ir.Position = pos

  override def newPhase(p: Phase): StdPhase = new JSCodePhase(p)

  private object jsnme { // scalastyle:ignore
    val arg_outer = newTermName("arg$outer")
    val newString = newTermName("newString")
  }

  class JSCodePhase(prev: Phase) extends StdPhase(prev) with JSExportsPhase {

    override def name: String = phaseName
    override def description: String = GenJSCode.this.description
    override def erasedTypes: Boolean = true

    // Some state --------------------------------------------------------------

    val currentClassSym          = new ScopedVar[Symbol]
    val currentMethodSym         = new ScopedVar[Symbol]
    val thisLocalVarIdent        = new ScopedVar[Option[js.Ident]](None)
    val fakeTailJumpParamRepl    = new ScopedVar[(Symbol, Symbol)]((NoSymbol, NoSymbol))
    val enclosingLabelDefParams  = new ScopedVar(Map.empty[Symbol, List[Symbol]])
    val mutableLocalVars         = new ScopedVar[mutable.Set[Symbol]]
    val mutatedLocalVars         = new ScopedVar[mutable.Set[Symbol]]
    val unexpectedMutatedFields  = new ScopedVar[mutable.Set[Symbol]]
    val paramAccessorLocals      = new ScopedVar(Map.empty[Symbol, js.ParamDef])

    var isModuleInitialized: Boolean = false // see genApply for super calls

    val countsOfReturnsToMatchEnd = mutable.Map.empty[Symbol, Int]

    private def currentClassType = encodeClassType(currentClassSym)

    val tryingToGenMethodAsJSFunction = new ScopedVar[Boolean](false)
    class CancelGenMethodAsJSFunction(message: String)
        extends Throwable(message) with scala.util.control.ControlThrowable

    // Rewriting of anonymous function classes ---------------------------------

    // scalastyle:off disallow.space.after.token
    private val translatedAnonFunctions =
      mutable.Map.empty[Symbol,
        /*ctor args:*/ List[js.Tree] => /*instance:*/ js.Tree]
    private val instantiatedAnonFunctions =
      mutable.Set.empty[Symbol]
    private val undefinedDefaultParams =
      mutable.Set.empty[Symbol]
    // scalastyle:on disallow.space.after.token

    // Top-level apply ---------------------------------------------------------

    override def run(): Unit = {
      scalaPrimitives.init()
      initializeCoreBTypesCompat()
      jsPrimitives.init()
      super.run()
    }

    /** Generates the Scala.js IR for a compilation unit
     *  This method iterates over all the class and interface definitions
     *  found in the compilation unit and emits their IR (.sjsir).
     *
     *  Some classes are never actually emitted:
     *  - Classes representing primitive types
     *  - The scala.Array class
     *  - Implementation classes for raw JS traits
     *
     *  Some classes representing anonymous functions are not actually emitted.
     *  Instead, a temporary representation of their `apply` method is built
     *  and recorded, so that it can be inlined as a JavaScript anonymous
     *  function in the method that instantiates it.
     *
     *  Other ClassDefs are emitted according to their nature:
     *  * Scala.js-defined JS class     -> `genScalaJSDefinedJSClass()`
     *  * Other raw JS type (<: js.Any) -> `genRawJSClassData()`
     *  * Interface                     -> `genInterface()`
     *  * Implementation class          -> `genImplClass()`
     *  * Normal class                  -> `genClass()`
     */
    override def apply(cunit: CompilationUnit): Unit = {
      try {
        val generatedClasses = ListBuffer.empty[(Symbol, js.ClassDef)]

        def collectClassDefs(tree: Tree): List[ClassDef] = {
          tree match {
            case EmptyTree => Nil
            case PackageDef(_, stats) => stats flatMap collectClassDefs
            case cd: ClassDef => cd :: Nil
          }
        }
        val allClassDefs = collectClassDefs(cunit.body)

        /* First gen and record lambdas for js.FunctionN and js.ThisFunctionN.
         * Since they are SAMs, there cannot be dependencies within this set,
         * and hence we are sure we can record them before they are used,
         * which is critical for these.
         */
        val nonRawJSFunctionDefs = allClassDefs filterNot { cd =>
          if (isRawJSFunctionDef(cd.symbol)) {
            genAndRecordRawJSFunctionClass(cd)
            true
          } else {
            false
          }
        }

        /* Then try to gen and record lambdas for scala.FunctionN.
         * These may fail, and sometimes because of dependencies. Since there
         * appears to be more forward dependencies than backward dependencies
         * (at least for non-nested lambdas, which we cannot translate anyway),
         * we process class defs in reverse order here.
         */
        val fullClassDefs = (nonRawJSFunctionDefs.reverse filterNot { cd =>
          cd.symbol.isAnonymousFunction && tryGenAndRecordAnonFunctionClass(cd)
        }).reverse

        /* Finally, we emit true code for the remaining class defs. */
        for (cd <- fullClassDefs) {
          val sym = cd.symbol
          implicit val pos = sym.pos

          /* Do not actually emit code for primitive types nor scala.Array. */
          val isPrimitive =
            isPrimitiveValueClass(sym) || (sym == ArrayClass)

          /* Similarly, do not emit code for impl classes of raw JS traits. */
          val isRawJSImplClass =
            sym.isImplClass && isRawJSType(
                sym.owner.info.decl(sym.name.dropRight(nme.IMPL_CLASS_SUFFIX.length)).tpe)

          if (!isPrimitive && !isRawJSImplClass) {
            withScopedVars(
                currentClassSym         := sym,
                unexpectedMutatedFields := mutable.Set.empty
            ) {
              val tree = if (isRawJSType(sym.tpe)) {
                assert(!isRawJSFunctionDef(sym),
                    s"Raw JS function def should have been recorded: $cd")
                if (!sym.isInterface && isScalaJSDefinedJSClass(sym))
                  genScalaJSDefinedJSClass(cd)
                else
                  genRawJSClassData(cd)
              } else if (sym.isInterface) {
                genInterface(cd)
              } else if (sym.isImplClass) {
                genImplClass(cd)
              } else {
                genClass(cd)
              }

              generatedClasses += ((sym, tree))
            }
          }
        }

        val clDefs = generatedClasses.map(_._2).toList
        generatedJSAST(clDefs)

        for ((sym, tree) <- generatedClasses) {
          genIRFile(cunit, sym, tree)
        }
      } finally {
        translatedAnonFunctions.clear()
        instantiatedAnonFunctions.clear()
        undefinedDefaultParams.clear()
        countsOfReturnsToMatchEnd.clear()
        pos2irPosCache.clear()
      }
    }

    // Generate a class --------------------------------------------------------

    /** Gen the IR ClassDef for a class definition (maybe a module class).
     */
    def genClass(cd: ClassDef): js.ClassDef = {
      val ClassDef(mods, name, _, impl) = cd
      val sym = cd.symbol
      implicit val pos = sym.pos

      assert(!sym.isInterface && !sym.isImplClass,
          "genClass() must be called only for normal classes: "+sym)
      assert(sym.superClass != NoSymbol, sym)

      if (hasDefaultCtorArgsAndRawJSModule(sym)) {
        reporter.error(pos,
            "Implementation restriction: constructors of " +
            "Scala classes cannot have default parameters " +
            "if their companion module is JS native.")
      }

      val classIdent = encodeClassFullNameIdent(sym)
      val isHijacked = isHijackedBoxedClass(sym)

      // Optimizer hints

      def isStdLibClassWithAdHocInlineAnnot(sym: Symbol): Boolean = {
        val fullName = sym.fullName
        (fullName.startsWith("scala.Tuple") && !fullName.endsWith("$")) ||
        (fullName.startsWith("scala.collection.mutable.ArrayOps$of"))
      }

      val shouldMarkInline = (
          sym.hasAnnotation(InlineAnnotationClass) ||
          (sym.isAnonymousFunction && !sym.isSubClass(PartialFunctionClass)) ||
          isStdLibClassWithAdHocInlineAnnot(sym))

      val optimizerHints =
        OptimizerHints.empty.
          withInline(shouldMarkInline).
          withNoinline(sym.hasAnnotation(NoinlineAnnotationClass))

      // Generate members (constructor + methods)

      val generatedMethods = new ListBuffer[js.MethodDef]
      val exportedSymbols = new ListBuffer[Symbol]

      def gen(tree: Tree): Unit = {
        tree match {
          case EmptyTree => ()
          case Template(_, _, body) => body foreach gen

          case ValDef(mods, name, tpt, rhs) =>
            () // fields are added via genClassFields()

          case dd: DefDef =>
            val sym = dd.symbol

            val isExport = jsInterop.isExport(sym)
            val isNamedExport = isExport && sym.annotations.exists(
                _.symbol == JSExportNamedAnnotation)

            if (isNamedExport)
              generatedMethods += genNamedExporterDef(dd)
            else
              generatedMethods ++= genMethod(dd)

            if (isExport) {
              // We add symbols that we have to export here. This way we also
              // get inherited stuff that is implemented in this class.
              exportedSymbols += sym
            }

          case _ => abort("Illegal tree in gen of genClass(): " + tree)
        }
      }

      gen(impl)

      // Generate fields if necessary (and add to methods + ctors)
      val generatedMembers =
        if (!isHijacked) genClassFields(cd) ++ generatedMethods.toList
        else generatedMethods.toList // No fields needed

      // Generate the exported members, constructors and accessors
      val exports = {
        // Generate the exported members
        val memberExports = genMemberExports(sym, exportedSymbols.toList)

        // Generate exported constructors or accessors
        val exportedConstructorsOrAccessors =
          if (isStaticModule(sym)) genModuleAccessorExports(sym)
          else genConstructorExports(sym)

        memberExports ++ exportedConstructorsOrAccessors
      }

      // Hashed definitions of the class
      val hashedDefs =
        Hashers.hashDefs(generatedMembers ++ exports)

      // The complete class definition
      val kind =
        if (isStaticModule(sym)) ClassKind.ModuleClass
        else if (isHijacked) ClassKind.HijackedClass
        else ClassKind.Class

      val classDefinition = js.ClassDef(
          classIdent,
          kind,
          Some(encodeClassFullNameIdent(sym.superClass)),
          genClassInterfaces(sym),
          None,
          hashedDefs)(
          optimizerHints)

      classDefinition
    }

    /** Gen the IR ClassDef for a Scala.js-defined JS class. */
    def genScalaJSDefinedJSClass(cd: ClassDef): js.ClassDef = {
      val sym = cd.symbol
      implicit val pos = sym.pos

      assert(isScalaJSDefinedJSClass(sym),
          "genScalaJSDefinedJSClass() must be called only for " +
          s"Scala.js-defined JS classes: $sym")
      assert(sym.superClass != NoSymbol, sym)

      val classIdent = encodeClassFullNameIdent(sym)

      // Generate members (constructor + methods)

      val constructorTrees = new ListBuffer[DefDef]
      val generatedMethods = new ListBuffer[js.MethodDef]
      val dispatchMethodNames = new ListBuffer[String]

      def gen(tree: Tree): Unit = {
        tree match {
          case EmptyTree => ()
          case Template(_, _, body) => body foreach gen

          case ValDef(mods, name, tpt, rhs) =>
            () // fields are added via genClassFields()

          case dd: DefDef =>
            val sym = dd.symbol
            val exposed = isExposed(sym)

            if (sym.isClassConstructor) {
              constructorTrees += dd
            } else if (exposed && sym.isAccessor) {
              /* Exposed accessors must not be emitted, since the field they
               * access is enough.
               */
            } else {
              generatedMethods ++= genMethod(dd)

              // Collect the names of the dispatchers we have to create
              if (exposed && !sym.isDeferred) {
                /* We add symbols that we have to expose here. This way we also
                 * get inherited stuff that is implemented in this class.
                 */
                dispatchMethodNames += jsNameOf(sym)
              }
            }

          case _ => abort("Illegal tree in gen of genClass(): " + tree)
        }
      }

      gen(cd.impl)

      // Generate class-level exporters
      val exports =
        if (isStaticModule(sym)) genModuleAccessorExports(sym)
        else genJSClassExports(sym)

      // Generate fields (and add to methods + ctors)
      val generatedMembers = {
        genClassFields(cd) :::
        genJSClassConstructor(sym, constructorTrees.toList) ::
        genJSClassDispatchers(sym, dispatchMethodNames.result().distinct) :::
        generatedMethods.toList :::
        exports
      }

      // Hashed definitions of the class
      val hashedDefs =
        Hashers.hashDefs(generatedMembers)

      // The complete class definition
      val kind =
        if (isStaticModule(sym)) ClassKind.JSModuleClass
        else ClassKind.JSClass

      val classDefinition = js.ClassDef(
          classIdent,
          kind,
          Some(encodeClassFullNameIdent(sym.superClass)),
          genClassInterfaces(sym),
          None,
          hashedDefs)(
          OptimizerHints.empty)

      classDefinition
    }

    // Generate the class data of a raw JS class -------------------------------

    /** Gen the IR ClassDef for a raw JS class or trait.
     */
    def genRawJSClassData(cd: ClassDef): js.ClassDef = {
      val sym = cd.symbol
      implicit val pos = sym.pos

      val classIdent = encodeClassFullNameIdent(sym)
      val superClass =
        if (sym.isInterface) None
        else Some(encodeClassFullNameIdent(sym.superClass))
      val jsName =
        if (sym.isInterface || sym.isModuleClass) None
        else Some(fullJSNameOf(sym))

      js.ClassDef(classIdent, ClassKind.RawJSType,
          superClass,
          genClassInterfaces(sym),
          jsName,
          Nil)(
          OptimizerHints.empty)
    }

    // Generate an interface ---------------------------------------------------

    /** Gen the IR ClassDef for an interface definition.
     */
    def genInterface(cd: ClassDef): js.ClassDef = {
      val sym = cd.symbol
      implicit val pos = sym.pos

      val classIdent = encodeClassFullNameIdent(sym)

      // fill in class info builder
      def gen(tree: Tree): List[js.MethodDef] = {
        tree match {
          case EmptyTree            => Nil
          case Template(_, _, body) => body.flatMap(gen)
          case dd: DefDef           => genMethod(dd).toList
          case _ =>
            abort("Illegal tree in gen of genInterface(): " + tree)
        }
      }
      val generatedMethods = gen(cd.impl)
      val interfaces = genClassInterfaces(sym)

      // Hashed definitions of the interface
      val hashedDefs =
        Hashers.hashDefs(generatedMethods)

      js.ClassDef(classIdent, ClassKind.Interface, None, interfaces, None,
          hashedDefs)(OptimizerHints.empty)
    }

    // Generate an implementation class of a trait -----------------------------

    /** Gen the IR ClassDef for an implementation class (of a trait).
     */
    def genImplClass(cd: ClassDef): js.ClassDef = {
      val ClassDef(mods, name, _, impl) = cd
      val sym = cd.symbol
      implicit val pos = sym.pos

      def gen(tree: Tree): List[js.MethodDef] = {
        tree match {
          case EmptyTree => Nil
          case Template(_, _, body) => body.flatMap(gen)

          case dd: DefDef =>
            assert(!dd.symbol.isDeferred,
                s"Found an abstract method in an impl class at $pos: ${dd.symbol.fullName}")
            val m = genMethod(dd)
            m.toList

          case _ => abort("Illegal tree in gen of genImplClass(): " + tree)
        }
      }
      val generatedMethods = gen(impl)

      val classIdent = encodeClassFullNameIdent(sym)
      val objectClassIdent = encodeClassFullNameIdent(ObjectClass)

      // Hashed definitions of the impl class
      val hashedDefs =
        Hashers.hashDefs(generatedMethods)

      js.ClassDef(classIdent, ClassKind.Class,
          Some(objectClassIdent), Nil, None,
          hashedDefs)(OptimizerHints.empty)
    }

    private def genClassInterfaces(sym: Symbol)(
        implicit pos: Position): List[js.Ident] = {
      for {
        parent <- sym.info.parents
        typeSym = parent.typeSymbol
        _ = assert(typeSym != NoSymbol, "parent needs symbol")
        if (typeSym.isInterface)
      } yield {
        encodeClassFullNameIdent(typeSym)
      }
    }

    // Generate the fields of a class ------------------------------------------

    /** Gen definitions for the fields of a class.
     *  The fields are initialized with the zero of their types.
     */
    def genClassFields(cd: ClassDef): List[js.FieldDef] = {
      val classSym = cd.symbol
      assert(currentClassSym.get == classSym,
          "genClassFields called with a ClassDef other than the current one")

      // Non-method term members are fields, except for module members.
      (for {
        f <- classSym.info.decls
        if !f.isMethod && f.isTerm && !f.isModule
      } yield {
        implicit val pos = f.pos

        val mutable =
          suspectFieldMutable(f) || unexpectedMutatedFields.contains(f)
        val name =
          if (isExposed(f)) js.StringLiteral(jsNameOf(f))
          else encodeFieldSym(f)

        val irTpe = if (!isScalaJSDefinedJSClass(classSym)) {
          toIRType(f.tpe)
        } else {
          val tpeEnteringPosterasure =
            enteringPhase(currentRun.posterasurePhase)(f.tpe)
          tpeEnteringPosterasure match {
            case tpe: ErasedValueType =>
              /* Here, we must store the field as the boxed representation of
               * the value class. The default value of that field, as
               * initialized at the time the instance is created, will
               * therefore be null. This will not match the behavior we would
               * get in a Scala class. To match the behavior, we would need to
               * initialized to an instance of the boxed representation, with
               * an underlying value set to the zero of its type. However we
               * cannot implement that, so we live with the discrepancy.
               * Anyway, scalac also has problems with uninitialized value
               * class values, if they come from a generic context.
               */
              jstpe.ClassType(encodeClassFullName(tpe.valueClazz))

            case _ if f.tpe.typeSymbol == CharClass =>
              /* Will be initialized to null, which will unbox to '\0' when
               * read.
               */
              jstpe.ClassType(ir.Definitions.BoxedCharacterClass)

            case _ =>
              /* Other types are not boxed, so we can initialized them to
               * their true zero.
               */
              toIRType(f.tpe)
          }
        }

        js.FieldDef(name, irTpe, mutable)
      }).toList
    }

    // Constructor of a Scala.js-defined JS class ------------------------------

    def genJSClassConstructor(classSym: Symbol,
        constructorTrees: List[DefDef]): js.Tree = {
      implicit val pos = classSym.pos

      if (hasDefaultCtorArgsAndRawJSModule(classSym)) {
        reporter.error(pos,
            "Implementation restriction: constructors of " +
            "Scala.js-defined JS classes cannot have default parameters " +
            "if their companion module is JS native.")
        js.EmptyTree
      } else {
        withNewLocalNameScope {
          val ctors: List[js.MethodDef] = constructorTrees.flatMap { tree =>
            genMethodWithCurrentLocalNameScope(tree)
          }

          val dispatch =
            genJSConstructorExport(constructorTrees.map(_.symbol))
          val js.MethodDef(_, dispatchName, dispatchArgs, dispatchResultType,
          dispatchResolution) = dispatch

          val jsConstructorBuilder = mkJSConstructorBuilder(ctors)

          val overloadIdent = freshLocalIdent("overload")

          // Section containing the overload resolution and casts of parameters
          val overloadSelection = mkOverloadSelection(jsConstructorBuilder,
            overloadIdent, dispatchResolution)

          /* Section containing all the code executed before the call to `this`
           * for every secondary constructor.
           */
          val prePrimaryCtorBody =
            jsConstructorBuilder.mkPrePrimaryCtorBody(overloadIdent)

          val primaryCtorBody = jsConstructorBuilder.primaryCtorBody

          /* Section containing all the code executed after the call to this for
           * every secondary constructor.
           */
          val postPrimaryCtorBody =
            jsConstructorBuilder.mkPostPrimaryCtorBody(overloadIdent)

          val newBody = js.Block(overloadSelection ::: prePrimaryCtorBody ::
              primaryCtorBody :: postPrimaryCtorBody :: Nil)

          js.MethodDef(static = false, dispatchName, dispatchArgs, jstpe.NoType,
              newBody)(dispatch.optimizerHints, None)
        }
      }
    }

    private class ConstructorTree(val overrideNum: Int, val method: js.MethodDef,
        val subConstructors: List[ConstructorTree]) {

      lazy val overrideNumBounds: (Int, Int) =
        if (subConstructors.isEmpty) (overrideNum, overrideNum)
        else (subConstructors.head.overrideNumBounds._1, overrideNum)

      def get(methodName: String): Option[ConstructorTree] = {
        if (methodName == this.method.name.name) {
          Some(this)
        } else {
          subConstructors.iterator.map(_.get(methodName)).collectFirst {
            case Some(node) => node
          }
        }
      }

      def getParamRefs(implicit pos: Position): List[js.VarRef] =
        method.args.map(_.ref)

      def getAllParamDefsAsVars(implicit pos: Position): List[js.VarDef] = {
        val localDefs = method.args.map { pDef =>
          js.VarDef(pDef.name, pDef.ptpe, mutable = true, jstpe.zeroOf(pDef.ptpe))
        }
        localDefs ++ subConstructors.flatMap(_.getAllParamDefsAsVars)
      }
    }

    private class JSConstructorBuilder(root: ConstructorTree) {

      def primaryCtorBody: js.Tree = root.method.body

      def hasSubConstructors: Boolean = root.subConstructors.nonEmpty

      def getOverrideNum(methodName: String): Int =
        root.get(methodName).fold(-1)(_.overrideNum)

      def getParamRefsFor(methodName: String)(implicit pos: Position): List[js.VarRef] =
        root.get(methodName).fold(List.empty[js.VarRef])(_.getParamRefs)

      def getAllParamDefsAsVars(implicit pos: Position): List[js.VarDef] =
        root.getAllParamDefsAsVars

      def mkPrePrimaryCtorBody(overrideNumIdent: js.Ident)(
          implicit pos: Position): js.Tree = {
        val overrideNumRef = js.VarRef(overrideNumIdent)(jstpe.IntType)
        mkSubPreCalls(root, overrideNumRef)
      }

      def mkPostPrimaryCtorBody(overrideNumIdent: js.Ident)(
          implicit pos: Position): js.Tree = {
        val overrideNumRef = js.VarRef(overrideNumIdent)(jstpe.IntType)
        js.Block(mkSubPostCalls(root, overrideNumRef))
      }

      private def mkSubPreCalls(constructorTree: ConstructorTree,
          overrideNumRef: js.VarRef)(implicit pos: Position): js.Tree = {
        val overrideNumss = constructorTree.subConstructors.map(_.overrideNumBounds)
        val paramRefs = constructorTree.getParamRefs
        val bodies = constructorTree.subConstructors.map { constructorTree =>
          mkPrePrimaryCtorBodyOnSndCtr(constructorTree, overrideNumRef, paramRefs)
        }
        overrideNumss.zip(bodies).foldRight[js.Tree](js.Skip()) {
          case ((numBounds, body), acc) =>
            val cond = mkOverrideNumsCond(overrideNumRef, numBounds)
            js.If(cond, body, acc)(jstpe.BooleanType)
        }
      }

      private def mkPrePrimaryCtorBodyOnSndCtr(constructorTree: ConstructorTree,
          overrideNumRef: js.VarRef, outputParams: List[js.VarRef])(
          implicit pos: Position): js.Tree = {
        val subCalls =
          mkSubPreCalls(constructorTree, overrideNumRef)

        val preSuperCall = {
          constructorTree.method.body match {
            case js.Block(stats) =>
              val beforeSuperCall = stats.takeWhile {
                case js.ApplyStatic(_, mtd, _) => !ir.Definitions.isConstructorName(mtd.name)
                case _                         => true
              }
              val superCallParams = stats.collectFirst {
                case js.ApplyStatic(_, mtd, js.This() :: args)
                    if ir.Definitions.isConstructorName(mtd.name) =>
                  zipMap(outputParams, args)(js.Assign(_, _))
              }.getOrElse(Nil)

              beforeSuperCall ::: superCallParams

            case js.ApplyStatic(_, mtd, js.This() :: args)
                if ir.Definitions.isConstructorName(mtd.name) =>
              zipMap(outputParams, args)(js.Assign(_, _))

            case _ => Nil
          }
        }

        js.Block(subCalls :: preSuperCall)
      }

      private def mkSubPostCalls(constructorTree: ConstructorTree,
          overrideNumRef: js.VarRef)(implicit pos: Position): js.Tree = {
        val overrideNumss = constructorTree.subConstructors.map(_.overrideNumBounds)
        val bodies = constructorTree.subConstructors.map { ct =>
          mkPostPrimaryCtorBodyOnSndCtr(ct, overrideNumRef)
        }
        overrideNumss.zip(bodies).foldRight[js.Tree](js.Skip()) {
          case ((numBounds, js.Skip()), acc) => acc

          case ((numBounds, body), acc) =>
            val cond = mkOverrideNumsCond(overrideNumRef, numBounds)
            js.If(cond, body, acc)(jstpe.BooleanType)
        }
      }

      private def mkPostPrimaryCtorBodyOnSndCtr(constructorTree: ConstructorTree,
          overrideNumRef: js.VarRef)(implicit pos: Position): js.Tree = {
        val postSuperCall = {
          constructorTree.method.body match {
            case js.Block(stats) =>
              stats.dropWhile {
                case js.ApplyStatic(_, mtd, _) => !ir.Definitions.isConstructorName(mtd.name)
                case _                         => true
              }.tail

            case _ => Nil
          }
        }
        js.Block(postSuperCall :+ mkSubPostCalls(constructorTree, overrideNumRef))
      }

      private def mkOverrideNumsCond(numRef: js.VarRef,
          numBounds: (Int, Int))(implicit pos: Position) = numBounds match {
        case (lo, hi) if lo == hi =>
          js.BinaryOp(js.BinaryOp.===, js.IntLiteral(lo), numRef)

        case (lo, hi) if lo == hi - 1 =>
          val lhs = js.BinaryOp(js.BinaryOp.===, numRef, js.IntLiteral(lo))
          val rhs = js.BinaryOp(js.BinaryOp.===, numRef, js.IntLiteral(hi))
          js.If(lhs, js.BooleanLiteral(true), rhs)(jstpe.BooleanType)

        case (lo, hi) =>
          val lhs = js.BinaryOp(js.BinaryOp.Num_<=, js.IntLiteral(lo), numRef)
          val rhs = js.BinaryOp(js.BinaryOp.Num_<=, numRef, js.IntLiteral(hi))
          js.BinaryOp(js.BinaryOp.Boolean_&, lhs, rhs)
          js.If(lhs, rhs, js.BooleanLiteral(false))(jstpe.BooleanType)
      }
    }

    private def zipMap[T, U, V](xs: List[T], ys: List[U])(
        f: (T, U) => V): List[V] = {
      for ((x, y) <- xs zip ys) yield f(x, y)
    }

    /** mkOverloadSelection return a list of `stats` with that starts with:
     *  1) The definition for the local variable that will hold the overload
     *     resolution number.
     *  2) The definitions of all local variables that are used as parameters
     *     in all the constructors.
     *  3) The overload resolution match/if statements. For each overload the
     *     overload number is assigned and the parameters are cast and assigned
     *     to their corresponding variables.
     */
    private def mkOverloadSelection(jsConstructorBuilder: JSConstructorBuilder,
        overloadIdent: js.Ident, dispatchResolution: js.Tree)(
        implicit pos: Position): List[js.Tree]= {
      if (!jsConstructorBuilder.hasSubConstructors) {
        dispatchResolution match {
          /* Dispatch to constructor with no arguments.
           * Contains trivial parameterless call to the constructor.
           */
          case js.ApplyStatic(_, mtd, js.This() :: Nil)
              if ir.Definitions.isConstructorName(mtd.name) =>
            Nil

          /* Dispatch to constructor with at least one argument.
           * Where js.Block's stats.init corresponds to the parameter casts and
           * js.Block's stats.last contains the call to the constructor.
           */
          case js.Block(stats) =>
            val js.ApplyStatic(_, method, _) = stats.last
            val refs = jsConstructorBuilder.getParamRefsFor(method.name)
            val paramCasts = stats.init.map(_.asInstanceOf[js.VarDef])
            zipMap(refs, paramCasts) { (ref, paramCast) =>
              js.VarDef(ref.ident, ref.tpe, mutable = false, paramCast.rhs)
            }
        }
      } else {
        val overloadRef = js.VarRef(overloadIdent)(jstpe.IntType)

        /* transformDispatch takes the body of the method generated by
         * `genJSConstructorExport` and transform it recursively.
         */
        def transformDispatch(tree: js.Tree): js.Tree = tree match {
          /* Dispatch to constructor with no arguments.
           * Contains trivial parameterless call to the constructor.
           */
          case js.ApplyStatic(_, method, js.This() :: Nil)
              if ir.Definitions.isConstructorName(method.name) =>
            js.Assign(overloadRef,
              js.IntLiteral(jsConstructorBuilder.getOverrideNum(method.name)))

          /* Dispatch to constructor with at least one argument.
           * Where js.Block's stats.init corresponds to the parameter casts and
           * js.Block's stats.last contains the call to the constructor.
           */
          case js.Block(stats) =>
            val js.ApplyStatic(_, method, _) = stats.last

            val num = jsConstructorBuilder.getOverrideNum(method.name)
            val overloadAssign = js.Assign(overloadRef, js.IntLiteral(num))

            val refs = jsConstructorBuilder.getParamRefsFor(method.name)
            val paramCasts = stats.init.map(_.asInstanceOf[js.VarDef].rhs)
            val parameterAssigns = zipMap(refs, paramCasts)(js.Assign(_, _))

            js.Block(overloadAssign :: parameterAssigns)

          // Parameter count resolution
          case js.Match(selector, cases, default) =>
            val newCases = cases.map {
              case (literals, body) => (literals, transformDispatch(body))
            }
            val newDefault = transformDispatch(default)
            js.Match(selector, newCases, newDefault)(tree.tpe)

          // Parameter type resolution
          case js.If(cond, thenp, elsep) =>
            js.If(cond, transformDispatch(thenp),
                transformDispatch(elsep))(tree.tpe)

          // Throw(StringLiteral(No matching overload))
          case tree: js.Throw =>
            tree
        }

        val newDispatchResolution = transformDispatch(dispatchResolution)
        val allParamDefsAsVars = jsConstructorBuilder.getAllParamDefsAsVars
        val overrideNumDef =
          js.VarDef(overloadIdent, jstpe.IntType, mutable = true, js.IntLiteral(0))

        overrideNumDef :: allParamDefsAsVars ::: newDispatchResolution :: Nil
      }
    }

    private def mkJSConstructorBuilder(ctors: List[js.MethodDef])(
        implicit pos: Position): JSConstructorBuilder = {
      def findCtorForwarderCall(tree: js.Tree): String = tree match {
        case js.ApplyStatic(_, method, js.This() :: _)
            if ir.Definitions.isConstructorName(method.name) =>
          method.name

        case js.Block(stats) =>
          stats.collectFirst {
            case js.ApplyStatic(_, method, js.This() :: _)
                if ir.Definitions.isConstructorName(method.name) =>
              method.name
          }.get
      }

      val (primaryCtor :: Nil, secondaryCtors) = ctors.partition {
        _.body match {
          case js.Block(stats) =>
            stats.exists(_.isInstanceOf[js.JSSuperConstructorCall])

          case _: js.JSSuperConstructorCall => true
          case _                            => false
        }
      }

      val ctorToChildren = secondaryCtors.map { ctor =>
        findCtorForwarderCall(ctor.body) -> ctor
      }.groupBy(_._1).mapValues(_.map(_._2)).withDefaultValue(Nil)

      var overrideNum = -1
      def mkConstructorTree(method: js.MethodDef): ConstructorTree = {
        val methodName = method.name.name
        val subCtrTrees = ctorToChildren(methodName).map(mkConstructorTree)
        overrideNum += 1
        new ConstructorTree(overrideNum, method, subCtrTrees)
      }

      new JSConstructorBuilder(mkConstructorTree(primaryCtor))
    }

    // Generate a method -------------------------------------------------------

    def genMethod(dd: DefDef): Option[js.MethodDef] = {
      withNewLocalNameScope {
        genMethodWithCurrentLocalNameScope(dd)
      }
    }

    /** Gen JS code for a method definition in a class or in an impl class.
     *  On the JS side, method names are mangled to encode the full signature
     *  of the Scala method, as described in `JSEncoding`, to support
     *  overloading.
     *
     *  Some methods are not emitted at all:
     *  * Primitives, since they are never actually called
     *  * Abstract methods
     *  * Constructors of hijacked classes
     *  * Methods with the {{{@JavaDefaultMethod}}} annotation mixed in classes.
     *
     *  Constructors are emitted by generating their body as a statement.
     *
     *  Interface methods with the {{{@JavaDefaultMethod}}} annotation produce
     *  default methods forwarding to the trait impl class method.
     *
     *  Other (normal) methods are emitted with `genMethodBody()`.
     */
    def genMethodWithCurrentLocalNameScope(dd: DefDef): Option[js.MethodDef] = {
      implicit val pos = dd.pos
      val DefDef(mods, name, _, vparamss, _, rhs) = dd
      val sym = dd.symbol

      isModuleInitialized = false

      withScopedVars(
          currentMethodSym        := sym,
          thisLocalVarIdent       := None,
          fakeTailJumpParamRepl   := (NoSymbol, NoSymbol),
          enclosingLabelDefParams := Map.empty
      ) {
        assert(vparamss.isEmpty || vparamss.tail.isEmpty,
            "Malformed parameter list: " + vparamss)
        val params = if (vparamss.isEmpty) Nil else vparamss.head map (_.symbol)

        val isJSClassConstructor =
          sym.isClassConstructor && isScalaJSDefinedJSClass(currentClassSym)

        val methodName: js.PropertyName = encodeMethodSym(sym)

        def jsParams = for (param <- params) yield {
          implicit val pos = param.pos
          js.ParamDef(encodeLocalSym(param), toIRType(param.tpe),
              mutable = false, rest = false)
        }

        if (scalaPrimitives.isPrimitive(sym)) {
          None
        } else if (sym.isDeferred || sym.owner.isInterface) {
          val body = if (sym.hasAnnotation(JavaDefaultMethodAnnotation)) {
            /* For an interface method with @JavaDefaultMethod, make it a
             * default method calling the impl class method.
             */
            val implClassSym = sym.owner.implClass
            val implMethodSym = implClassSym.info.member(sym.name).suchThat { s =>
              s.isMethod &&
              s.tpe.params.size == sym.tpe.params.size + 1 &&
              s.tpe.params.head.tpe =:= sym.owner.toTypeConstructor &&
              s.tpe.params.tail.zip(sym.tpe.params).forall {
                case (sParam, symParam) =>
                  sParam.tpe =:= symParam.tpe
              }
            }
            genTraitImplApply(implMethodSym,
                js.This()(currentClassType) :: jsParams.map(_.ref))
          } else {
            js.EmptyTree
          }
          Some(js.MethodDef(static = false, methodName,
              jsParams, toIRType(sym.tpe.resultType), body)(
              OptimizerHints.empty, None))
        } else if (isJSNativeCtorDefaultParam(sym)) {
          None
        } else if (sym.isClassConstructor && isHijackedBoxedClass(sym.owner)) {
          None
        } else if (!sym.owner.isImplClass &&
            sym.hasAnnotation(JavaDefaultMethodAnnotation)) {
          // Do not emit trait impl forwarders with @JavaDefaultMethod
          None
        } else {
          withScopedVars(
              mutableLocalVars := mutable.Set.empty,
              mutatedLocalVars := mutable.Set.empty
          ) {
            def isTraitImplForwarder = dd.rhs match {
              case app: Apply => foreignIsImplClass(app.symbol.owner)
              case _          => false
            }

            val shouldMarkInline = {
              sym.hasAnnotation(InlineAnnotationClass) ||
              sym.name.startsWith(nme.ANON_FUN_NAME) ||
              adHocInlineMethods.contains(sym.fullName)
            }

            val shouldMarkNoinline = {
              sym.hasAnnotation(NoinlineAnnotationClass) &&
              !isTraitImplForwarder &&
              !ignoreNoinlineAnnotation(sym)
            }

            val optimizerHints =
              OptimizerHints.empty.
                withInline(shouldMarkInline).
                withNoinline(shouldMarkNoinline)

            val methodDef = {
              if (isJSClassConstructor) {
                val body0 = genStat(rhs)
                val body1 =
                  if (!sym.isPrimaryConstructor) body0
                  else moveAllStatementsAfterSuperConstructorCall(body0)
                js.MethodDef(static = false, methodName,
                    jsParams, jstpe.NoType, body1)(optimizerHints, None)
              } else if (sym.isClassConstructor) {
                js.MethodDef(static = false, methodName,
                    jsParams, jstpe.NoType,
                    genStat(rhs))(optimizerHints, None)
              } else {
                val resultIRType = toIRType(sym.tpe.resultType)
                genMethodDef(static = sym.owner.isImplClass, methodName,
                    params, resultIRType, rhs, optimizerHints)
              }
            }

            val methodDefWithoutUselessVars = {
              val unmutatedMutableLocalVars =
                (mutableLocalVars -- mutatedLocalVars).toList
              val mutatedImmutableLocalVals =
                (mutatedLocalVars -- mutableLocalVars).toList
              if (unmutatedMutableLocalVars.isEmpty &&
                  mutatedImmutableLocalVals.isEmpty) {
                // OK, we're good (common case)
                methodDef
              } else {
                val patches = (
                    unmutatedMutableLocalVars.map(encodeLocalSym(_).name -> false) :::
                    mutatedImmutableLocalVals.map(encodeLocalSym(_).name -> true)
                ).toMap
                patchMutableFlagOfLocals(methodDef, patches)
              }
            }

            Some(methodDefWithoutUselessVars)
          }
        }
      }
    }

    private val adHocInlineMethods = Set(
        "scala.collection.mutable.ArrayOps$ofRef.newBuilder$extension",
        "scala.runtime.ScalaRunTime.arrayClass",
        "scala.runtime.ScalaRunTime.arrayElementClass"
    )

    /** Patches the mutable flags of selected locals in a [[js.MethodDef]].
     *
     *  @param patches  Map from local name to new value of the mutable flags.
     *                  For locals not in the map, the flag is untouched.
     */
    private def patchMutableFlagOfLocals(methodDef: js.MethodDef,
        patches: Map[String, Boolean]): js.MethodDef = {

      def newMutable(name: String, oldMutable: Boolean): Boolean =
        patches.getOrElse(name, oldMutable)

      val js.MethodDef(static, methodName, params, resultType, body) = methodDef
      val newParams = for {
        p @ js.ParamDef(name, ptpe, mutable, rest) <- params
      } yield {
        js.ParamDef(name, ptpe, newMutable(name.name, mutable), rest)(p.pos)
      }
      val transformer = new ir.Transformers.Transformer {
        override def transform(tree: js.Tree, isStat: Boolean): js.Tree = tree match {
          case js.VarDef(name, vtpe, mutable, rhs) =>
            assert(isStat)
            super.transform(js.VarDef(
                name, vtpe, newMutable(name.name, mutable), rhs)(tree.pos), isStat)
          case js.Closure(captureParams, params, body, captureValues) =>
            js.Closure(captureParams, params, body,
                captureValues.map(transformExpr))(tree.pos)
          case _ =>
            super.transform(tree, isStat)
        }
      }
      val newBody =
        transformer.transform(body, isStat = resultType == jstpe.NoType)
      js.MethodDef(static, methodName, newParams, resultType,
          newBody)(methodDef.optimizerHints, None)(methodDef.pos)
    }

    /** Moves all statements after the super constructor call.
     *
     *  This is used for the primary constructor of a Scala.js-defined JS
     *  class, because those cannot access `this` before the super constructor
     *  call.
     *
     *  scalac inserts statements before the super constructor call for early
     *  initializers and param accessor initializers (including val's and var's
     *  declared in the params). We move those after the super constructor
     *  call, and are therefore executed later than for a Scala class.
     */
    private def moveAllStatementsAfterSuperConstructorCall(
        body: js.Tree): js.Tree = {
      val bodyStats = body match {
        case js.Block(stats) => stats
        case _               => body :: Nil
      }

      val (beforeSuper, superCall :: afterSuper) =
        bodyStats.span(!_.isInstanceOf[js.JSSuperConstructorCall])

      assert(!beforeSuper.exists(_.isInstanceOf[js.VarDef]),
          "Trying to move a local VarDef after the super constructor call " +
          "of a Scala.js-defined JS class at ${body.pos}")

      js.Block(
          superCall ::
          beforeSuper :::
          afterSuper)(body.pos)
    }

    /** Generates the MethodDef of a (non-constructor) method
     *
     *  Most normal methods are emitted straightforwardly. If the result
     *  type is Unit, then the body is emitted as a statement. Otherwise, it is
     *  emitted as an expression.
     *
     *  The additional complexity of this method handles the transformation of
     *  a peculiarity of recursive tail calls: the local ValDef that replaces
     *  `this`.
     */
    def genMethodDef(static: Boolean, methodName: js.PropertyName,
        paramsSyms: List[Symbol], resultIRType: jstpe.Type,
        tree: Tree, optimizerHints: OptimizerHints): js.MethodDef = {
      implicit val pos = tree.pos

      val jsParams = for (param <- paramsSyms) yield {
        implicit val pos = param.pos
        js.ParamDef(encodeLocalSym(param), toIRType(param.tpe),
            mutable = false, rest = false)
      }

      val bodyIsStat = resultIRType == jstpe.NoType

      def genBody() = tree match {
        case Block(
            (thisDef @ ValDef(_, nme.THIS, _, initialThis)) :: otherStats,
            rhs) =>
          // This method has tail jumps

          // To be called from within withScopedVars
          def genInnerBody() = {
            js.Block(otherStats.map(genStat) :+ (
                if (bodyIsStat) genStat(rhs)
                else            genExpr(rhs)))
          }

          initialThis match {
            case This(_) =>
              val thisSym = thisDef.symbol
              if (thisSym.isMutable)
                mutableLocalVars += thisSym

              val thisLocalIdent = encodeLocalSym(thisSym)
              val thisLocalVarDef = js.VarDef(thisLocalIdent,
                  currentClassType, thisSym.isMutable, genThis())

              val innerBody = {
                withScopedVars(
                  thisLocalVarIdent := Some(thisLocalIdent)
                ) {
                  genInnerBody()
                }
              }

              js.Block(thisLocalVarDef, innerBody)

            case Ident(_) =>
              withScopedVars(
                fakeTailJumpParamRepl := (thisDef.symbol, initialThis.symbol)
              ) {
                genInnerBody()
              }
          }

        case _ =>
          if (bodyIsStat) genStat(tree)
          else            genExpr(tree)
      }

      if (!isScalaJSDefinedJSClass(currentClassSym)) {
        js.MethodDef(static, methodName, jsParams, resultIRType, genBody())(
            optimizerHints, None)
      } else {
        assert(!static, tree.pos)

        withScopedVars(
          thisLocalVarIdent := Some(freshLocalIdent("this"))
        ) {
          val thisParamDef = js.ParamDef(thisLocalVarIdent.get.get,
              jstpe.AnyType, mutable = false, rest = false)

          js.MethodDef(static = true, methodName, thisParamDef :: jsParams,
              resultIRType, genBody())(
              optimizerHints, None)
        }
      }
    }

    /** Gen JS code for a tree in statement position (in the IR).
     */
    def genStat(tree: Tree): js.Tree = {
      exprToStat(genStatOrExpr(tree, isStat = true))
    }

    /** Turn a JavaScript expression of type Unit into a statement */
    def exprToStat(tree: js.Tree): js.Tree = {
      /* Any JavaScript expression is also a statement, but at least we get rid
       * of some pure expressions that come from our own codegen.
       */
      implicit val pos = tree.pos
      tree match {
        case js.Block(stats :+ expr)  => js.Block(stats :+ exprToStat(expr))
        case _:js.Literal | js.This() => js.Skip()
        case _                        => tree
      }
    }

    /** Gen JS code for a tree in expression position (in the IR).
     */
    def genExpr(tree: Tree): js.Tree = {
      val result = genStatOrExpr(tree, isStat = false)
      assert(result.tpe != jstpe.NoType,
          s"genExpr($tree) returned a tree with type NoType at pos ${tree.pos}")
      result
    }

    /** Gen JS code for a tree in statement or expression position (in the IR).
     *
     *  This is the main transformation method. Each node of the Scala AST
     *  is transformed into an equivalent portion of the JS AST.
     */
    def genStatOrExpr(tree: Tree, isStat: Boolean): js.Tree = {
      implicit val pos = tree.pos

      tree match {
        /** LabelDefs (for while and do..while loops) */
        case lblDf: LabelDef =>
          genLabelDef(lblDf)

        /** Local val or var declaration */
        case ValDef(_, name, _, rhs) =>
          /* Must have been eliminated by the tail call transform performed
           * by genMethodBody(). */
          assert(name != nme.THIS,
              s"ValDef(_, nme.THIS, _, _) found at ${tree.pos}")

          val sym = tree.symbol
          val rhsTree =
            if (rhs == EmptyTree) genZeroOf(sym.tpe)
            else genExpr(rhs)

          rhsTree match {
            case js.UndefinedParam() =>
              // This is an intermediate assignment for default params on a
              // js.Any. Add the symbol to the corresponding set to inform
              // the Ident resolver how to replace it and don't emit the symbol
              undefinedDefaultParams += sym
              js.Skip()
            case _ =>
              if (sym.isMutable)
                mutableLocalVars += sym
              js.VarDef(encodeLocalSym(sym),
                  toIRType(sym.tpe), sym.isMutable, rhsTree)
          }

        case If(cond, thenp, elsep) =>
          js.If(genExpr(cond), genStatOrExpr(thenp, isStat),
              genStatOrExpr(elsep, isStat))(toIRType(tree.tpe))

        case Return(expr) =>
          js.Return(toIRType(expr.tpe) match {
            case jstpe.NoType => js.Block(genStat(expr), js.Undefined())
            case _            => genExpr(expr)
          })

        case t: Try =>
          genTry(t, isStat)

        case Throw(expr) =>
          val ex = genExpr(expr)
          js.Throw {
            if (isMaybeJavaScriptException(expr.tpe)) {
              genApplyMethod(
                  genLoadModule(RuntimePackageModule),
                  Runtime_unwrapJavaScriptException,
                  List(ex))
            } else {
              ex
            }
          }

        case app: Apply =>
          genApply(app, isStat)

        case app: ApplyDynamic =>
          genApplyDynamic(app)

        case This(qual) =>
          if (tree.symbol == currentClassSym.get) {
            genThis()
          } else {
            assert(tree.symbol.isModuleClass,
                "Trying to access the this of another class: " +
                "tree.symbol = " + tree.symbol +
                ", class symbol = " + currentClassSym.get +
                " compilation unit:" + currentUnit)
            genLoadModule(tree.symbol)
          }

        case Select(qualifier, selector) =>
          val sym = tree.symbol
          if (sym.isModule) {
            assert(!sym.isPackageClass, "Cannot use package as value: " + tree)
            genLoadModule(sym)
          } else if (sym.isStaticMember) {
            genStaticMember(sym)
          } else if (paramAccessorLocals contains sym) {
            paramAccessorLocals(sym).ref
          } else if (isScalaJSDefinedJSClass(sym.owner)) {
            val genQual = genExpr(qualifier)
            val boxed = if (isExposed(sym))
              js.JSBracketSelect(genQual, js.StringLiteral(jsNameOf(sym)))
            else
              js.JSDotSelect(genQual, encodeFieldSym(sym))
            fromAny(boxed,
                enteringPhase(currentRun.posterasurePhase)(sym.tpe))
          } else {
            js.Select(genExpr(qualifier),
                encodeFieldSym(sym))(toIRType(sym.tpe))
          }

        case Ident(name) =>
          val sym = tree.symbol
          if (!sym.hasPackageFlag) {
            if (sym.isModule) {
              assert(!sym.isPackageClass, "Cannot use package as value: " + tree)
              genLoadModule(sym)
            } else if (undefinedDefaultParams contains sym) {
              // This is a default parameter whose assignment was moved to
              // a local variable. Put a literal undefined param again
              js.UndefinedParam()(toIRType(sym.tpe))
            } else {
              js.VarRef(encodeLocalSym(sym))(toIRType(sym.tpe))
            }
          } else {
            sys.error("Cannot use package as value: " + tree)
          }

        case Literal(value) =>
          value.tag match {
            case UnitTag =>
              js.Skip()
            case BooleanTag =>
              js.BooleanLiteral(value.booleanValue)
            case ByteTag | ShortTag | CharTag | IntTag =>
              js.IntLiteral(value.intValue)
            case LongTag =>
              js.LongLiteral(value.longValue)
            case FloatTag =>
              js.FloatLiteral(value.floatValue)
            case DoubleTag =>
              js.DoubleLiteral(value.doubleValue)
            case StringTag =>
              js.StringLiteral(value.stringValue)
            case NullTag =>
              js.Null()
            case ClazzTag =>
              genClassConstant(value.typeValue)
            case EnumTag =>
              genStaticMember(value.symbolValue)
          }

        case tree: Block =>
          genBlock(tree, isStat)

        case Typed(Super(_, _), _) =>
          genThis()

        case Typed(expr, _) =>
          genExpr(expr)

        case Assign(lhs, rhs) =>
          val sym = lhs.symbol
          if (sym.isStaticMember)
            abort(s"Assignment to static member ${sym.fullName} not supported")
          val genRhs = genExpr(rhs)
          lhs match {
            case Select(qualifier, _) =>
              val ctorAssignment = (
                  currentMethodSym.isClassConstructor &&
                  currentMethodSym.owner == qualifier.symbol &&
                  qualifier.isInstanceOf[This]
              )
              if (!ctorAssignment && !suspectFieldMutable(sym))
                unexpectedMutatedFields += sym

              val genQual = genExpr(qualifier)

              if (isScalaJSDefinedJSClass(sym.owner)) {
                val genLhs = if (isExposed(sym))
                  js.JSBracketSelect(genQual, js.StringLiteral(jsNameOf(sym)))
                else
                  js.JSDotSelect(genQual, encodeFieldSym(sym))
                val boxedRhs =
                  ensureBoxed(genRhs,
                      enteringPhase(currentRun.posterasurePhase)(rhs.tpe))
                js.Assign(genLhs, boxedRhs)
              } else {
                js.Assign(
                    js.Select(genQual, encodeFieldSym(sym))(toIRType(sym.tpe)),
                    genRhs)
              }
            case _ =>
              mutatedLocalVars += sym
              js.Assign(
                  js.VarRef(encodeLocalSym(sym))(toIRType(sym.tpe)),
                  genRhs)
          }

        /** Array constructor */
        case av: ArrayValue =>
          genArrayValue(av)

        /** A Match reaching the backend is supposed to be optimized as a switch */
        case mtch: Match =>
          genMatch(mtch, isStat)

        /** Anonymous function (only with -Ydelambdafy:method) */
        case fun: Function =>
          genAnonFunction(fun)

        case EmptyTree =>
          js.Skip()

        case _ =>
          abort("Unexpected tree in genExpr: " +
              tree + "/" + tree.getClass + " at: " + tree.pos)
      }
    } // end of GenJSCode.genExpr()

    /** Gen JS this of the current class.
     *  Normally encoded straightforwardly as a JS this.
     *  But must be replaced by the tail-jump-this local variable if there
     *  is one.
     */
    private def genThis()(implicit pos: Position): js.Tree = {
      thisLocalVarIdent.fold[js.Tree] {
        if (tryingToGenMethodAsJSFunction) {
          throw new CancelGenMethodAsJSFunction(
              "Trying to generate `this` inside the body")
        }
        js.This()(currentClassType)
      } { thisLocalIdent =>
        js.VarRef(thisLocalIdent)(currentClassType)
      }
    }

    /** Gen JS code for LabelDef
     *  The only LabelDefs that can reach here are the desugaring of
     *  while and do..while loops. All other LabelDefs (for tail calls or
     *  matches) are caught upstream and transformed in ad hoc ways.
     *
     *  So here we recognize all the possible forms of trees that can result
     *  of while or do..while loops, and we reconstruct the loop for emission
     *  to JS.
     */
    def genLabelDef(tree: LabelDef): js.Tree = {
      implicit val pos = tree.pos
      val sym = tree.symbol

      tree match {
        // while (cond) { body }
        case LabelDef(lname, Nil,
            If(cond,
                Block(bodyStats, Apply(target @ Ident(lname2), Nil)),
                Literal(_))) if (target.symbol == sym) =>
          js.While(genExpr(cond), js.Block(bodyStats map genStat))

        // while (cond) { body }; result
        case LabelDef(lname, Nil,
            Block(List(
                If(cond,
                    Block(bodyStats, Apply(target @ Ident(lname2), Nil)),
                    Literal(_))),
                result)) if (target.symbol == sym) =>
          js.Block(
              js.While(genExpr(cond), js.Block(bodyStats map genStat)),
              genExpr(result))

        // while (true) { body }
        case LabelDef(lname, Nil,
            Block(bodyStats,
                Apply(target @ Ident(lname2), Nil))) if (target.symbol == sym) =>
          js.While(js.BooleanLiteral(true), js.Block(bodyStats map genStat))

        // while (false) { body }
        case LabelDef(lname, Nil, Literal(Constant(()))) =>
          js.Skip()

        // do { body } while (cond)
        case LabelDef(lname, Nil,
            Block(bodyStats,
                If(cond,
                    Apply(target @ Ident(lname2), Nil),
                    Literal(_)))) if (target.symbol == sym) =>
          js.DoWhile(js.Block(bodyStats map genStat), genExpr(cond))

        // do { body } while (cond); result
        case LabelDef(lname, Nil,
            Block(
                bodyStats :+
                If(cond,
                    Apply(target @ Ident(lname2), Nil),
                    Literal(_)),
                result)) if (target.symbol == sym) =>
          js.Block(
              js.DoWhile(js.Block(bodyStats map genStat), genExpr(cond)),
              genExpr(result))

        /* Arbitrary other label - we can jump to it from inside it.
         * This is typically for the label-defs implementing tail-calls.
         * It can also handle other weird LabelDefs generated by some compiler
         * plugins (see for example #1148).
         */
        case LabelDef(labelName, labelParams, rhs) =>
          val labelParamSyms = labelParams.map(_.symbol) map {
            s => if (s == fakeTailJumpParamRepl._1) fakeTailJumpParamRepl._2 else s
          }

          withScopedVars(
            enclosingLabelDefParams :=
              enclosingLabelDefParams.get + (tree.symbol -> labelParamSyms)
          ) {
            val bodyType = toIRType(tree.tpe)
            val labelIdent = encodeLabelSym(tree.symbol)
            val blockLabelIdent = freshLocalIdent()

            js.Labeled(blockLabelIdent, bodyType, {
              js.While(js.BooleanLiteral(true), {
                if (bodyType == jstpe.NoType)
                  js.Block(genStat(rhs), js.Return(js.Undefined(), Some(blockLabelIdent)))
                else
                  js.Return(genExpr(rhs), Some(blockLabelIdent))
              }, Some(labelIdent))
            })
          }
      }
    }

    /** Gen JS code for a try..catch or try..finally block
     *
     *  try..finally blocks are compiled straightforwardly to try..finally
     *  blocks of JS.
     *
     *  try..catch blocks are a bit more subtle, as JS does not have
     *  type-based selection of exceptions to catch. We thus encode explicitly
     *  the type tests, like in:
     *
     *  try { ... }
     *  catch (e) {
     *    if (e.isInstanceOf[IOException]) { ... }
     *    else if (e.isInstanceOf[Exception]) { ... }
     *    else {
     *      throw e; // default, re-throw
     *    }
     *  }
     */
    def genTry(tree: Try, isStat: Boolean): js.Tree = {
      implicit val pos = tree.pos
      val Try(block, catches, finalizer) = tree

      val blockAST = genStatOrExpr(block, isStat)

      val exceptIdent = freshLocalIdent("e")
      val origExceptVar = js.VarRef(exceptIdent)(jstpe.AnyType)

      val resultType = toIRType(tree.tpe)

      val handlerAST = {
        if (catches.isEmpty) {
          js.EmptyTree
        } else {
          val mightCatchJavaScriptException = catches.exists { caseDef =>
            caseDef.pat match {
              case Typed(Ident(nme.WILDCARD), tpt) =>
                isMaybeJavaScriptException(tpt.tpe)
              case Ident(nme.WILDCARD) =>
                true
              case pat @ Bind(_, _) =>
                isMaybeJavaScriptException(pat.symbol.tpe)
            }
          }

          val (exceptValDef, exceptVar) = if (mightCatchJavaScriptException) {
            val valDef = js.VarDef(freshLocalIdent("e"),
                encodeClassType(ThrowableClass), mutable = false, {
              genApplyMethod(
                  genLoadModule(RuntimePackageModule),
                  Runtime_wrapJavaScriptException,
                  List(origExceptVar))
            })
            (valDef, valDef.ref)
          } else {
            (js.Skip(), origExceptVar)
          }

          val elseHandler: js.Tree = js.Throw(origExceptVar)

          val handler0 = catches.foldRight(elseHandler) { (caseDef, elsep) =>
            implicit val pos = caseDef.pos
            val CaseDef(pat, _, body) = caseDef

            // Extract exception type and variable
            val (tpe, boundVar) = (pat match {
              case Typed(Ident(nme.WILDCARD), tpt) =>
                (tpt.tpe, None)
              case Ident(nme.WILDCARD) =>
                (ThrowableClass.tpe, None)
              case Bind(_, _) =>
                (pat.symbol.tpe, Some(encodeLocalSym(pat.symbol)))
            })

            // Generate the body that must be executed if the exception matches
            val bodyWithBoundVar = (boundVar match {
              case None =>
                genStatOrExpr(body, isStat)
              case Some(bv) =>
                val castException = genAsInstanceOf(exceptVar, tpe)
                js.Block(
                    js.VarDef(bv, toIRType(tpe), mutable = false, castException),
                    genStatOrExpr(body, isStat))
            })

            // Generate the test
            if (tpe == ThrowableClass.tpe) {
              bodyWithBoundVar
            } else {
              val cond = genIsInstanceOf(exceptVar, tpe)
              js.If(cond, bodyWithBoundVar, elsep)(resultType)
            }
          }

          js.Block(
              exceptValDef,
              handler0)
        }
      }

      val finalizerAST = genStat(finalizer) match {
        case js.Skip() => js.EmptyTree
        case ast       => ast
      }

      if (handlerAST == js.EmptyTree && finalizerAST == js.EmptyTree) blockAST
      else js.Try(blockAST, exceptIdent, handlerAST, finalizerAST)(resultType)
    }

    /** Gen JS code for an Apply node (method call)
     *
     *  There's a whole bunch of varieties of Apply nodes: regular method
     *  calls, super calls, constructor calls, isInstanceOf/asInstanceOf,
     *  primitives, JS calls, etc. They are further dispatched in here.
     */
    def genApply(tree: Apply, isStat: Boolean): js.Tree = {
      implicit val pos = tree.pos
      val Apply(fun, args) = tree
      val sym = fun.symbol

      def isRawJSDefaultParam: Boolean = {
        if (isCtorDefaultParam(sym)) {
          isRawJSCtorDefaultParam(sym)
        } else {
          sym.hasFlag(reflect.internal.Flags.DEFAULTPARAM) &&
          isRawJSType(sym.owner.tpe)
        }
      }

      fun match {
        case TypeApply(_, _) =>
          genApplyTypeApply(tree)

        case _ if isRawJSDefaultParam =>
          js.UndefinedParam()(toIRType(sym.tpe.resultType))

        case Select(Super(_, _), _) =>
          genSuperCall(tree, isStat)

        case Select(New(_), nme.CONSTRUCTOR) =>
          genApplyNew(tree)

        case _ =>
          if (sym.isLabel) {
            genLabelApply(tree)
          } else if (scalaPrimitives.isPrimitive(sym)) {
            genPrimitiveOp(tree, isStat)
          } else if (currentRun.runDefinitions.isBox(sym)) {
            // Box a primitive value (cannot be Unit)
            val arg = args.head
            makePrimitiveBox(genExpr(arg), arg.tpe)
          } else if (currentRun.runDefinitions.isUnbox(sym)) {
            // Unbox a primitive value (cannot be Unit)
            val arg = args.head
            makePrimitiveUnbox(genExpr(arg), tree.tpe)
          } else {
            genNormalApply(tree, isStat)
          }
      }
    }

    /** Gen an Apply with a TypeApply method.
     *  Only isInstanceOf and asInstanceOf keep their type argument until the
     *  backend.
     */
    private def genApplyTypeApply(tree: Apply): js.Tree = {
      implicit val pos = tree.pos
      val Apply(TypeApply(fun @ Select(obj, _), targs), _) = tree
      val sym = fun.symbol

      val cast = sym match {
        case Object_isInstanceOf => false
        case Object_asInstanceOf => true
        case _ =>
          abort("Unexpected type application " + fun +
              "[sym: " + sym.fullName + "]" + " in: " + tree)
      }

      val to = targs.head.tpe
      val l = toTypeKind(obj.tpe)
      val r = toTypeKind(to)
      val source = genExpr(obj)

      if (l.isValueType && r.isValueType) {
        if (cast)
          genConversion(l, r, source)
        else
          js.BooleanLiteral(l == r)
      } else if (l.isValueType) {
        val result = if (cast) {
          val ctor = ClassCastExceptionClass.info.member(
              nme.CONSTRUCTOR).suchThat(_.tpe.params.isEmpty)
          js.Throw(genNew(ClassCastExceptionClass, ctor, Nil))
        } else {
          js.BooleanLiteral(false)
        }
        js.Block(source, result) // eval and discard source
      } else if (r.isValueType) {
        assert(!cast, s"Unexpected asInstanceOf from ref type to value type")
        genIsInstanceOf(source, boxedClass(to.typeSymbol).tpe)
      } else {
        if (cast)
          genAsInstanceOf(source, to)
        else
          genIsInstanceOf(source, to)
      }
    }

    /** Gen JS code for a super call, of the form Class.super[mix].fun(args).
     *
     *  This does not include calls defined in mixin traits, as these are
     *  already desugared by the 'mixin' phase. Only calls to super classes
     *  remain.
     *  Since a class has exactly one direct superclass, and calling a method
     *  two classes above the current one is invalid, the `mix` item is
     *  irrelevant.
     */
    private def genSuperCall(tree: Apply, isStat: Boolean): js.Tree = {
      implicit val pos = tree.pos
      val Apply(fun @ Select(sup @ Super(_, mix), _), args) = tree
      val sym = fun.symbol

      if (sym == Object_getClass) {
        // The only primitive that is also callable as super call
        js.GetClass(genThis())
      } else if (isScalaJSDefinedJSClass(currentClassSym)) {
        genJSSuperCall(tree, isStat)
      } else {
        val superCall = genApplyMethodStatically(
            genThis()(sup.pos), sym, genActualArgs(sym, args))

        // Initialize the module instance just after the super constructor call.
        if (isStaticModule(currentClassSym) && !isModuleInitialized &&
            currentMethodSym.isClassConstructor) {
          isModuleInitialized = true
          val thisType = jstpe.ClassType(encodeClassFullName(currentClassSym))
          val initModule = js.StoreModule(thisType, js.This()(thisType))
          js.Block(superCall, initModule)
        } else {
          superCall
        }
      }
    }

    /** Gen JS code for a constructor call (new).
     *  Further refined into:
     *  * new String(...)
     *  * new of a hijacked boxed class
     *  * new of an anonymous function class that was recorded as JS function
     *  * new of a raw JS class
     *  * new Array
     *  * regular new
     */
    private def genApplyNew(tree: Apply): js.Tree = {
      implicit val pos = tree.pos
      val Apply(fun @ Select(New(tpt), nme.CONSTRUCTOR), args) = tree
      val ctor = fun.symbol
      val tpe = tpt.tpe

      assert(ctor.isClassConstructor,
          "'new' call to non-constructor: " + ctor.name)

      if (isStringType(tpe)) {
        genNewString(tree)
      } else if (isHijackedBoxedClass(tpe.typeSymbol)) {
        genNewHijackedBoxedClass(tpe.typeSymbol, ctor, args map genExpr)
      } else if (translatedAnonFunctions contains tpe.typeSymbol) {
        val functionMaker = translatedAnonFunctions(tpe.typeSymbol)
        functionMaker(args map genExpr)
      } else if (isRawJSType(tpe)) {
        genPrimitiveJSNew(tree)
      } else {
        toTypeKind(tpe) match {
          case arr @ ARRAY(elem) =>
            genNewArray(arr.toIRType, args map genExpr)
          case rt @ REFERENCE(cls) =>
            genNew(cls, ctor, genActualArgs(ctor, args))
          case generatedType =>
            abort(s"Non reference type cannot be instantiated: $generatedType")
        }
      }
    }

    /** Gen jump to a label.
     *  Most label-applys are caught upstream (while and do..while loops,
     *  jumps to next case of a pattern match), but some are still handled here:
     *  * Jumps to enclosing label-defs, including tail-recursive calls
     *  * Jump to the end of a pattern match
     */
    private def genLabelApply(tree: Apply): js.Tree = {
      implicit val pos = tree.pos
      val Apply(fun, args) = tree
      val sym = fun.symbol

      if (enclosingLabelDefParams.contains(sym)) {
        genEnclosingLabelApply(tree)
      } else if (sym.name.toString() startsWith "matchEnd") {
        /* Jump the to the end-label of a pattern match
         * Such labels have exactly one argument, which is the result of
         * the pattern match (of type BoxedUnit if the match is in statement
         * position). We simply `return` the argument as the result of the
         * labeled block surrounding the match.
         */
        countsOfReturnsToMatchEnd(sym) += 1
        js.Return(genExpr(args.head), Some(encodeLabelSym(sym)))
      } else {
        /* No other label apply should ever happen. If it does, then we
         * have missed a pattern of LabelDef/LabelApply and some new
         * translation must be found for it.
         */
        abort("Found unknown label apply at "+tree.pos+": "+tree)
      }
    }

    /** Gen a label-apply to an enclosing label def.
     *
     *  This is typically used for tail-recursive calls.
     *
     *  Basically this is compiled into
     *  continue labelDefIdent;
     *  but arguments need to be updated beforehand.
     *
     *  Since the rhs for the new value of an argument can depend on the value
     *  of another argument (and since deciding if it is indeed the case is
     *  impossible in general), new values are computed in temporary variables
     *  first, then copied to the actual variables representing the argument.
     *
     *  Trivial assignments (arg1 = arg1) are eliminated.
     *
     *  If, after elimination of trivial assignments, only one assignment
     *  remains, then we do not use a temporary variable for this one.
     */
    private def genEnclosingLabelApply(tree: Apply): js.Tree = {
      implicit val pos = tree.pos
      val Apply(fun, args) = tree
      val sym = fun.symbol

      // Prepare quadruplets of (formalArg, irType, tempVar, actualArg)
      // Do not include trivial assignments (when actualArg == formalArg)
      val formalArgs = enclosingLabelDefParams(sym)
      val actualArgs = args map genExpr
      val quadruplets = {
        for {
          (formalArgSym, actualArg) <- formalArgs zip actualArgs
          formalArg = encodeLocalSym(formalArgSym)
          if (actualArg match {
            case js.VarRef(`formalArg`) => false
            case _                      => true
          })
        } yield {
          mutatedLocalVars += formalArgSym
          val tpe = toIRType(formalArgSym.tpe)
          (js.VarRef(formalArg)(tpe), tpe,
              freshLocalIdent("temp$" + formalArg.name),
              actualArg)
        }
      }

      // The actual jump (continue labelDefIdent;)
      val jump = js.Continue(Some(encodeLabelSym(sym)))

      quadruplets match {
        case Nil => jump

        case (formalArg, argType, _, actualArg) :: Nil =>
          js.Block(
              js.Assign(formalArg, actualArg),
              jump)

        case _ =>
          val tempAssignments =
            for ((_, argType, tempArg, actualArg) <- quadruplets)
              yield js.VarDef(tempArg, argType, mutable = false, actualArg)
          val trueAssignments =
            for ((formalArg, argType, tempArg, _) <- quadruplets)
              yield js.Assign(formalArg, js.VarRef(tempArg)(argType))
          js.Block(tempAssignments ++ trueAssignments :+ jump)
      }
    }

    /** Gen a "normal" apply (to a true method).
     *
     *  But even these are further refined into:
     *  * Methods of java.lang.String, which are redirected to the
     *    RuntimeString trait implementation.
     *  * Calls to methods of raw JS types (Scala.js -> JS bridge)
     *  * Calls to methods in impl classes of traits.
     *  * Regular method call
     */
    private def genNormalApply(tree: Apply, isStat: Boolean): js.Tree = {
      implicit val pos = tree.pos
      val Apply(fun @ Select(receiver, _), args) = tree
      val sym = fun.symbol

      def isStringMethodFromObject: Boolean = sym.name match {
        case nme.toString_ | nme.equals_ | nme.hashCode_ => true
        case _                                           => false
      }

      if (sym.owner == StringClass && !isStringMethodFromObject) {
        genStringCall(tree)
      } else if (isRawJSType(receiver.tpe) && sym.owner != ObjectClass) {
        if (!isScalaJSDefinedJSClass(sym.owner) || isExposed(sym))
          genPrimitiveJSCall(tree, isStat)
        else
          genApplyJSClassMethod(genExpr(receiver), sym, genActualArgs(sym, args))
      } else if (foreignIsImplClass(sym.owner)) {
        genTraitImplApply(sym, args map genExpr)
      } else if (sym.isClassConstructor) {
        /* See #66: we have to emit a statically linked call to avoid calling a
         * constructor with the same signature in a subclass. */
        genApplyMethodStatically(genExpr(receiver), sym, genActualArgs(sym, args))
      } else {
        genApplyMethod(genExpr(receiver), sym, genActualArgs(sym, args))
      }
    }

    def genApplyMethodStatically(receiver: js.Tree, method: Symbol,
        arguments: List[js.Tree])(implicit pos: Position): js.Tree = {
      val className = encodeClassFullName(method.owner)
      val methodIdent = encodeMethodSym(method)
      val resultType =
        if (method.isClassConstructor) jstpe.NoType
        else toIRType(method.tpe.resultType)
      js.ApplyStatically(receiver, jstpe.ClassType(className),
          methodIdent, arguments)(resultType)
    }

    def genTraitImplApply(method: Symbol, arguments: List[js.Tree])(
        implicit pos: Position): js.Tree = {
      genApplyStatic(method, arguments)
    }

    def genApplyJSClassMethod(receiver: js.Tree, method: Symbol,
        arguments: List[js.Tree])(implicit pos: Position): js.Tree = {
      genApplyStatic(method, receiver :: arguments)
    }

    def genApplyStatic(method: Symbol, arguments: List[js.Tree])(
        implicit pos: Position): js.Tree = {
      val cls = encodeClassFullName(method.owner)
      val methodIdent = encodeMethodSym(method)
      genApplyStatic(cls, methodIdent, arguments,
          toIRType(method.tpe.resultType))
    }

    def genApplyStatic(cls: String, methodIdent: js.Ident,
        arguments: List[js.Tree], resultType: jstpe.Type)(
        implicit pos: Position): js.Tree = {
      js.ApplyStatic(jstpe.ClassType(cls), methodIdent,
          arguments)(resultType)
    }

    /** Gen JS code for a conversion between primitive value types */
    def genConversion(from: TypeKind, to: TypeKind, value: js.Tree)(
        implicit pos: Position): js.Tree = {
      def int0 = js.IntLiteral(0)
      def int1 = js.IntLiteral(1)
      def long0 = js.LongLiteral(0L)
      def long1 = js.LongLiteral(1L)
      def float0 = js.FloatLiteral(0.0f)
      def float1 = js.FloatLiteral(1.0f)

      // scalastyle:off disallow.space.before.token
      (from, to) match {
        case (INT(_),   BOOL) => js.BinaryOp(js.BinaryOp.Num_!=,  value, int0)
        case (LONG,     BOOL) => js.BinaryOp(js.BinaryOp.Long_!=, value, long0)
        case (FLOAT(_), BOOL) => js.BinaryOp(js.BinaryOp.Num_!=,  value, float0)

        case (BOOL, INT(_))   => js.If(value, int1,   int0  )(jstpe.IntType)
        case (BOOL, LONG)     => js.If(value, long1,  long0 )(jstpe.LongType)
        case (BOOL, FLOAT(_)) => js.If(value, float1, float0)(jstpe.FloatType)

        case _ => value
      }
      // scalastyle:on disallow.space.before.token
    }

    /** Gen JS code for an isInstanceOf test (for reference types only) */
    def genIsInstanceOf(value: js.Tree, to: Type)(
        implicit pos: Position): js.Tree = {

      val sym = to.typeSymbol

      if (sym == ObjectClass) {
        js.BinaryOp(js.BinaryOp.!==, value, js.Null())
      } else if (isRawJSType(to)) {
        if (sym.isTrait) {
          reporter.error(pos,
              s"isInstanceOf[${sym.fullName}] not supported because it is a raw JS trait")
          js.BooleanLiteral(true)
        } else {
          js.Unbox(js.JSBinaryOp(
              js.JSBinaryOp.instanceof, value, genPrimitiveJSClass(sym)), 'Z')
        }
      } else {
        js.IsInstanceOf(value, toReferenceType(to))
      }
    }

    /** Gen JS code for an asInstanceOf cast (for reference types only) */
    def genAsInstanceOf(value: js.Tree, to: Type)(
        implicit pos: Position): js.Tree = {

      def default: js.Tree =
        js.AsInstanceOf(value, toReferenceType(to))

      val sym = to.typeSymbol

      if (sym == ObjectClass || isRawJSType(to)) {
        /* asInstanceOf[Object] always succeeds, and
         * asInstanceOf to a raw JS type is completely erased.
         */
        value
      } else if (FunctionClass.seq contains to.typeSymbol) {
        /* Don't hide a JSFunctionToScala inside a useless cast, otherwise
         * the optimization avoiding double-wrapping in genApply() will not
         * be able to kick in.
         */
        value match {
          case JSFunctionToScala(fun, _) => value
          case _                         => default
        }
      } else {
        default
      }
    }

    /** Gen JS code for a call to a Scala method.
     *  This also registers that the given method is called by the current
     *  method in the method info builder.
     */
    def genApplyMethod(receiver: js.Tree,
        methodSym: Symbol, arguments: List[js.Tree])(
        implicit pos: Position): js.Tree = {
      genApplyMethod(receiver, encodeMethodSym(methodSym),
          arguments, toIRType(methodSym.tpe.resultType))
    }

    /** Gen JS code for a call to a Scala method.
     *  This also registers that the given method is called by the current
     *  method in the method info builder.
     */
    def genApplyMethod(receiver: js.Tree, methodIdent: js.Ident,
        arguments: List[js.Tree], resultType: jstpe.Type)(
        implicit pos: Position): js.Tree = {
      js.Apply(receiver, methodIdent, arguments)(resultType)
    }

    /** Gen JS code for a call to a Scala class constructor.
     *
     *  This also registers that the given class is instantiated by the current
     *  method, and that the given constructor is called, in the method info
     *  builder.
     */
    def genNew(clazz: Symbol, ctor: Symbol, arguments: List[js.Tree])(
        implicit pos: Position): js.Tree = {
      if (clazz.isAnonymousFunction)
        instantiatedAnonFunctions += clazz
      assert(!isRawJSFunctionDef(clazz),
          s"Trying to instantiate a raw JS function def $clazz")
      val className = encodeClassFullName(clazz)
      val ctorIdent = encodeMethodSym(ctor)
      js.New(jstpe.ClassType(className), ctorIdent, arguments)
    }

    /** Gen JS code for a call to a constructor of a hijacked boxed class.
     *  All of these have 2 constructors: one with the primitive
     *  value, which is erased, and one with a String, which is
     *  equivalent to BoxedClass.valueOf(arg).
     */
    private def genNewHijackedBoxedClass(clazz: Symbol, ctor: Symbol,
        arguments: List[js.Tree])(implicit pos: Position): js.Tree = {
      assert(arguments.size == 1)
      if (isStringType(ctor.tpe.params.head.tpe)) {
        // BoxedClass.valueOf(arg)
        val companion = clazz.companionModule.moduleClass
        val valueOf = getMemberMethod(companion, nme.valueOf) suchThat { s =>
          s.tpe.params.size == 1 && isStringType(s.tpe.params.head.tpe)
        }
        genApplyMethod(genLoadModule(companion), valueOf, arguments)
      } else {
        // erased
        arguments.head
      }
    }

    /** Gen JS code for creating a new Array: new Array[T](length)
     *  For multidimensional arrays (dimensions > 1), the arguments can
     *  specify up to `dimensions` lengths for the first dimensions of the
     *  array.
     */
    def genNewArray(arrayType: jstpe.ArrayType, arguments: List[js.Tree])(
        implicit pos: Position): js.Tree = {
      assert(arguments.length <= arrayType.dimensions,
          "too many arguments for array constructor: found " + arguments.length +
          " but array has only " + arrayType.dimensions + " dimension(s)")

      js.NewArray(arrayType, arguments)
    }

    /** Gen JS code for an array literal.
     */
    def genArrayValue(tree: Tree): js.Tree = {
      implicit val pos = tree.pos
      val ArrayValue(tpt @ TypeTree(), elems) = tree

      val arrType = toReferenceType(tree.tpe).asInstanceOf[jstpe.ArrayType]
      js.ArrayValue(arrType, elems map genExpr)
    }

    /** Gen JS code for a Match, i.e., a switch-able pattern match
     *  Eventually, this is compiled into a JS switch construct. But because
     *  we can be in expression position, and a JS switch cannot be given a
     *  meaning in expression position, we emit a JS "match" construct (which
     *  does not need the `break`s in each case. `JSDesugaring` will transform
     *  that in a switch.
     *
     *  Some caveat here. It may happen that there is a guard in here, despite
     *  the fact that switches cannot have guards (in the JVM nor in JS).
     *  The JVM backend emits a jump to the default clause when a guard is not
     *  fulfilled. We cannot do that. Instead, currently we duplicate the body
     *  of the default case in the else branch of the guard test.
     */
    def genMatch(tree: Tree, isStat: Boolean): js.Tree = {
      implicit val pos = tree.pos
      val Match(selector, cases) = tree

      val expr = genExpr(selector)
      val resultType = toIRType(tree.tpe)

      val List(defaultBody0) = for {
        CaseDef(Ident(nme.WILDCARD), EmptyTree, body) <- cases
      } yield body

      val (defaultBody, defaultLabelSym) = defaultBody0 match {
        case LabelDef(_, Nil, rhs) if hasSynthCaseSymbol(defaultBody0) =>
          (rhs, defaultBody0.symbol)
        case _ =>
          (defaultBody0, NoSymbol)
      }

      val genDefaultBody = genStatOrExpr(defaultBody, isStat)

      var clauses: List[(List[js.Literal], js.Tree)] = Nil
      var elseClause: js.Tree = js.EmptyTree

      for (caze @ CaseDef(pat, guard, body) <- cases) {
        assert(guard == EmptyTree)

        def genBody(body: Tree): js.Tree = body match {
          // Yes, this will duplicate the default body in the output
          case app @ Apply(_, Nil) if app.symbol == defaultLabelSym =>
            genDefaultBody
          case Block(List(app @ Apply(_, Nil)), _) if app.symbol == defaultLabelSym =>
            genDefaultBody

          case If(cond, thenp, elsep) =>
            js.If(genExpr(cond), genBody(thenp), genBody(elsep))(
                resultType)(body.pos)

          /* For #1955. If we receive a tree with the shape
           *   if (cond) {
           *     thenp
           *   } else {
           *     elsep
           *   }
           *   scala.runtime.BoxedUnit.UNIT
           * we rewrite it as
           *   if (cond) {
           *     thenp
           *     scala.runtime.BoxedUnit.UNIT
           *   } else {
           *     elsep
           *     scala.runtime.BoxedUnit.UNIT
           *   }
           * so that it fits the shape of if/elses we can deal with.
           */
          case Block(List(If(cond, thenp, elsep)), s: Select)
              if s.symbol == definitions.BoxedUnit_UNIT =>
            val newThenp = Block(thenp, s).setType(s.tpe).setPos(thenp.pos)
            val newElsep = Block(elsep, s).setType(s.tpe).setPos(elsep.pos)
            js.If(genExpr(cond), genBody(newThenp), genBody(newElsep))(
                resultType)(body.pos)

          case _ =>
            genStatOrExpr(body, isStat)
        }

        def genLiteral(lit: Literal): js.Literal =
          genExpr(lit).asInstanceOf[js.Literal]

        pat match {
          case lit: Literal =>
            clauses = (List(genLiteral(lit)), genBody(body)) :: clauses
          case Ident(nme.WILDCARD) =>
            elseClause = genDefaultBody
          case Alternative(alts) =>
            val genAlts = {
              alts map {
                case lit: Literal => genLiteral(lit)
                case _ =>
                  abort("Invalid case in alternative in switch-like pattern match: " +
                      tree + " at: " + tree.pos)
              }
            }
            clauses = (genAlts, genBody(body)) :: clauses
          case _ =>
            abort("Invalid case statement in switch-like pattern match: " +
                tree + " at: " + (tree.pos))
        }
      }

      js.Match(expr, clauses.reverse, elseClause)(resultType)
    }

    private def genBlock(tree: Block, isStat: Boolean): js.Tree = {
      implicit val pos = tree.pos
      val Block(stats, expr) = tree

      /** Predicate satisfied by LabelDefs produced by the pattern matcher */
      def isCaseLabelDef(tree: Tree) =
        tree.isInstanceOf[LabelDef] && hasSynthCaseSymbol(tree)

      def translateMatch(expr: LabelDef) = {
        /* Block that appeared as the result of a translated match
         * Such blocks are recognized by having at least one element that is
         * a so-called case-label-def.
         * The method `genTranslatedMatch()` takes care of compiling the
         * actual match.
         *
         * The assumption is once we encounter a case, the remainder of the
         * block will consist of cases.
         * The prologue may be empty, usually it is the valdef that stores
         * the scrut.
         */
        val (prologue, cases) = stats.span(s => !isCaseLabelDef(s))
        assert(cases.forall(isCaseLabelDef),
            "Assumption on the form of translated matches broken: " + tree)

        val genPrologue = prologue map genStat
        val translatedMatch =
          genTranslatedMatch(cases.map(_.asInstanceOf[LabelDef]), expr)

        js.Block(genPrologue :+ translatedMatch)
      }

      expr match {
        case expr: LabelDef if isCaseLabelDef(expr) =>
          translateMatch(expr)

        // Sometimes the pattern matcher casts its final result
        case Apply(TypeApply(Select(expr: LabelDef, nme.asInstanceOf_Ob), _), _)
            if isCaseLabelDef(expr) =>
          translateMatch(expr)

        case _ =>
          assert(!stats.exists(isCaseLabelDef), "Found stats with case label " +
              s"def in non-match block at ${tree.pos}: $tree")

          /* Normal block */
          val statements = stats map genStat
          val expression = genStatOrExpr(expr, isStat)
          js.Block(statements :+ expression)
      }
    }

    /** Gen JS code for a translated match
     *
     *  This implementation relies heavily on the patterns of trees emitted
     *  by the current pattern match phase (as of Scala 2.10).
     *
     *  The trees output by the pattern matcher are assumed to follow these
     *  rules:
     *  * Each case LabelDef (in `cases`) must not take any argument.
     *  * The last one must be a catch-all (case _ =>) that never falls through.
     *  * Jumps to the `matchEnd` are allowed anywhere in the body of the
     *    corresponding case label-defs, but not outside.
     *  * Jumps to case label-defs are restricted to jumping to the very next
     *    case, and only in positions denoted by <jump> in:
     *    <case-body> ::=
     *        If(_, <case-body>, <case-body>)
     *      | Block(_, <case-body>)
     *      | <jump>
     *      | _
     *    These restrictions, together with the fact that we are in statement
     *    position (thanks to the above transformation), mean that they can be
     *    simply replaced by `skip`.
     *
     *  To implement jumps to `matchEnd`, which have one argument which is the
     *  result of the match, we enclose all the cases in one big labeled block.
     *  Jumps are then compiled as `return`s out of the block.
     */
    def genTranslatedMatch(cases: List[LabelDef],
        matchEnd: LabelDef)(implicit pos: Position): js.Tree = {

      val matchEndSym = matchEnd.symbol
      countsOfReturnsToMatchEnd(matchEndSym) = 0

      val nextCaseSyms = (cases.tail map (_.symbol)) :+ NoSymbol

      val translatedCases = for {
        (LabelDef(_, Nil, rhs), nextCaseSym) <- cases zip nextCaseSyms
      } yield {
        def genCaseBody(tree: Tree): js.Tree = {
          implicit val pos = tree.pos
          tree match {
            case If(cond, thenp, elsep) =>
              js.If(genExpr(cond), genCaseBody(thenp), genCaseBody(elsep))(
                  jstpe.NoType)

            case Block(stats, expr) =>
              js.Block((stats map genStat) :+ genCaseBody(expr))

            case Apply(_, Nil) if tree.symbol == nextCaseSym =>
              js.Skip()

            case _ =>
              genStat(tree)
          }
        }

        genCaseBody(rhs)
      }

      val returnCount = countsOfReturnsToMatchEnd.remove(matchEndSym).get

      genOptimizedLabeled(encodeLabelSym(matchEndSym), toIRType(matchEnd.tpe),
          translatedCases, returnCount)
    }

    /** Gen JS code for a Labeled block from a pattern match, while trying
     *  to optimize it away as an If chain.
     *
     *  It is important to do so at compile-time because, when successful, the
     *  resulting IR can be much better optimized by the optimizer.
     *
     *  The optimizer also does something similar, but *after* it has processed
     *  the body of the Labeled block, at which point it has already lost any
     *  information about stack-allocated values.
     *
     *  !!! There is quite of bit of code duplication with
     *      OptimizerCore.tryOptimizePatternMatch.
     */
    def genOptimizedLabeled(label: js.Ident, tpe: jstpe.Type,
        translatedCases: List[js.Tree], returnCount: Int)(
        implicit pos: Position): js.Tree = {
      def default =
        js.Labeled(label, tpe, js.Block(translatedCases))

      @tailrec
      def createRevAlts(xs: List[js.Tree],
          acc: List[(js.Tree, js.Tree)]): List[(js.Tree, js.Tree)] = xs match {
        case js.If(cond, body, js.Skip()) :: xr =>
          createRevAlts(xr, (cond, body) :: acc)
        case remaining =>
          (js.EmptyTree, js.Block(remaining)(remaining.head.pos)) :: acc
      }
      val revAlts = createRevAlts(translatedCases, Nil)

      if (revAlts.size == returnCount) {
        @tailrec
        def constructOptimized(revAlts: List[(js.Tree, js.Tree)],
            elsep: js.Tree): js.Tree = {
          revAlts match {
            case (cond, body) :: revAltsRest =>
              body match {
                case jse.BlockOrAlone(prep,
                    js.Return(result, Some(`label`))) =>
                  val prepAndResult = js.Block(prep :+ result)(body.pos)
                  if (cond == js.EmptyTree) {
                    assert(elsep == js.EmptyTree)
                    constructOptimized(revAltsRest, prepAndResult)
                  } else {
                    assert(elsep != js.EmptyTree)
                    constructOptimized(revAltsRest,
                        js.If(cond, prepAndResult, elsep)(tpe)(cond.pos))
                  }
                case _ =>
                  default
              }
            case Nil =>
              elsep
          }
        }
        constructOptimized(revAlts, js.EmptyTree)
      } else {
        default
      }
    }

    /** Gen JS code for a primitive method call */
    private def genPrimitiveOp(tree: Apply, isStat: Boolean): js.Tree = {
      import scalaPrimitives._

      implicit val pos = tree.pos

      val sym = tree.symbol
      val Apply(fun @ Select(receiver, _), args) = tree

      val code = scalaPrimitives.getPrimitive(sym, receiver.tpe)

      if (isArithmeticOp(code) || isLogicalOp(code) || isComparisonOp(code))
        genSimpleOp(tree, receiver :: args, code)
      else if (code == scalaPrimitives.CONCAT)
        genStringConcat(tree, receiver, args)
      else if (code == HASH)
        genScalaHash(tree, receiver)
      else if (isArrayOp(code))
        genArrayOp(tree, code)
      else if (code == SYNCHRONIZED)
        genSynchronized(tree, isStat)
      else if (isCoercion(code))
        genCoercion(tree, receiver, code)
      else if (jsPrimitives.isJavaScriptPrimitive(code))
        genJSPrimitive(tree, receiver, args, code)
      else
        abort("Unknown primitive operation: " + sym.fullName + "(" +
            fun.symbol.simpleName + ") " + " at: " + (tree.pos))
    }

    /** Gen JS code for a simple operation (arithmetic, logical, or comparison) */
    private def genSimpleOp(tree: Apply, args: List[Tree], code: Int): js.Tree = {
      import scalaPrimitives._

      implicit val pos = tree.pos

      def isLongOp(ltpe: Type, rtpe: Type) =
        (isLongType(ltpe) || isLongType(rtpe)) &&
        !(toTypeKind(ltpe).isInstanceOf[FLOAT] ||
          toTypeKind(rtpe).isInstanceOf[FLOAT] ||
          isStringType(ltpe) || isStringType(rtpe))

      val sources = args map genExpr

      val resultType = toIRType(tree.tpe)

      sources match {
        // Unary operation
        case List(source) =>
          (code match {
            case POS =>
              source
            case NEG =>
              (resultType: @unchecked) match {
                case jstpe.IntType =>
                  js.BinaryOp(js.BinaryOp.Int_-, js.IntLiteral(0), source)
                case jstpe.LongType =>
                  js.BinaryOp(js.BinaryOp.Long_-, js.LongLiteral(0), source)
                case jstpe.FloatType =>
                  js.BinaryOp(js.BinaryOp.Float_-, js.FloatLiteral(0.0f), source)
                case jstpe.DoubleType =>
                  js.BinaryOp(js.BinaryOp.Double_-, js.DoubleLiteral(0), source)
              }
            case NOT =>
              (resultType: @unchecked) match {
                case jstpe.IntType =>
                  js.BinaryOp(js.BinaryOp.Int_^, js.IntLiteral(-1), source)
                case jstpe.LongType =>
                  js.BinaryOp(js.BinaryOp.Long_^, js.LongLiteral(-1), source)
              }
            case ZNOT =>
              js.UnaryOp(js.UnaryOp.Boolean_!, source)
            case _ =>
              abort("Unknown unary operation code: " + code)
          })

        // Binary operation on Longs
        case List(lsrc, rsrc) if isLongOp(args(0).tpe, args(1).tpe) =>
          def toLong(tree: js.Tree, tpe: Type) =
            if (isLongType(tpe)) tree
            else js.UnaryOp(js.UnaryOp.IntToLong, tree)

          def toInt(tree: js.Tree, tpe: Type) =
            if (isLongType(tpe)) js.UnaryOp(js.UnaryOp.LongToInt, rsrc)
            else tree

          val ltree = toLong(lsrc, args(0).tpe)
          def rtree = toLong(rsrc, args(1).tpe)
          def rtreeInt = toInt(rsrc, args(1).tpe)

          import js.BinaryOp._
          (code: @switch) match {
            case ADD => js.BinaryOp(Long_+,   ltree, rtree)
            case SUB => js.BinaryOp(Long_-,   ltree, rtree)
            case MUL => js.BinaryOp(Long_*,   ltree, rtree)
            case DIV => js.BinaryOp(Long_/,   ltree, rtree)
            case MOD => js.BinaryOp(Long_%,   ltree, rtree)
            case OR  => js.BinaryOp(Long_|,   ltree, rtree)
            case XOR => js.BinaryOp(Long_^,   ltree, rtree)
            case AND => js.BinaryOp(Long_&,   ltree, rtree)
            case LSL => js.BinaryOp(Long_<<,  ltree, rtreeInt)
            case LSR => js.BinaryOp(Long_>>>, ltree, rtreeInt)
            case ASR => js.BinaryOp(Long_>>,  ltree, rtreeInt)
            case EQ  => js.BinaryOp(Long_==,  ltree, rtree)
            case NE  => js.BinaryOp(Long_!=,  ltree, rtree)
            case LT  => js.BinaryOp(Long_<,   ltree, rtree)
            case LE  => js.BinaryOp(Long_<=,  ltree, rtree)
            case GT  => js.BinaryOp(Long_>,   ltree, rtree)
            case GE  => js.BinaryOp(Long_>=,  ltree, rtree)
            case _ =>
              abort("Unknown binary operation code: " + code)
          }

        // Binary operation
        case List(lsrc_in, rsrc_in) =>
          val leftKind = toTypeKind(args(0).tpe)
          val rightKind = toTypeKind(args(1).tpe)

          val opType = (leftKind, rightKind) match {
            case (DoubleKind, _) | (_, DoubleKind) => jstpe.DoubleType
            case (FloatKind, _) | (_, FloatKind)   => jstpe.FloatType
            case (INT(_), _) | (_, INT(_))         => jstpe.IntType
            case (BooleanKind, BooleanKind)        => jstpe.BooleanType
            case _                                 => jstpe.AnyType
          }

          def convertArg(tree: js.Tree, kind: TypeKind) = {
            /* If we end up with a long, the op type must be float or double,
             * so we can first eliminate the Long case by converting to Double.
             */
            val notLong =
              if (kind == LongKind) js.UnaryOp(js.UnaryOp.LongToDouble, tree)
              else tree

            if (opType != jstpe.FloatType) notLong
            else if (kind == FloatKind) notLong
            else js.UnaryOp(js.UnaryOp.DoubleToFloat, notLong)
          }

          val lsrc = convertArg(lsrc_in, leftKind)
          val rsrc = convertArg(rsrc_in, rightKind)

          def genEquality(eqeq: Boolean, not: Boolean) = {
            opType match {
              case jstpe.IntType | jstpe.DoubleType | jstpe.FloatType =>
                js.BinaryOp(
                    if (not) js.BinaryOp.Num_!= else js.BinaryOp.Num_==,
                    lsrc, rsrc)
              case jstpe.BooleanType =>
                js.BinaryOp(
                    if (not) js.BinaryOp.Boolean_!= else js.BinaryOp.Boolean_==,
                    lsrc, rsrc)
              case _ =>
                if (eqeq &&
                    // don't call equals if we have a literal null at either side
                    !lsrc.isInstanceOf[js.Null] &&
                    !rsrc.isInstanceOf[js.Null] &&
                    // Arrays, Null, Nothing do not have an equals() method
                    leftKind.isInstanceOf[REFERENCE]) {
                  val body = genEqEqPrimitive(args(0).tpe, args(1).tpe, lsrc, rsrc)
                  if (not) js.UnaryOp(js.UnaryOp.Boolean_!, body) else body
                } else {
                  js.BinaryOp(
                      if (not) js.BinaryOp.!== else js.BinaryOp.===,
                      lsrc, rsrc)
                }
            }
          }

          (code: @switch) match {
            case EQ => genEquality(eqeq = true, not = false)
            case NE => genEquality(eqeq = true, not = true)
            case ID => genEquality(eqeq = false, not = false)
            case NI => genEquality(eqeq = false, not = true)

            case ZOR  => js.If(lsrc, js.BooleanLiteral(true), rsrc)(jstpe.BooleanType)
            case ZAND => js.If(lsrc, rsrc, js.BooleanLiteral(false))(jstpe.BooleanType)

            case _ =>
              import js.BinaryOp._
              val op = (resultType: @unchecked) match {
                case jstpe.IntType =>
                  (code: @switch) match {
                    case ADD => Int_+
                    case SUB => Int_-
                    case MUL => Int_*
                    case DIV => Int_/
                    case MOD => Int_%
                    case OR  => Int_|
                    case AND => Int_&
                    case XOR => Int_^
                    case LSL => Int_<<
                    case LSR => Int_>>>
                    case ASR => Int_>>
                  }
                case jstpe.FloatType =>
                  (code: @switch) match {
                    case ADD => Float_+
                    case SUB => Float_-
                    case MUL => Float_*
                    case DIV => Float_/
                    case MOD => Float_%
                  }
                case jstpe.DoubleType =>
                  (code: @switch) match {
                    case ADD => Double_+
                    case SUB => Double_-
                    case MUL => Double_*
                    case DIV => Double_/
                    case MOD => Double_%
                  }
                case jstpe.BooleanType =>
                  (code: @switch) match {
                    case LT   => Num_<
                    case LE   => Num_<=
                    case GT   => Num_>
                    case GE   => Num_>=
                    case OR   => Boolean_|
                    case AND  => Boolean_&
                    case XOR  => Boolean_!=
                  }
              }
              js.BinaryOp(op, lsrc, rsrc)
          }

        case _ =>
          abort("Too many arguments for primitive function: " + tree)
      }
    }

    /** Gen JS code for a call to Any.== */
    def genEqEqPrimitive(ltpe: Type, rtpe: Type, lsrc: js.Tree, rsrc: js.Tree)(
        implicit pos: Position): js.Tree = {
      /* True if the equality comparison is between values that require the
       * use of the rich equality comparator
       * (scala.runtime.BoxesRunTime.equals).
       * This is the case when either side of the comparison might have a
       * run-time type subtype of java.lang.Number or java.lang.Character,
       * **which includes when either is a raw JS type**.
       * When it is statically known that both sides are equal and subtypes of
       * Number or Character, not using the rich equality is possible (their
       * own equals method will do ok.)
       */
      val mustUseAnyComparator: Boolean = isRawJSType(ltpe) || isRawJSType(rtpe) || {
        val areSameFinals = ltpe.isFinalType && rtpe.isFinalType && (ltpe =:= rtpe)
        !areSameFinals && isMaybeBoxed(ltpe.typeSymbol) && isMaybeBoxed(rtpe.typeSymbol)
      }

      if (mustUseAnyComparator) {
        val equalsMethod: Symbol = {
          // scalastyle:off line.size.limit
          val ptfm = platform.asInstanceOf[backend.JavaPlatform with ThisPlatform] // 2.10 compat
          if (ltpe <:< BoxedNumberClass.tpe) {
            if (rtpe <:< BoxedNumberClass.tpe) ptfm.externalEqualsNumNum
            else if (rtpe <:< BoxedCharacterClass.tpe) ptfm.externalEqualsNumObject // will be externalEqualsNumChar in 2.12, SI-9030
            else ptfm.externalEqualsNumObject
          } else ptfm.externalEquals
          // scalastyle:on line.size.limit
        }
        val moduleClass = equalsMethod.owner
        val instance = genLoadModule(moduleClass)
        genApplyMethod(instance, equalsMethod, List(lsrc, rsrc))
      } else {
        // if (lsrc eq null) rsrc eq null else lsrc.equals(rsrc)
        if (isStringType(ltpe)) {
          // String.equals(that) === (this eq that)
          js.BinaryOp(js.BinaryOp.===, lsrc, rsrc)
        } else {
          /* This requires to evaluate both operands in local values first.
           * The optimizer will eliminate them if possible.
           */
          val ltemp = js.VarDef(freshLocalIdent(), lsrc.tpe, mutable = false, lsrc)
          val rtemp = js.VarDef(freshLocalIdent(), rsrc.tpe, mutable = false, rsrc)
          js.Block(
              ltemp,
              rtemp,
              js.If(js.BinaryOp(js.BinaryOp.===, ltemp.ref, js.Null()),
                  js.BinaryOp(js.BinaryOp.===, rtemp.ref, js.Null()),
                  genApplyMethod(ltemp.ref, Object_equals, List(rtemp.ref)))(
                  jstpe.BooleanType))
        }
      }
    }

    /** Gen JS code for string concatenation.
     */
    private def genStringConcat(tree: Apply, receiver: Tree,
        args: List[Tree]): js.Tree = {
      implicit val pos = tree.pos

      /* Primitive number types such as scala.Int have a
       *   def +(s: String): String
       * method, which is why we have to box the lhs sometimes.
       * Otherwise, both lhs and rhs are already reference types (Any of String)
       * so boxing is not necessary (in particular, rhs is never a primitive).
       */
      assert(!isPrimitiveValueType(receiver.tpe) || isStringType(args.head.tpe))
      assert(!isPrimitiveValueType(args.head.tpe))

      val rhs = genExpr(args.head)

      val lhs = {
        val lhs0 = genExpr(receiver)
        // Box the receiver if it is a primitive value
        if (!isPrimitiveValueType(receiver.tpe)) lhs0
        else makePrimitiveBox(lhs0, receiver.tpe)
      }

      js.BinaryOp(js.BinaryOp.String_+, lhs, rhs)
    }

    /** Gen JS code for a call to Any.## */
    private def genScalaHash(tree: Apply, receiver: Tree): js.Tree = {
      implicit val pos = tree.pos

      val instance = genLoadModule(ScalaRunTimeModule)
      val arguments = List(genExpr(receiver))
      val sym = getMember(ScalaRunTimeModule, nme.hash_)

      genApplyMethod(instance, sym, arguments)
    }

    /** Gen JS code for an array operation (get, set or length) */
    private def genArrayOp(tree: Tree, code: Int): js.Tree = {
      import scalaPrimitives._

      implicit val pos = tree.pos

      val Apply(Select(arrayObj, _), args) = tree
      val arrayValue = genExpr(arrayObj)
      val arguments = args map genExpr

      def genSelect() = {
        val elemIRType =
          toTypeKind(arrayObj.tpe).asInstanceOf[ARRAY].elem.toIRType
        js.ArraySelect(arrayValue, arguments(0))(elemIRType)
      }

      if (scalaPrimitives.isArrayGet(code)) {
        // get an item of the array
        assert(args.length == 1,
            s"Array get requires 1 argument, found ${args.length} in $tree")
        genSelect()
      } else if (scalaPrimitives.isArraySet(code)) {
        // set an item of the array
        assert(args.length == 2,
            s"Array set requires 2 arguments, found ${args.length} in $tree")
        js.Assign(genSelect(), arguments(1))
      } else {
        // length of the array
        js.ArrayLength(arrayValue)
      }
    }

    /** Gen JS code for a call to AnyRef.synchronized */
    private def genSynchronized(tree: Apply, isStat: Boolean): js.Tree = {
      /* JavaScript is single-threaded, so we can drop the
       * synchronization altogether.
       */
      val Apply(Select(receiver, _), List(arg)) = tree
      val newReceiver = genExpr(receiver)
      val newArg = genStatOrExpr(arg, isStat)
      newReceiver match {
        case js.This() =>
          // common case for which there is no side-effect nor NPE
          newArg
        case _ =>
          implicit val pos = tree.pos
          val NPECtor = getMemberMethod(NullPointerExceptionClass,
              nme.CONSTRUCTOR).suchThat(_.tpe.params.isEmpty)
          js.Block(
              js.If(js.BinaryOp(js.BinaryOp.===, newReceiver, js.Null()),
                  js.Throw(genNew(NullPointerExceptionClass, NPECtor, Nil)),
                  js.Skip())(jstpe.NoType),
              newArg)
      }
    }

    /** Gen JS code for a coercion */
    private def genCoercion(tree: Apply, receiver: Tree, code: Int): js.Tree = {
      import scalaPrimitives._

      implicit val pos = tree.pos

      val source = genExpr(receiver)

      def source2int = (code: @switch) match {
        case F2C | D2C | F2B | D2B | F2S | D2S | F2I | D2I =>
          js.UnaryOp(js.UnaryOp.DoubleToInt, source)
        case L2C | L2B | L2S | L2I =>
          js.UnaryOp(js.UnaryOp.LongToInt, source)
        case _ =>
          source
      }

      (code: @switch) match {
        // To Char, need to crop at unsigned 16-bit
        case B2C | S2C | I2C | L2C | F2C | D2C =>
          js.BinaryOp(js.BinaryOp.Int_&, source2int, js.IntLiteral(0xffff))

        // To Byte, need to crop at signed 8-bit
        case C2B | S2B | I2B | L2B | F2B | D2B =>
          // note: & 0xff would not work because of negative values
          js.BinaryOp(js.BinaryOp.Int_>>,
              js.BinaryOp(js.BinaryOp.Int_<<, source2int, js.IntLiteral(24)),
              js.IntLiteral(24))

        // To Short, need to crop at signed 16-bit
        case C2S | I2S | L2S | F2S | D2S =>
          // note: & 0xffff would not work because of negative values
          js.BinaryOp(js.BinaryOp.Int_>>,
              js.BinaryOp(js.BinaryOp.Int_<<, source2int, js.IntLiteral(16)),
              js.IntLiteral(16))

        // To Int, need to crop at signed 32-bit
        case L2I | F2I | D2I =>
          source2int

        // Any int to Long
        case C2L | B2L | S2L | I2L =>
          js.UnaryOp(js.UnaryOp.IntToLong, source)

        // Any double to Long
        case F2L | D2L =>
          js.UnaryOp(js.UnaryOp.DoubleToLong, source)

        // Long to Double
        case L2D =>
          js.UnaryOp(js.UnaryOp.LongToDouble, source)

        // Any int, or Double, to Float
        case C2F | B2F | S2F | I2F | D2F =>
          js.UnaryOp(js.UnaryOp.DoubleToFloat, source)

        // Long to Float === Long to Double to Float
        case L2F =>
          js.UnaryOp(js.UnaryOp.DoubleToFloat,
              js.UnaryOp(js.UnaryOp.LongToDouble, source))

        // Identities and IR upcasts
        case C2C | B2B | S2S | I2I | L2L | F2F | D2D |
             C2I | C2D |
             B2S | B2I | B2D |
             S2I | S2D |
             I2D |
             F2D =>
          source
      }
    }

    /** Gen JS code for an ApplyDynamic
     *  ApplyDynamic nodes appear as the result of calls to methods of a
     *  structural type.
     *
     *  Most unfortunately, earlier phases of the compiler assume too much
     *  about the backend, namely, they believe arguments and the result must
     *  be boxed, and do the boxing themselves. This decision should be left
     *  to the backend, but it's not, so we have to undo these boxes.
     *  Note that this applies to parameter types only. The return type is boxed
     *  anyway since we do not know it's exact type.
     *
     *  This then generates a call to the reflective call proxy for the given
     *  arguments.
     */
    private def genApplyDynamic(tree: ApplyDynamic): js.Tree = {
      implicit val pos = tree.pos

      val sym = tree.symbol
      val params = sym.tpe.params

      /** check if the method we are invoking is eq or ne. they cannot be
       *  overridden since they are final. If this is true, we only emit a
       *  `===` or `!==`.
       */
      val isEqOrNeq = (sym.name == nme.eq || sym.name == nme.ne) &&
        params.size == 1 && params.head.tpe.typeSymbol == ObjectClass

      /** check if the method we are invoking conforms to a method on
       *  scala.Array. If this is the case, we check that case specially at
       *  runtime to avoid having reflective call proxies on scala.Array.
       *  (Also, note that the element type of Array#update is not erased and
       *  therefore the method name mangling would turn out wrong)
       *
       *  Note that we cannot check if the expected return type is correct,
       *  since this type information is already erased.
       */
      def isArrayLikeOp = {
        sym.name == nme.update &&
          params.size == 2 && params.head.tpe.typeSymbol == IntClass ||
        sym.name == nme.apply &&
          params.size == 1 && params.head.tpe.typeSymbol == IntClass ||
        sym.name == nme.length &&
          params.size == 0 ||
        sym.name == nme.clone_ &&
          params.size == 0
      }

      /**
       * Tests whether one of our reflective "boxes" for primitive types
       * implements the particular method. If this is the case
       * (result != NoSymbol), we generate a runtime instance check if we are
       * dealing with the appropriate primitive type.
       */
      def matchingSymIn(clazz: Symbol) = clazz.tpe.member(sym.name).suchThat { s =>
        val sParams = s.tpe.params
        !s.isBridge &&
        params.size == sParams.size &&
        (params zip sParams).forall { case (s1,s2) =>
          s1.tpe =:= s2.tpe
        }
      }

      val ApplyDynamic(receiver, args) = tree

      if (isEqOrNeq) {
        // Just emit a boxed equality check
        val jsThis = genExpr(receiver)
        val jsThat = genExpr(args.head)
        val op = if (sym.name == nme.eq) js.BinaryOp.=== else js.BinaryOp.!==
        ensureBoxed(js.BinaryOp(op, jsThis, jsThat), BooleanClass.tpe)
      } else {
        // Create a fully-fledged reflective call
        val receiverType = toIRType(receiver.tpe)
        val callTrgIdent = freshLocalIdent()
        val callTrgVarDef =
          js.VarDef(callTrgIdent, receiverType, mutable = false, genExpr(receiver))
        val callTrg = js.VarRef(callTrgIdent)(receiverType)

        val arguments = args zip sym.tpe.params map { case (arg, param) =>
          /* No need for enteringPosterasure, because value classes are not
           * supported as parameters of methods in structural types.
           * We could do it for safety and future-proofing anyway, except that
           * I am weary of calling enteringPosterasure for a reflective method
           * symbol.
           *
           * Note also that this will typically unbox a primitive value that
           * has just been boxed, or will .asInstanceOf[T] an expression which
           * is already of type T. But the optimizer will get rid of that, and
           * reflective calls are not numerous, so we don't complicate the
           * compiler to eliminate them early.
           */
          fromAny(genExpr(arg), param.tpe)
        }

        val proxyIdent = encodeMethodSym(sym, reflProxy = true)
        var callStatement: js.Tree =
          genApplyMethod(callTrg, proxyIdent, arguments, jstpe.AnyType)

        if (isArrayLikeOp) {
          def genRTCall(method: Symbol, args: js.Tree*) =
            genApplyMethod(genLoadModule(ScalaRunTimeModule),
                method, args.toList)
          val isArrayTree =
            genRTCall(ScalaRunTime_isArray, callTrg, js.IntLiteral(1))
          callStatement = js.If(isArrayTree, {
            sym.name match {
              case nme.update =>
                js.Block(
                    genRTCall(currentRun.runDefinitions.arrayUpdateMethod,
                        callTrg, arguments(0), arguments(1)),
                    js.Undefined()) // Boxed Unit
              case nme.apply =>
                genRTCall(currentRun.runDefinitions.arrayApplyMethod, callTrg,
                    arguments(0))
              case nme.length =>
                genRTCall(currentRun.runDefinitions.arrayLengthMethod, callTrg)
              case nme.clone_ =>
                genApplyMethod(callTrg, Object_clone, arguments)
            }
          }, {
            callStatement
          })(jstpe.AnyType)
        }

        for {
          (rtClass, reflBoxClass) <- Seq(
              (StringClass, StringClass),
              (BoxedDoubleClass, NumberReflectiveCallClass),
              (BoxedBooleanClass, BooleanReflectiveCallClass),
              (BoxedLongClass, LongReflectiveCallClass)
          )
          implMethodSym = matchingSymIn(reflBoxClass)
          if implMethodSym != NoSymbol && implMethodSym.isPublic
        } {
          callStatement = js.If(genIsInstanceOf(callTrg, rtClass.tpe), {
            if (implMethodSym.owner == ObjectClass) {
              // If the method is defined on Object, we can call it normally.
              genApplyMethod(callTrg, implMethodSym, arguments)
            } else {
              if (rtClass == StringClass) {
                val (rtModuleClass, methodIdent) =
                  encodeRTStringMethodSym(implMethodSym)
                val retTpe = implMethodSym.tpe.resultType
                val castCallTrg = fromAny(callTrg, StringClass.toTypeConstructor)
                val rawApply = genApplyMethod(
                    genLoadModule(rtModuleClass),
                    methodIdent,
                    castCallTrg :: arguments,
                    toIRType(retTpe))
                // Box the result of the implementing method if required
                if (isPrimitiveValueType(retTpe))
                  makePrimitiveBox(rawApply, retTpe)
                else
                  rawApply
              } else {
                val reflBoxClassPatched = {
                  def isIntOrLongKind(kind: TypeKind) = kind match {
                    case _:INT | LONG => true
                    case _            => false
                  }
                  if (rtClass == BoxedDoubleClass &&
                      toTypeKind(implMethodSym.tpe.resultType) == DoubleKind &&
                      isIntOrLongKind(toTypeKind(sym.tpe.resultType))) {
                    // This must be an Int, and not a Double
                    IntegerReflectiveCallClass
                  } else {
                    reflBoxClass
                  }
                }
                val castCallTrg =
                  fromAny(callTrg,
                      reflBoxClassPatched.primaryConstructor.tpe.params.head.tpe)
                val reflBox = genNew(reflBoxClassPatched,
                    reflBoxClassPatched.primaryConstructor, List(castCallTrg))
                genApplyMethod(
                    reflBox,
                    proxyIdent,
                    arguments,
                    jstpe.AnyType)
              }
            }
          }, { // else
            callStatement
          })(jstpe.AnyType)
        }

        js.Block(callTrgVarDef, callStatement)
      }
    }

    /** Ensures that the value of the given tree is boxed.
     *  @param expr Tree to be boxed if needed.
     *  @param tpeEnteringPosterasure The type of `expr` as it was entering
     *    the posterasure phase.
     */
    def ensureBoxed(expr: js.Tree, tpeEnteringPosterasure: Type)(
        implicit pos: Position): js.Tree = {

      tpeEnteringPosterasure match {
        case tpe if isPrimitiveValueType(tpe) =>
          makePrimitiveBox(expr, tpe)

        case tpe: ErasedValueType =>
          val boxedClass = tpe.valueClazz
          val ctor = boxedClass.primaryConstructor
          genNew(boxedClass, ctor, List(expr))

        case _ =>
          expr
      }
    }

    /** Extracts a value typed as Any to the given type after posterasure.
     *  @param expr Tree to be extracted.
     *  @param tpeEnteringPosterasure The type of `expr` as it was entering
     *    the posterasure phase.
     */
    def fromAny(expr: js.Tree, tpeEnteringPosterasure: Type)(
        implicit pos: Position): js.Tree = {

      tpeEnteringPosterasure match {
        case tpe if isPrimitiveValueType(tpe) =>
          makePrimitiveUnbox(expr, tpe)

        case tpe: ErasedValueType =>
          val boxedClass = tpe.valueClazz
          val unboxMethod = boxedClass.derivedValueClassUnbox
          val content = genApplyMethod(
              genAsInstanceOf(expr, tpe), unboxMethod, Nil)
          if (unboxMethod.tpe.resultType <:< tpe.erasedUnderlying)
            content
          else
            fromAny(content, tpe.erasedUnderlying)

        case tpe =>
          genAsInstanceOf(expr, tpe)
      }
    }

    /** Gen a boxing operation (tpe is the primitive type) */
    def makePrimitiveBox(expr: js.Tree, tpe: Type)(
        implicit pos: Position): js.Tree = {
      toTypeKind(tpe) match {
        case VOID => // must be handled at least for JS interop
          js.Block(expr, js.Undefined())
        case kind: ValueTypeKind =>
          if (kind == CharKind) {
            genApplyMethod(
                genLoadModule(BoxesRunTimeClass),
                BoxesRunTime_boxToCharacter,
                List(expr))
          } else {
            expr // box is identity for all non-Char types
          }
        case _ =>
          abort(s"makePrimitiveBox requires a primitive type, found $tpe at $pos")
      }
    }

    /** Gen an unboxing operation (tpe is the primitive type) */
    def makePrimitiveUnbox(expr: js.Tree, tpe: Type)(
        implicit pos: Position): js.Tree = {
      toTypeKind(tpe) match {
        case VOID => // must be handled at least for JS interop
          expr
        case kind: ValueTypeKind =>
          if (kind == CharKind) {
            genApplyMethod(
                genLoadModule(BoxesRunTimeClass),
                BoxesRunTime_unboxToChar,
                List(expr))
          } else {
            js.Unbox(expr, kind.primitiveCharCode)
          }
        case _ =>
          abort(s"makePrimitiveUnbox requires a primitive type, found $tpe at $pos")
      }
    }

    private def lookupModuleClass(name: String) = {
      val module = getModuleIfDefined(name)
      if (module == NoSymbol) NoSymbol
      else module.moduleClass
    }

    lazy val ReflectArrayModuleClass = lookupModuleClass("java.lang.reflect.Array")
    lazy val UtilArraysModuleClass = lookupModuleClass("java.util.Arrays")

    /** Gen JS code for a Scala.js-specific primitive method */
    private def genJSPrimitive(tree: Apply, receiver0: Tree,
        args: List[Tree], code: Int): js.Tree = {
      import jsPrimitives._

      implicit val pos = tree.pos

      def receiver = genExpr(receiver0)
      def genArgs = genPrimitiveJSArgs(tree.symbol, args)

      if (code == DYNNEW) {
        // js.Dynamic.newInstance(clazz)(actualArgs:_*)
        val (jsClass, actualArgs) = extractFirstArg(genArgs)
        js.JSNew(jsClass, actualArgs)
      } else if (code == DYNLIT) {
        /* We have a call of the form:
         *   js.Dynamic.literal(name1 = arg1, name2 = arg2, ...)
         * or
         *   js.Dynamic.literal(name1 -> arg1, name2 -> arg2, ...)
         * or in general
         *   js.Dynamic.literal(tup1, tup2, ...)
         *
         * Translate to:
         *   var obj = {};
         *   obj[name1] = arg1;
         *   obj[name2] = arg2;
         *   ...
         *   obj
         * or, if possible, to:
         *   {name1: arg1, name2: arg2, ... }
         */

        def warnIfDuplicatedKey(pairs: List[(js.StringLiteral, js.Tree)]): Unit = {
          val allKeys = pairs.collect { case (js.StringLiteral(keyName), _) => keyName }
          val keyCounts = allKeys.distinct.map(key => key -> allKeys.count(_ == key))
          val duplicateKeyCounts = keyCounts.filter(1 < _._2)
          if (duplicateKeyCounts.nonEmpty) {
            reporter.warning(pos,
                "Duplicate keys in object literal: " +
                duplicateKeyCounts.map {
                  case (keyName, count) => s""""$keyName" defined $count times"""
                }.mkString(", ") +
                ". Only the last occurrence is assigned."
            )
          }
        }

        // Extract first arg to future proof against varargs
        extractFirstArg(genArgs) match {
          // case js.Dynamic.literal("name1" -> ..., "name2" -> ...)
          case (js.StringLiteral("apply"), jse.LitNamed(pairs)) =>
            warnIfDuplicatedKey(pairs)
            js.JSObjectConstr(pairs)

          /* case js.Dynamic.literal(x: _*)
           * Even though scalac does not support this notation, it is still
           * possible to write its expansion by hand:
           * js.Dynamic.literal.applyDynamic("apply")(x: _*)
           */
          case (js.StringLiteral("apply"), tups)
              if tups.exists(_.isInstanceOf[js.JSSpread]) =>
            // Delegate to a runtime method
            val tupsArray = tups match {
              case List(js.JSSpread(tupsArray)) => tupsArray
              case _                            => js.JSArrayConstr(tups)
            }
            genApplyMethod(
                genLoadModule(RuntimePackageModule),
                Runtime_jsTupleArray2jsObject,
                List(tupsArray))

          // case js.Dynamic.literal(x, y)
          case (js.StringLiteral("apply"), tups) =>
            // Check for duplicated explicit keys
            val pairs = jse.LitNamedExtractor.extractFrom(tups)
            warnIfDuplicatedKey(pairs)

            // Create tmp variable
            val resIdent = freshLocalIdent("obj")
            val resVarDef = js.VarDef(resIdent, jstpe.AnyType, mutable = false,
                js.JSObjectConstr(Nil))
            val res = resVarDef.ref

            // Assign fields
            val tuple2Type = encodeClassType(TupleClass(2))
            val assigns = tups flatMap {
              // special case for literals
              case jse.Tuple2(name, value) =>
                js.Assign(js.JSBracketSelect(res, name), value) :: Nil
              case tupExpr =>
                val tupIdent = freshLocalIdent("tup")
                val tup = js.VarRef(tupIdent)(tuple2Type)
                js.VarDef(tupIdent, tuple2Type, mutable = false, tupExpr) ::
                js.Assign(js.JSBracketSelect(res,
                    genApplyMethod(tup, js.Ident("$$und1__O"), Nil, jstpe.AnyType)),
                    genApplyMethod(tup, js.Ident("$$und2__O"), Nil, jstpe.AnyType)) :: Nil
            }

            js.Block(resVarDef +: assigns :+ res: _*)

          // case where another method is called
          case (js.StringLiteral(name), _) if name != "apply" =>
            reporter.error(pos,
                s"js.Dynamic.literal does not have a method named $name")
            js.Undefined()
          case _ =>
            reporter.error(pos,
                s"js.Dynamic.literal.${tree.symbol.name} may not be called directly")
            js.Undefined()
        }
      } else if (code == ARR_CREATE) {
        // js.Array.create(elements: _*)
        js.JSArrayConstr(genArgs)
      } else if (code == CONSTRUCTOROF) {
        def fail() = {
          reporter.error(pos,
              "runtime.constructorOf() must be called with a constant " +
              "classOf[T] representing a class extending js.Any " +
              "(not a trait nor an object)")
          js.Undefined()
        }
        args match {
          case List(Literal(value)) if value.tag == ClazzTag =>
            val kind = toTypeKind(value.typeValue)
            kind match {
              case REFERENCE(classSym) if isRawJSType(classSym.tpe) &&
                  !classSym.isTrait && !classSym.isModuleClass =>
                genPrimitiveJSClass(classSym)
              case _ =>
                fail()
            }
          case _ =>
            fail()
        }
      } else (genArgs match {
        case Nil =>
          code match {
            case GETCLASS     => js.GetClass(receiver)
            case ENV_INFO     => js.JSEnvInfo()
            case LINKING_INFO => js.JSLinkingInfo()
            case DEBUGGER     => js.Debugger()
            case UNITVAL      => js.Undefined()
            case UNITTYPE     => genClassConstant(UnitTpe)
            case JS_NATIVE    =>
              reporter.error(pos, "js.native may only be used as stub implementation in facade types")
              js.Undefined()
          }

        case List(arg) =>

          /** Factorization of F2JS and F2JSTHIS. */
          def genFunctionToJSFunction(isThisFunction: Boolean): js.Tree = {
            val arity = {
              val funName = tree.fun.symbol.name.encoded
              assert(funName.startsWith("fromFunction"))
              funName.stripPrefix("fromFunction").toInt
            }
            val inputClass = FunctionClass(arity)
            val inputIRType = encodeClassType(inputClass)
            val applyMeth = getMemberMethod(inputClass, nme.apply) suchThat { s =>
              val ps = s.paramss
              ps.size == 1 &&
              ps.head.size == arity &&
              ps.head.forall(_.tpe.typeSymbol == ObjectClass)
            }
            val fCaptureParam = js.ParamDef(js.Ident("f"), inputIRType,
                mutable = false, rest = false)
            val jsArity =
              if (isThisFunction) arity - 1
              else arity
            val jsParams = (1 to jsArity).toList map {
              x => js.ParamDef(js.Ident("arg"+x), jstpe.AnyType,
                  mutable = false, rest = false)
            }
            js.Closure(
                List(fCaptureParam),
                jsParams,
                genApplyMethod(
                    fCaptureParam.ref,
                    applyMeth,
                    if (isThisFunction)
                      js.This()(jstpe.AnyType) :: jsParams.map(_.ref)
                    else
                      jsParams.map(_.ref)),
                List(arg))
          }

          code match {
            /** Convert a scala.FunctionN f to a js.FunctionN. */
            case F2JS =>
              arg match {
                /* This case will happen every time we have a Scala lambda
                 * in js.FunctionN position. We remove the JS function to
                 * Scala function wrapper, instead of adding a Scala function
                 * to JS function wrapper.
                 */
                case JSFunctionToScala(fun, arity) =>
                  fun
                case _ =>
                  genFunctionToJSFunction(isThisFunction = false)
              }

            /** Convert a scala.FunctionN f to a js.ThisFunction{N-1}. */
            case F2JSTHIS =>
              genFunctionToJSFunction(isThisFunction = true)

            case DICT_DEL =>
              // js.Dictionary.delete(arg)
              js.JSDelete(js.JSBracketSelect(receiver, arg))

            case TYPEOF =>
              // js.typeOf(arg)
              genAsInstanceOf(js.JSUnaryOp(js.JSUnaryOp.typeof, arg),
                  StringClass.tpe)

            case OBJPROPS =>
              // js.Object.properties(arg)
              genApplyMethod(
                  genLoadModule(RuntimePackageModule),
                  Runtime_propertiesOf,
                  List(arg))
          }

        case List(arg1, arg2) =>
          code match {
            case HASPROP =>
              // js.Object.hasProperty(arg1, arg2)
              /* Here we have an issue with evaluation order of arg1 and arg2,
               * since the obvious translation is `arg2 in arg1`, but then
               * arg2 is evaluated before arg1. Since this is not a commonly
               * used operator, we don't try to avoid unnessary temp vars, and
               * simply always evaluate arg1 in a temp before doing the `in`.
               */
              val temp = freshLocalIdent()
              js.Block(
                  js.VarDef(temp, jstpe.AnyType, mutable = false, arg1),
                  js.Unbox(js.JSBinaryOp(js.JSBinaryOp.in, arg2,
                      js.VarRef(temp)(jstpe.AnyType)), 'Z'))
          }
      })
    }

    /** Gen JS code for a primitive JS call (to a method of a subclass of js.Any)
     *  This is the typed Scala.js to JS bridge feature. Basically it boils
     *  down to calling the method without name mangling. But other aspects
     *  come into play:
     *  * Operator methods are translated to JS operators (not method calls)
     *  * apply is translated as a function call, i.e. o() instead of o.apply()
     *  * Scala varargs are turned into JS varargs (see genPrimitiveJSArgs())
     *  * Getters and parameterless methods are translated as Selects
     *  * Setters are translated to Assigns of Selects
     */
    private def genPrimitiveJSCall(tree: Apply, isStat: Boolean): js.Tree = {
      implicit val pos = tree.pos

      val sym = tree.symbol
      val Apply(fun @ Select(receiver0, _), args0) = tree

      val receiver = genExpr(receiver0)
      val args = genPrimitiveJSArgs(sym, args0)

      genJSCallGeneric(sym, receiver, args, isStat)
    }

    private def genJSSuperCall(tree: Apply, isStat: Boolean): js.Tree = {
      implicit val pos = tree.pos
      val Apply(fun @ Select(sup @ Super(_, _), _), args) = tree
      val sym = fun.symbol

      val genReceiver = genThis()(sup.pos)
      lazy val genScalaArgs = genActualArgs(sym, args)
      lazy val genJSArgs = genPrimitiveJSArgs(sym, args)

      if (sym.owner == ObjectClass) {
        // Normal call anyway
        assert(!sym.isClassConstructor,
            "Trying to call the super constructor of Object in a " +
            s"Scala.js-defined JS class at $pos")
        genApplyMethod(genReceiver, sym, genScalaArgs)
      } else if (sym.isClassConstructor) {
        js.JSSuperConstructorCall(genJSArgs)
      } else if (isScalaJSDefinedJSClass(sym.owner) && !isExposed(sym)) {
        // Reroute to the static method
        genApplyJSClassMethod(genReceiver, sym, genScalaArgs)
      } else {
        genJSCallGeneric(sym, genReceiver, genJSArgs, isStat,
            superIn = Some(currentClassSym))
      }
    }

    private def genJSCallGeneric(sym: Symbol, receiver: js.Tree,
        args: List[js.Tree], isStat: Boolean, superIn: Option[Symbol] = None)(
        implicit pos: Position): js.Tree = {
      def noSpread = !args.exists(_.isInstanceOf[js.JSSpread])
      val argc = args.size // meaningful only for methods that don't have varargs

      def requireNotSuper(): Unit = {
        if (superIn.isDefined) {
          reporter.error(pos,
              "Illegal super call in Scala.js-defined JS class")
        }
      }

      def hasExplicitJSEncoding =
        sym.hasAnnotation(JSNameAnnotation) ||
        sym.hasAnnotation(JSBracketAccessAnnotation) ||
        sym.hasAnnotation(JSBracketCallAnnotation)

      val boxedResult = sym.name match {
        case JSUnaryOpMethodName(code) if argc == 0 =>
          requireNotSuper()
          js.JSUnaryOp(code, receiver)

        case JSBinaryOpMethodName(code) if argc == 1 =>
          requireNotSuper()
          js.JSBinaryOp(code, receiver, args.head)

        case nme.apply if sym.owner.isSubClass(JSThisFunctionClass) =>
          requireNotSuper()
          js.JSBracketMethodApply(receiver, js.StringLiteral("call"), args)

        case nme.apply if !hasExplicitJSEncoding =>
          requireNotSuper()
          js.JSFunctionApply(receiver, args)

        case _ =>
          def jsFunName = js.StringLiteral(jsNameOf(sym))

          def genSuperReference(propName: js.Tree): js.Tree = {
            superIn.fold[js.Tree] {
              js.JSBracketSelect(receiver, propName)
            } { superInSym =>
              js.JSSuperBracketSelect(
                  jstpe.ClassType(encodeClassFullName(superInSym)),
                  receiver, propName)
            }
          }

          def genSelectGet(propName: js.Tree): js.Tree =
            genSuperReference(propName)

          def genSelectSet(propName: js.Tree, value: js.Tree): js.Tree =
            js.Assign(genSuperReference(propName), value)

          def genCall(methodName: js.Tree, args: List[js.Tree]): js.Tree = {
            superIn.fold[js.Tree] {
              js.JSBracketMethodApply(
                  receiver, methodName, args)
            } { superInSym =>
              js.JSSuperBracketCall(
                  jstpe.ClassType(encodeClassFullName(superInSym)),
                  receiver, methodName, args)
            }
          }

          if (jsInterop.isJSGetter(sym)) {
            assert(noSpread && argc == 0)
            genSelectGet(jsFunName)
          } else if (jsInterop.isJSSetter(sym)) {
            assert(noSpread && argc == 1)
            genSelectSet(jsFunName, args.head)
          } else if (jsInterop.isJSBracketAccess(sym)) {
            assert(noSpread && (argc == 1 || argc == 2),
                s"@JSBracketAccess methods should have 1 or 2 non-varargs arguments")
            args match {
              case List(keyArg) =>
                genSelectGet(keyArg)
              case List(keyArg, valueArg) =>
                genSelectSet(keyArg, valueArg)
            }
          } else if (jsInterop.isJSBracketCall(sym)) {
            val (methodName, actualArgs) = extractFirstArg(args)
            genCall(methodName, actualArgs)
          } else {
            genCall(jsFunName, args)
          }
      }

      boxedResult match {
        case js.Assign(_, _) =>
          boxedResult
        case _ if isStat =>
          boxedResult
        case _ =>
          fromAny(boxedResult,
              enteringPhase(currentRun.posterasurePhase)(sym.tpe.resultType))
      }
    }

    private object JSUnaryOpMethodName {
      private val map = Map(
        nme.UNARY_+ -> js.JSUnaryOp.+,
        nme.UNARY_- -> js.JSUnaryOp.-,
        nme.UNARY_~ -> js.JSUnaryOp.~,
        nme.UNARY_! -> js.JSUnaryOp.!
      )

      def unapply(name: TermName): Option[js.JSUnaryOp.Code] =
        map.get(name)
    }

    private object JSBinaryOpMethodName {
      private val map = Map(
        nme.ADD -> js.JSBinaryOp.+,
        nme.SUB -> js.JSBinaryOp.-,
        nme.MUL -> js.JSBinaryOp.*,
        nme.DIV -> js.JSBinaryOp./,
        nme.MOD -> js.JSBinaryOp.%,

        nme.LSL -> js.JSBinaryOp.<<,
        nme.ASR -> js.JSBinaryOp.>>,
        nme.LSR -> js.JSBinaryOp.>>>,
        nme.OR  -> js.JSBinaryOp.|,
        nme.AND -> js.JSBinaryOp.&,
        nme.XOR -> js.JSBinaryOp.^,

        nme.LT -> js.JSBinaryOp.<,
        nme.LE -> js.JSBinaryOp.<=,
        nme.GT -> js.JSBinaryOp.>,
        nme.GE -> js.JSBinaryOp.>=,

        nme.ZAND -> js.JSBinaryOp.&&,
        nme.ZOR  -> js.JSBinaryOp.||
      )

      def unapply(name: TermName): Option[js.JSBinaryOp.Code] =
        map.get(name)
    }

    /** Extract the first argument to a primitive JS call.
     *  This is nothing else than decomposing into head and tail, except that
     *  we assert that the first element is not a JSSpread.
     */
    private def extractFirstArg(args: List[js.Tree]): (js.Tree, List[js.Tree]) = {
      assert(args.nonEmpty,
          "Trying to extract the first argument of an empty argument list")
      val firstArg = args.head
      assert(!firstArg.isInstanceOf[js.JSSpread],
          "Trying to extract the first argument of an argument list starting " +
          "with a Spread argument: " + firstArg)
      (firstArg, args.tail)
    }

    /** Gen JS code for new java.lang.String(...)
     *  Proxies calls to method newString on object
     *  scala.scalajs.runtime.RuntimeString with proper arguments
     */
    private def genNewString(tree: Apply): js.Tree = {
      implicit val pos = tree.pos
      val Apply(fun @ Select(_, _), args0) = tree

      val ctor = fun.symbol
      val args = args0 map genExpr

      // Filter members of target module for matching member
      val compMembers = for {
        mem <- RuntimeStringModule.tpe.members
        if mem.name == jsnme.newString && ctor.tpe.matches(mem.tpe)
      } yield mem

      if (compMembers.isEmpty) {
        reporter.error(pos,
            s"""Could not find implementation for constructor of java.lang.String
               |with type ${ctor.tpe}. Constructors on java.lang.String
               |are forwarded to the companion object of
               |scala.scalajs.runtime.RuntimeString""".stripMargin)
        js.Undefined()
      } else {
        assert(compMembers.size == 1,
            s"""For constructor with type ${ctor.tpe} on java.lang.String,
               |found multiple companion module members.""".stripMargin)

        // Emit call to companion object
        genApplyMethod(
            genLoadModule(RuntimeStringModule), compMembers.head, args)
      }
    }

    /** Gen JS code for calling a method on java.lang.String.
     *
     *  Forwards call on java.lang.String to the module
     *  scala.scalajs.runtime.RuntimeString.
     */
    private def genStringCall(tree: Apply): js.Tree = {
      implicit val pos = tree.pos

      val sym = tree.symbol

      // Deconstruct tree and create receiver and argument JS expressions
      val Apply(Select(receiver0, _), args0) = tree
      val receiver = genExpr(receiver0)
      val args = args0 map genExpr

      // Emit call to the RuntimeString module
      val (rtModuleClass, methodIdent) = encodeRTStringMethodSym(sym)
      genApplyMethod(
          genLoadModule(rtModuleClass),
          methodIdent,
          receiver :: args,
          toIRType(tree.tpe))
    }

    /** Gen JS code for a new of a raw JS class (subclass of js.Any) */
    private def genPrimitiveJSNew(tree: Apply): js.Tree = {
      implicit val pos = tree.pos

      val Apply(fun @ Select(New(tpt), _), args0) = tree
      val cls = tpt.tpe.typeSymbol
      val ctor = fun.symbol

      val args = genPrimitiveJSArgs(ctor, args0)

      if (cls == JSObjectClass && args.isEmpty) js.JSObjectConstr(Nil)
      else if (cls == JSArrayClass && args.isEmpty) js.JSArrayConstr(Nil)
      else js.JSNew(genPrimitiveJSClass(cls), args)
    }

    /** Gen JS code representing a JS class (subclass of js.Any) */
    private def genPrimitiveJSClass(sym: Symbol)(
        implicit pos: Position): js.Tree = {
      assert(!isStaticModule(sym) && !sym.isInterface,
          s"genPrimitiveJSClass called with non-class $sym")
      js.LoadJSConstructor(jstpe.ClassType(encodeClassFullName(sym)))
    }

    /** Gen JS code representing a JS module (var of the global scope) */
    private def genPrimitiveJSModule(sym: Symbol)(
        implicit pos: Position): js.Tree = {
      assert(sym.isModuleClass,
          s"genPrimitiveJSModule called with non-module $sym")
      fullJSNameOf(sym).split('.').foldLeft(genLoadGlobal()) { (memo, chunk) =>
        js.JSBracketSelect(memo, js.StringLiteral(chunk))
      }
    }

    /** Gen actual actual arguments to Scala method call.
     *  Returns a list of the transformed arguments.
     *
     *  This tries to optimize repeated arguments (varargs) by turning them
     *  into js.WrappedArray instead of Scala wrapped arrays.
     */
    private def genActualArgs(sym: Symbol, args: List[Tree])(
        implicit pos: Position): List[js.Tree] = {
      val wereRepeated = exitingPhase(currentRun.typerPhase) {
        sym.tpe.params.map(p => isScalaRepeatedParamType(p.tpe))
      }

      if (wereRepeated.size > args.size) {
        // Should not happen, but let's not crash
        args.map(genExpr)
      } else {
        /* Arguments that are in excess compared to the type signature after
         * erasure are lambda-lifted arguments. They cannot be repeated, hence
         * the extension to `false`.
         */
        for ((arg, wasRepeated) <- args.zipAll(wereRepeated, EmptyTree, false)) yield {
          if (wasRepeated) {
            tryGenRepeatedParamAsJSArray(arg, handleNil = false).fold {
              genExpr(arg)
            } { genArgs =>
              genNew(WrappedArrayClass, WrappedArray_ctor,
                  List(js.JSArrayConstr(genArgs)))
            }
          } else {
            genExpr(arg)
          }
        }
      }
    }

    /** Gen actual actual arguments to a primitive JS call.
     *
     *  * Repeated arguments (varargs) are expanded
     *  * Default arguments are omitted or replaced by undefined
     *  * All arguments are boxed
     *
     *  Repeated arguments that cannot be expanded at compile time (i.e., if a
     *  Seq is passed to a varargs parameter with the syntax `seq: _*`) will be
     *  wrapped in a [[js.JSSpread]] node to be expanded at runtime.
     */
    private def genPrimitiveJSArgs(sym: Symbol, args: List[Tree])(
        implicit pos: Position): List[js.Tree] = {

      /* lambdalift might have to introduce some parameters when transforming
       * nested Scala.js-defined JS classes. Hence, the list of parameters
       * exiting typer and entering posterasure might not be compatible with
       * the list of actual arguments we receive now.
       *
       * We therefore need to establish of list of formal parameters based on
       * the current signature of `sym`, but have to look back in time to see
       * whether they were repeated and what was their type (for those that
       * were already present at the time).
       *
       * Unfortunately, for some reason lambdalift creates new symbol *even
       * for parameters originally in the signature* when doing so! That is
       * why we use the *names* of the parameters as a link through time,
       * rather than the symbols.
       *
       * This is pretty fragile, but fortunately we have a huge test suite to
       * back us up should scalac alter its behavior.
       */

      val wereRepeated = exitingPhase(currentRun.typerPhase) {
        for {
          params <- sym.tpe.paramss
          param <- params
        } yield {
          param.name -> isScalaRepeatedParamType(param.tpe)
        }
      }.toMap

      val paramTpes = enteringPhase(currentRun.posterasurePhase) {
        for (param <- sym.tpe.params)
          yield param.name -> param.tpe
      }.toMap

      var reversedArgs: List[js.Tree] = Nil

      for ((arg, paramSym) <- args zip sym.tpe.params) {
        val wasRepeated = wereRepeated.getOrElse(paramSym.name, false)
        if (wasRepeated) {
          reversedArgs =
            genPrimitiveJSRepeatedParam(arg) reverse_::: reversedArgs
        } else {
          val unboxedArg = genExpr(arg)
          val boxedArg = unboxedArg match {
            case js.UndefinedParam() =>
              unboxedArg
            case _ =>
              val tpe = paramTpes.getOrElse(paramSym.name, paramSym.tpe)
              ensureBoxed(unboxedArg, tpe)
          }
          reversedArgs ::= boxedArg
        }
      }

      /* Remove all consecutive js.UndefinedParam's at the end of the argument
       * list. No check is performed whether they may be there, since they will
       * only be placed where default arguments can be anyway.
       */
      reversedArgs = reversedArgs.dropWhile(_.isInstanceOf[js.UndefinedParam])

      // Find remaining js.UndefinedParam and replace by js.Undefined. This can
      // happen with named arguments or when multiple argument lists are present
      reversedArgs = reversedArgs map {
        case js.UndefinedParam() => js.Undefined()
        case arg                 => arg
      }

      reversedArgs.reverse
    }

    /** Gen JS code for a repeated param of a primitive JS method
     *  In this case `arg` has type Seq[T] for some T, but the result should
     *  be an expanded list of the elements in the sequence. So this method
     *  takes care of the conversion.
     *  It is specialized for the shapes of tree generated by the desugaring
     *  of repeated params in Scala, so that these are actually expanded at
     *  compile-time.
     *  Otherwise, it returns a JSSpread with the Seq converted to a js.Array.
     */
    private def genPrimitiveJSRepeatedParam(arg: Tree): List[js.Tree] = {
      tryGenRepeatedParamAsJSArray(arg, handleNil = true) getOrElse {
        /* Fall back to calling runtime.genTraversableOnce2jsArray
         * to perform the conversion to js.Array, then wrap in a Spread
         * operator.
         */
        implicit val pos = arg.pos
        val jsArrayArg = genApplyMethod(
            genLoadModule(RuntimePackageModule),
            Runtime_genTraversableOnce2jsArray,
            List(genExpr(arg)))
        List(js.JSSpread(jsArrayArg))
      }
    }

    /** Try and expand a repeated param (xs: T*) at compile-time.
     *  This method recognizes the shapes of tree generated by the desugaring
     *  of repeated params in Scala, and expands them.
     *  If `arg` does not have the shape of a generated repeated param, this
     *  method returns `None`.
     */
    private def tryGenRepeatedParamAsJSArray(arg: Tree,
        handleNil: Boolean): Option[List[js.Tree]] = {
      implicit val pos = arg.pos

      // Given a method `def foo(args: T*)`
      arg match {
        // foo(arg1, arg2, ..., argN) where N > 0
        case MaybeAsInstanceOf(WrapArray(
            MaybeAsInstanceOf(ArrayValue(tpt, elems)))) =>
          /* Value classes in arrays are already boxed, so no need to use
           * the type before erasure.
           */
          val elemTpe = tpt.tpe
          Some(elems.map(e => ensureBoxed(genExpr(e), elemTpe)))

        // foo()
        case Select(_, _) if handleNil && arg.symbol == NilModule =>
          Some(Nil)

        // foo(argSeq:_*) - cannot be optimized
        case _ =>
          None
      }
    }

    object MaybeAsInstanceOf {
      def unapply(tree: Tree): Some[Tree] = tree match {
        case Apply(TypeApply(asInstanceOf_? @ Select(base, _), _), _)
        if asInstanceOf_?.symbol == Object_asInstanceOf =>
          Some(base)
        case _ =>
          Some(tree)
      }
    }

    object WrapArray {
      lazy val isWrapArray: Set[Symbol] = Seq(
          nme.wrapRefArray,
          nme.wrapByteArray,
          nme.wrapShortArray,
          nme.wrapCharArray,
          nme.wrapIntArray,
          nme.wrapLongArray,
          nme.wrapFloatArray,
          nme.wrapDoubleArray,
          nme.wrapBooleanArray,
          nme.wrapUnitArray,
          nme.genericWrapArray).map(getMemberMethod(PredefModule, _)).toSet

      def unapply(tree: Apply): Option[Tree] = tree match {
        case Apply(wrapArray_?, List(wrapped))
        if isWrapArray(wrapArray_?.symbol) =>
          Some(wrapped)
        case _ =>
          None
      }
    }

    // Synthesizers for raw JS functions ---------------------------------------

    /** Try and gen and record JS code for an anonymous function class.
     *
     *  Returns true if the class could be rewritten that way, false otherwise.
     *
     *  We make the following assumptions on the form of such classes:
     *  - It is an anonymous function
     *    - Includes being anonymous, final, and having exactly one constructor
     *  - It is not a PartialFunction
     *  - It has no field other than param accessors
     *  - It has exactly one constructor
     *  - It has exactly one non-bridge method apply if it is not specialized,
     *    or a method apply$...$sp and a forwarder apply if it is specialized.
     *  - As a precaution: it is synthetic
     *
     *  From a class looking like this:
     *
     *    final class <anon>(outer, capture1, ..., captureM) extends AbstractionFunctionN[...] {
     *      def apply(param1, ..., paramN) = {
     *        <body>
     *      }
     *    }
     *    new <anon>(o, c1, ..., cM)
     *
     *  we generate a function maker that emits:
     *
     *    lambda<o, c1, ..., cM>[notype](
     *        outer, capture1, ..., captureM, param1, ..., paramN) {
     *      <body>
     *    }
     *
     *  so that, at instantiation point, we can write:
     *
     *    new AnonFunctionN(functionMaker(this, captured1, ..., capturedM))
     *
     *  Trickier things apply when the function is specialized.
     */
    private def tryGenAndRecordAnonFunctionClass(cd: ClassDef): Boolean = {
      // scalastyle:off return
      implicit val pos = cd.pos
      val sym = cd.symbol
      assert(sym.isAnonymousFunction,
          s"tryGenAndRecordAnonFunctionClass called with non-anonymous function $cd")

      withScopedVars(
          currentClassSym := sym
      ) {
        val (functionMakerBase, arity) =
          tryGenAndRecordAnonFunctionClassGeneric(cd) { msg =>
            return false
          }
        val functionMaker = { capturedArgs: List[js.Tree] =>
          JSFunctionToScala(functionMakerBase(capturedArgs), arity)
        }

        translatedAnonFunctions += sym -> functionMaker
      }
      true
      // scalastyle:on return
    }

    /** Constructor and extractor object for a tree that converts a JavaScript
     *  function into a Scala function.
     */
    private object JSFunctionToScala {
      private val AnonFunPrefScala =
        "scala.scalajs.runtime.AnonFunction"
      private val AnonFunPrefJS =
        "sjsr_AnonFunction"

      def apply(jsFunction: js.Tree, arity: Int)(
          implicit pos: Position): js.Tree = {
        val clsSym = getRequiredClass(AnonFunPrefScala + arity)
        val ctor = clsSym.tpe.member(nme.CONSTRUCTOR)
        genNew(clsSym, ctor, List(jsFunction))
      }

      def unapply(tree: js.New): Option[(js.Tree, Int)] = tree match {
        case js.New(jstpe.ClassType(wrapperName), _, List(fun))
            if wrapperName.startsWith(AnonFunPrefJS) =>
          val arityStr = wrapperName.substring(AnonFunPrefJS.length)
          try {
            Some((fun, arityStr.toInt))
          } catch {
            case e: NumberFormatException => None
          }

        case _ =>
          None
      }
    }

    /** Gen and record JS code for a raw JS function class.
     *
     *  This is called when emitting a ClassDef that represents an anonymous
     *  class extending `js.FunctionN`. These are generated by the SAM
     *  synthesizer when the target type is a `js.FunctionN`. Since JS
     *  functions are not classes, we deconstruct the ClassDef, then
     *  reconstruct it to be a genuine Closure.
     *
     *  Compared to `tryGenAndRecordAnonFunctionClass()`, this function must
     *  always succeed, because we really cannot afford keeping them as
     *  anonymous classes. The good news is that it can do so, because the
     *  body of SAM lambdas is hoisted in the enclosing class. Hence, the
     *  apply() method is just a forwarder to calling that hoisted method.
     *
     *  From a class looking like this:
     *
     *    final class <anon>(outer, capture1, ..., captureM) extends js.FunctionN[...] {
     *      def apply(param1, ..., paramN) = {
     *        outer.lambdaImpl(param1, ..., paramN, capture1, ..., captureM)
     *      }
     *    }
     *    new <anon>(o, c1, ..., cM)
     *
     *  we generate a function maker that emits:
     *
     *    lambda<o, c1, ..., cM>[notype](
     *        outer, capture1, ..., captureM, param1, ..., paramN) {
     *      outer.lambdaImpl(param1, ..., paramN, capture1, ..., captureM)
     *    }
     *
     *  The function maker is recorded in `translatedAnonFunctions` to be
     *  fetched later by the translation for New.
     */
    def genAndRecordRawJSFunctionClass(cd: ClassDef): Unit = {
      val sym = cd.symbol
      assert(isRawJSFunctionDef(sym),
          s"genAndRecordRawJSFunctionClass called with non-JS function $cd")

      withScopedVars(
          currentClassSym := sym
      ) {
        val (functionMaker, _) =
          tryGenAndRecordAnonFunctionClassGeneric(cd) { msg =>
            abort(s"Could not generate raw function maker for JS function: $msg")
          }

        translatedAnonFunctions += sym -> functionMaker
      }
    }

    /** Code common to tryGenAndRecordAnonFunctionClass and
     *  genAndRecordRawJSFunctionClass.
     */
    private def tryGenAndRecordAnonFunctionClassGeneric(cd: ClassDef)(
        fail: (=> String) => Nothing): (List[js.Tree] => js.Tree, Int) = {
      implicit val pos = cd.pos
      val sym = cd.symbol

      // First checks

      if (sym.isSubClass(PartialFunctionClass))
        fail(s"Cannot rewrite PartialFunction $cd")
      if (instantiatedAnonFunctions contains sym) {
        // when the ordering we're given is evil (it happens!)
        fail(s"Abort function rewrite because it was already instantiated: $cd")
      }

      // First step: find the apply method def, and collect param accessors

      var paramAccessors: List[Symbol] = Nil
      var applyDef: DefDef = null

      def gen(tree: Tree): Unit = {
        tree match {
          case EmptyTree => ()
          case Template(_, _, body) => body foreach gen
          case vd @ ValDef(mods, name, tpt, rhs) =>
            val fsym = vd.symbol
            if (!fsym.isParamAccessor)
              fail(s"Found field $fsym which is not a param accessor in anon function $cd")

            if (fsym.isPrivate) {
              paramAccessors ::= fsym
            } else {
              // Uh oh ... an inner something will try to access my fields
              fail(s"Found a non-private field $fsym in $cd")
            }
          case dd: DefDef =>
            val ddsym = dd.symbol
            if (ddsym.isClassConstructor) {
              if (!ddsym.isPrimaryConstructor)
                fail(s"Non-primary constructor $ddsym in anon function $cd")
            } else {
              val name = dd.name.toString
              if (name == "apply" || (ddsym.isSpecialized && name.startsWith("apply$"))) {
                if ((applyDef eq null) || ddsym.isSpecialized)
                  applyDef = dd
              } else {
                // Found a method we cannot encode in the rewriting
                fail(s"Found a non-apply method $ddsym in $cd")
              }
            }
          case _ =>
            fail("Illegal tree in gen of genAndRecordAnonFunctionClass(): " + tree)
        }
      }
      gen(cd.impl)
      paramAccessors = paramAccessors.reverse // preserve definition order

      if (applyDef eq null)
        fail(s"Did not find any apply method in anon function $cd")

      withNewLocalNameScope {
        // Second step: build the list of useful constructor parameters

        val ctorParams = sym.primaryConstructor.tpe.params

        if (paramAccessors.size != ctorParams.size &&
            !(paramAccessors.size == ctorParams.size-1 &&
                ctorParams.head.unexpandedName == jsnme.arg_outer)) {
          fail(
              s"Have param accessors $paramAccessors but "+
              s"ctor params $ctorParams in anon function $cd")
        }

        val hasUnusedOuterCtorParam = paramAccessors.size != ctorParams.size
        val usedCtorParams =
          if (hasUnusedOuterCtorParam) ctorParams.tail
          else ctorParams
        val ctorParamDefs = usedCtorParams map { p =>
          // in the apply method's context
          js.ParamDef(encodeLocalSym(p)(p.pos), toIRType(p.tpe),
              mutable = false, rest = false)(p.pos)
        }

        // Third step: emit the body of the apply method def

        val applyMethod = withScopedVars(
            paramAccessorLocals := (paramAccessors zip ctorParamDefs).toMap,
            tryingToGenMethodAsJSFunction := true
        ) {
          try {
            genMethodWithCurrentLocalNameScope(applyDef).getOrElse(
                abort(s"Oops, $applyDef did not produce a method"))
          } catch {
            case e: CancelGenMethodAsJSFunction =>
              fail(e.getMessage)
          }
        }

        // Fourth step: patch the body to unbox parameters and box result

        val js.MethodDef(_, _, params, _, body) = applyMethod
        val (patchedParams, patchedBody) =
          patchFunBodyWithBoxes(applyDef.symbol, params, body)

        // Fifth step: build the function maker

        val isThisFunction = JSThisFunctionClasses.exists(sym isSubClass _)
        assert(!isThisFunction || patchedParams.nonEmpty,
            s"Empty param list in ThisFunction: $cd")

        val functionMaker = { capturedArgs0: List[js.Tree] =>
          val capturedArgs =
            if (hasUnusedOuterCtorParam) capturedArgs0.tail
            else capturedArgs0
          assert(capturedArgs.size == ctorParamDefs.size)

          if (isThisFunction) {
            val thisParam :: actualParams = patchedParams
            js.Closure(
                ctorParamDefs,
                actualParams,
                js.Block(
                    js.VarDef(thisParam.name, thisParam.ptpe, mutable = false,
                        js.This()(thisParam.ptpe)(thisParam.pos))(thisParam.pos),
                    patchedBody),
                capturedArgs)
          } else {
            js.Closure(ctorParamDefs, patchedParams, patchedBody, capturedArgs)
          }
        }

        val arity = params.size

        (functionMaker, arity)
      }
    }

    /** Generate JS code for an anonymous function
     *
     *  Anonymous functions survive until the backend only under
     *  -Ydelambdafy:method
     *  and when they do, their body is always of the form
     *  EnclosingClass.this.someMethod(args)
     *  where the args are either formal parameters of the lambda, or local
     *  variables or the enclosing def. The latter must be captured.
     *
     *  We identify the captures using the same method as the `delambdafy`
     *  phase. We have an additional hack for `this`.
     *
     *  We translate them by instantiating scala.scalajs.runtime.AnonFunctionN
     *  with a JS closure:
     *
     *  new ScalaJS.c.sjsr_AnonFunctionN().init___xyz(
     *    lambda<this, capture1, ..., captureM>(
     *        _this, capture1, ..., captureM, arg1, ..., argN) {
     *      _this.someMethod(arg1, ..., argN, capture1, ..., captureM)
     *    }
     *  )
     *
     *  In addition, input params are unboxed before use, and the result of
     *  someMethod() is boxed back.
     */
    private def genAnonFunction(originalFunction: Function): js.Tree = {
      implicit val pos = originalFunction.pos
      val Function(paramTrees, Apply(
          targetTree @ Select(receiver, _), allArgs0)) = originalFunction

      val captureSyms =
        global.delambdafy.FreeVarTraverser.freeVarsOf(originalFunction)
      val target = targetTree.symbol
      val params = paramTrees.map(_.symbol)

      val allArgs = allArgs0 map genExpr

      val formalCaptures = captureSyms.toList map { sym =>
        // Use the anonymous function pos
        js.ParamDef(encodeLocalSym(sym)(pos), toIRType(sym.tpe),
            mutable = false, rest = false)(pos)
      }
      val actualCaptures = formalCaptures.map(_.ref)

      val formalArgs = params map { p =>
        // Use the param pos
        js.ParamDef(encodeLocalSym(p)(p.pos), toIRType(p.tpe),
            mutable = false, rest = false)(p.pos)
      }

      val isInImplClass = target.owner.isImplClass

      val (allFormalCaptures, body, allActualCaptures) = if (!isInImplClass) {
        val thisActualCapture = genExpr(receiver)
        val thisFormalCapture = js.ParamDef(
            freshLocalIdent("this")(receiver.pos),
            thisActualCapture.tpe, mutable = false, rest = false)(receiver.pos)
        val thisCaptureArg = thisFormalCapture.ref

        val body = if (isRawJSType(receiver.tpe) && target.owner != ObjectClass) {
          assert(isScalaJSDefinedJSClass(target.owner) && !isExposed(target),
              s"A Function lambda is trying to call an exposed JS method ${target.fullName}")
          genApplyJSClassMethod(thisCaptureArg, target, allArgs)
        } else {
          genApplyMethod(thisCaptureArg, target, allArgs)
        }

        (thisFormalCapture :: formalCaptures,
            body, thisActualCapture :: actualCaptures)
      } else {
        val body = genTraitImplApply(target, allArgs)

        (formalCaptures, body, actualCaptures)
      }

      val (patchedFormalArgs, patchedBody) = {
        patchFunBodyWithBoxes(target, formalArgs, body,
            useParamsBeforeLambdaLift = true)
      }

      val closure = js.Closure(
          allFormalCaptures,
          patchedFormalArgs,
          patchedBody,
          allActualCaptures)

      JSFunctionToScala(closure, params.size)
    }

    private def patchFunBodyWithBoxes(methodSym: Symbol,
        params: List[js.ParamDef], body: js.Tree,
        useParamsBeforeLambdaLift: Boolean = false)(
        implicit pos: Position): (List[js.ParamDef], js.Tree) = {
      val methodType = enteringPhase(currentRun.posterasurePhase)(methodSym.tpe)

      // See the comment in genPrimitiveJSArgs for a rationale about this
      val paramTpes = enteringPhase(currentRun.posterasurePhase) {
        for (param <- methodType.params)
          yield param.name -> param.tpe
      }.toMap

      /* Normally, we should work with the list of parameters as seen right
       * now. But when generating an anonymous function from a Function node,
       * the `methodSym` we use is the *target* of the inner call, not the
       * enclosing method for which we're patching the params and body. This
       * is a hack which we have to do because there is no such enclosing
       * method in that case. When we use the target, the list of symbols for
       * formal parameters that we want to see is that before lambdalift, not
       * the one we see right now.
       */
      val paramSyms = {
        if (useParamsBeforeLambdaLift)
          enteringPhase(currentRun.phaseNamed("lambdalift"))(methodSym.tpe.params)
        else
          methodSym.tpe.params
      }

      val (patchedParams, paramsLocal) = (for {
        (param, paramSym) <- params zip paramSyms
      } yield {
        val paramTpe = paramTpes.getOrElse(paramSym.name, paramSym.tpe)
        val paramName = param.name
        val js.Ident(name, origName) = paramName
        val newOrigName = origName.getOrElse(name)
        val newNameIdent = freshLocalIdent(name)(paramName.pos)
        val patchedParam = js.ParamDef(newNameIdent, jstpe.AnyType,
            mutable = false, rest = param.rest)(param.pos)
        val paramLocal = js.VarDef(paramName, param.ptpe, mutable = false,
            fromAny(patchedParam.ref, paramTpe))
        (patchedParam, paramLocal)
      }).unzip

      assert(!methodSym.isClassConstructor,
          s"Trying to patchFunBodyWithBoxes for constructor ${methodSym.fullName}")

      val patchedBody = js.Block(
          paramsLocal :+ ensureBoxed(body, methodType.resultType))

      (patchedParams, patchedBody)
    }

    // Utilities ---------------------------------------------------------------

    /** Generate a literal "zero" for the requested type */
    def genZeroOf(tpe: Type)(implicit pos: Position): js.Tree = toTypeKind(tpe) match {
      case VOID       => abort("Cannot call genZeroOf(VOID)")
      case BOOL       => js.BooleanLiteral(false)
      case LONG       => js.LongLiteral(0L)
      case INT(_)     => js.IntLiteral(0)
      case FloatKind  => js.FloatLiteral(0.0f)
      case DoubleKind => js.DoubleLiteral(0.0)
      case _          => js.Null()
    }

    /** Generate loading of a module value
     *  Can be given either the module symbol, or its module class symbol.
     */
    def genLoadModule(sym0: Symbol)(implicit pos: Position): js.Tree = {
      require(sym0.isModuleOrModuleClass,
          "genLoadModule called with non-module symbol: " + sym0)
      val sym1 = if (sym0.isModule) sym0.moduleClass else sym0
      val sym = // redirect all static methods of String to RuntimeString
        if (sym1 == StringModule) RuntimeStringModule.moduleClass
        else sym1

      val isGlobalScope = sym.tpe.typeSymbol isSubClass JSGlobalScopeClass

      if (isGlobalScope) {
        genLoadGlobal()
      } else if (isJSNativeClass(sym)) {
        genPrimitiveJSModule(sym)
      } else {
        val moduleClassName = encodeClassFullName(sym)

        val cls = jstpe.ClassType(moduleClassName)
        if (isRawJSType(sym.tpe)) js.LoadJSModule(cls)
        else js.LoadModule(cls)
      }
    }

    /** Gen JS code to load the global scope. */
    private def genLoadGlobal()(implicit pos: Position): js.Tree =
      js.JSBracketSelect(js.JSEnvInfo(), js.StringLiteral("global"))

    /** Generate access to a static member */
    private def genStaticMember(sym: Symbol)(implicit pos: Position) = {
      /* Actually, there is no static member in Scala.js. If we come here, that
       * is because we found the symbol in a Java-emitted .class in the
       * classpath. But the corresponding implementation in Scala.js will
       * actually be a val in the companion module.
       * We cannot use the .class files produced by our reimplementations of
       * these classes (in which the symbol would be a Scala accessor) because
       * that crashes the rest of scalac (at least for some choice symbols).
       * Hence we cheat here.
       */
      import scalaPrimitives._
      import jsPrimitives._
      if (isPrimitive(sym)) {
        getPrimitive(sym) match {
          case UNITVAL  => js.Undefined()
          case UNITTYPE => genClassConstant(UnitTpe)
        }
      } else {
        val instance = genLoadModule(sym.owner)
        val method = encodeStaticMemberSym(sym)
        js.Apply(instance, method, Nil)(toIRType(sym.tpe))
      }
    }

    /** Generate a Class[_] value (e.g. coming from classOf[T]) */
    private def genClassConstant(tpe: Type)(implicit pos: Position): js.Tree =
      js.ClassOf(toReferenceType(tpe))
  }

  /** Tests whether the given type represents a raw JavaScript type,
   *  i.e., whether it extends scala.scalajs.js.Any.
   */
  def isRawJSType(tpe: Type): Boolean =
    tpe.typeSymbol.annotations.find(_.tpe =:= RawJSTypeAnnot.tpe).isDefined

  /** Tests whether the given class is a Scala.js-defined JS class. */
  def isScalaJSDefinedJSClass(sym: Symbol): Boolean =
    !sym.isTrait && sym.hasAnnotation(ScalaJSDefinedAnnotation)

  /** Tests whether the given class is a JS native class. */
  private def isJSNativeClass(sym: Symbol): Boolean =
    isRawJSType(sym.tpe) && !isScalaJSDefinedJSClass(sym)

  /** Tests whether the given member is exposed, i.e., whether it was
   *  originally a public or protected member of a Scala.js-defined JS class.
   */
  private def isExposed(sym: Symbol): Boolean =
    sym.hasAnnotation(ExposedJSMemberAnnot)

  /** Test whether `sym` is the symbol of a raw JS function definition */
  private def isRawJSFunctionDef(sym: Symbol): Boolean =
    sym.isAnonymousClass && AllJSFunctionClasses.exists(sym isSubClass _)

  private def isRawJSCtorDefaultParam(sym: Symbol) = {
    isCtorDefaultParam(sym) &&
    isRawJSType(patchedLinkedClassOfClass(sym.owner).tpe)
  }

  private def isJSNativeCtorDefaultParam(sym: Symbol) = {
    isCtorDefaultParam(sym) &&
    isJSNativeClass(patchedLinkedClassOfClass(sym.owner))
  }

  private def isCtorDefaultParam(sym: Symbol) = {
    sym.hasFlag(reflect.internal.Flags.DEFAULTPARAM) &&
    sym.owner.isModuleClass &&
    nme.defaultGetterToMethod(sym.name) == nme.CONSTRUCTOR
  }

  private def hasDefaultCtorArgsAndRawJSModule(classSym: Symbol): Boolean = {
    /* Get the companion module class.
     * For inner classes the sym.owner.companionModule can be broken,
     * therefore companionModule is fetched at uncurryPhase.
     */
    val companionClass = enteringPhase(currentRun.uncurryPhase) {
      classSym.companionModule
    }.moduleClass

    def hasDefaultParameters = {
      val syms = classSym.info.members.filter(_.isClassConstructor)
      enteringPhase(currentRun.uncurryPhase) {
        syms.exists(_.paramss.iterator.flatten.exists(_.hasDefault))
      }
    }

    isJSNativeClass(companionClass) && hasDefaultParameters
  }

  private def patchedLinkedClassOfClass(sym: Symbol): Symbol = {
    /* Work around a bug of scalac with linkedClassOfClass where package
     * objects are involved (the companion class would somehow exist twice
     * in the scope, making an assertion fail in Symbol.suchThat).
     * Basically this inlines linkedClassOfClass up to companionClass,
     * then replaces the `suchThat` by a `filter` and `head`.
     */
    val flatOwnerInfo = {
      // inline Symbol.flatOwnerInfo because it is protected
      if (sym.needsFlatClasses)
        sym.info
      sym.owner.rawInfo
    }
    val result = flatOwnerInfo.decl(sym.name).filter(_ isCoDefinedWith sym)
    if (!result.isOverloaded) result
    else result.alternatives.head
  }

  /** Whether a field is suspected to be mutable in the IR's terms
   *
   *  A field is mutable in the IR, if it is assigned to elsewhere than in the
   *  constructor of its class.
   *
   *  Mixed-in fields are always mutable, since they will be assigned to in
   *  a trait initializer (rather than a constructor).
   *  Further, in 2.10.x fields used to implement lazy vals are not marked
   *  mutable (but assigned to in the accessor).
   */
  private def suspectFieldMutable(sym: Symbol) = {
    import scala.reflect.internal.Flags
    sym.hasFlag(Flags.MIXEDIN) || sym.isMutable || sym.isLazy
  }

  private def isStringType(tpe: Type): Boolean =
    tpe.typeSymbol == StringClass

  private def isLongType(tpe: Type): Boolean =
    tpe.typeSymbol == LongClass

  private lazy val BoxedBooleanClass = boxedClass(BooleanClass)
  private lazy val BoxedByteClass = boxedClass(ByteClass)
  private lazy val BoxedShortClass = boxedClass(ShortClass)
  private lazy val BoxedIntClass = boxedClass(IntClass)
  private lazy val BoxedLongClass = boxedClass(LongClass)
  private lazy val BoxedFloatClass = boxedClass(FloatClass)
  private lazy val BoxedDoubleClass = boxedClass(DoubleClass)

  private lazy val NumberClass = requiredClass[java.lang.Number]

  private lazy val HijackedNumberClasses =
    Seq(BoxedByteClass, BoxedShortClass, BoxedIntClass, BoxedLongClass,
        BoxedFloatClass, BoxedDoubleClass)
  private lazy val HijackedBoxedClasses =
    Seq(BoxedUnitClass, BoxedBooleanClass) ++ HijackedNumberClasses

  protected lazy val isHijackedBoxedClass: Set[Symbol] =
    HijackedBoxedClasses.toSet

  private lazy val InlineAnnotationClass = requiredClass[scala.inline]
  private lazy val NoinlineAnnotationClass = requiredClass[scala.noinline]

  private lazy val ignoreNoinlineAnnotation: Set[Symbol] = {
    val ccClass = getClassIfDefined("scala.util.continuations.ControlContext")

    Set(
        getMemberIfDefined(ListClass, nme.map),
        getMemberIfDefined(ListClass, nme.flatMap),
        getMemberIfDefined(ListClass, newTermName("collect")),
        getMemberIfDefined(ccClass, nme.map),
        getMemberIfDefined(ccClass, nme.flatMap)
    ) - NoSymbol
  }

  private def isMaybeJavaScriptException(tpe: Type) =
    JavaScriptExceptionClass isSubClass tpe.typeSymbol

  def isStaticModule(sym: Symbol): Boolean =
    sym.isModuleClass && !sym.isImplClass && !sym.isLifted
}
