/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.nscplugin

import scala.language.implicitConversions

import scala.annotation.switch

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import scala.tools.nsc._

import scala.annotation.tailrec

import scala.reflect.internal.Flags

import org.scalajs.ir
import org.scalajs.ir.{Trees => js, Types => jstpe, ClassKind, Hashers, OriginalName}
import org.scalajs.ir.Names.{LocalName, SimpleFieldName, FieldName, SimpleMethodName, MethodName, ClassName}
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees.OptimizerHints
import org.scalajs.ir.Version.Unversioned

import org.scalajs.nscplugin.util.{ScopedVar, VarBox}
import ScopedVar.withScopedVars

/** Generate JavaScript code and output it to disk
 *
 *  @author Sébastien Doeraene
 */
abstract class GenJSCode[G <: Global with Singleton](val global: G)
    extends plugins.PluginComponent with TypeConversions[G] with JSEncoding[G]
    with GenJSExports[G] with GenJSFiles[G] with CompatComponent {

  import GenJSCode._

  /** Not for use in the constructor body: only initialized afterwards. */
  val jsAddons: JSGlobalAddons {
    val global: GenJSCode.this.global.type
  }

  /** Not for use in the constructor body: only initialized afterwards. */
  val scalaJSOpts: ScalaJSOptions

  import global._
  import jsAddons._
  import rootMirror._
  import definitions._
  import jsDefinitions._
  import jsInterop.{jsNameOf, jsNativeLoadSpecOfOption, JSName, JSCallingConvention}

  import treeInfo.{hasSynthCaseSymbol, StripCast}

  import platform.isMaybeBoxed

  val phaseName: String = "jscode"
  override val description: String = "generate JavaScript code from ASTs"

  /** testing: this will be called for each generated `ClassDef`. */
  def generatedJSAST(clDef: js.ClassDef): Unit

  /** Implicit conversion from nsc Position to ir.Position. */
  implicit def pos2irPos(pos: Position): ir.Position = {
    if (pos == NoPosition) ir.Position.NoPosition
    else {
      val source = pos2irPosCache.toIRSource(pos.source)
      // nsc positions are 1-based but IR positions are 0-based
      ir.Position(source, pos.line-1, pos.column-1)
    }
  }

  private[this] object pos2irPosCache {
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

  object jsnme {
    val anyHash = newTermName("anyHash")
    val arg_outer = newTermName("arg$outer")
  }

  private sealed abstract class EnclosingLabelDefInfo {
    var generatedReturns: Int = 0
  }

  private final class EnclosingLabelDefInfoWithResultAsReturn()
      extends EnclosingLabelDefInfo

  private final class EnclosingLabelDefInfoWithResultAsAssigns(
      val paramSyms: List[Symbol])
      extends EnclosingLabelDefInfo

  class JSCodePhase(prev: Phase) extends StdPhase(prev) with JSExportsPhase {

    override def name: String = phaseName
    override def description: String = GenJSCode.this.description

    // Scoped state ------------------------------------------------------------
    // Per class body
    val currentClassSym = new ScopedVar[Symbol]
    private val fieldsMutatedInCurrentClass = new ScopedVar[mutable.Set[Name]]
    private val generatedSAMWrapperCount = new ScopedVar[VarBox[Int]]

    def currentThisType: jstpe.Type = {
      encodeClassType(currentClassSym) match {
        case tpe @ jstpe.ClassType(cls) =>
          jstpe.BoxedClassToPrimType.getOrElse(cls, tpe)
        case tpe =>
          tpe
      }
    }

    // Per method body
    private val currentMethodSym = new ScopedVar[Symbol]
    private val thisLocalVarIdent = new ScopedVar[Option[js.LocalIdent]]
    private val enclosingLabelDefInfos = new ScopedVar[Map[Symbol, EnclosingLabelDefInfo]]
    private val isModuleInitialized = new ScopedVar[VarBox[Boolean]]
    private val undefinedDefaultParams = new ScopedVar[mutable.Set[Symbol]]
    private val mutableLocalVars = new ScopedVar[mutable.Set[Symbol]]
    private val mutatedLocalVars = new ScopedVar[mutable.Set[Symbol]]

    private def withPerMethodBodyState[A](methodSym: Symbol)(body: => A): A = {
      withScopedVars(
          currentMethodSym := methodSym,
          thisLocalVarIdent := None,
          enclosingLabelDefInfos := Map.empty,
          isModuleInitialized := new VarBox(false),
          undefinedDefaultParams := mutable.Set.empty,
          mutableLocalVars := mutable.Set.empty,
          mutatedLocalVars := mutable.Set.empty
      ) {
        body
      }
    }

    // For anonymous methods
    // These have a default, since we always read them.
    private val tryingToGenMethodAsJSFunction = new ScopedVar[Boolean](false)
    private val paramAccessorLocals = new ScopedVar(Map.empty[Symbol, js.ParamDef])

    /* Contextual JS class value for some operations of nested JS classes that
     * need one.
     */
    private val contextualJSClassValue =
      new ScopedVar[Option[js.Tree]](None)

    private def acquireContextualJSClassValue[A](f: Option[js.Tree] => A): A = {
      val jsClassValue = contextualJSClassValue.get
      withScopedVars(
          contextualJSClassValue := None
      ) {
        f(jsClassValue)
      }
    }

    private class CancelGenMethodAsJSFunction(message: String)
        extends scala.util.control.ControlThrowable {
      override def getMessage(): String = message
    }

    // Rewriting of anonymous function classes ---------------------------------

    /** Start nested generation of a class.
     *
     *  Fully resets the scoped state (including local name scope).
     *  Allows to generate an anonymous class as needed.
     */
    private def nestedGenerateClass[T](clsSym: Symbol)(body: => T): T = {
      withScopedVars(
          currentClassSym := clsSym,
          fieldsMutatedInCurrentClass := mutable.Set.empty,
          generatedSAMWrapperCount := new VarBox(0),
          currentMethodSym := null,
          thisLocalVarIdent := null,
          enclosingLabelDefInfos := null,
          isModuleInitialized := null,
          undefinedDefaultParams := null,
          mutableLocalVars := null,
          mutatedLocalVars := null,
          tryingToGenMethodAsJSFunction := false,
          paramAccessorLocals := Map.empty
      )(withNewLocalNameScope(body))
    }

    // Global class generation state -------------------------------------------

    private val lazilyGeneratedAnonClasses = mutable.Map.empty[Symbol, ClassDef]
    private val generatedClasses = ListBuffer.empty[(js.ClassDef, Position)]
    private val generatedStaticForwarderClasses = ListBuffer.empty[(Symbol, js.ClassDef)]

    private def consumeLazilyGeneratedAnonClass(sym: Symbol): ClassDef = {
      /* If we are trying to generate an method as JSFunction, we cannot
       * actually consume the symbol, since we might fail trying and retry.
       * We will then see the same tree again and not find the symbol anymore.
       *
       * If we are sure this is the only generation, we remove the symbol to
       * make sure we don't generate the same class twice.
       */
      val optDef = {
        if (tryingToGenMethodAsJSFunction)
          lazilyGeneratedAnonClasses.get(sym)
        else
          lazilyGeneratedAnonClasses.remove(sym)
      }

      optDef.getOrElse {
        abort("Couldn't find tree for lazily generated anonymous class " +
            s"${sym.fullName} at ${sym.pos}")
      }
    }

    // Top-level apply ---------------------------------------------------------

    override def run(): Unit = {
      scalaPrimitives.init()
      genBCode.bTypes.initializeCoreBTypes()
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
     *  - Implementation classes for JS traits
     *
     *  Some classes representing anonymous functions are not actually emitted.
     *  Instead, a temporary representation of their `apply` method is built
     *  and recorded, so that it can be inlined as a JavaScript anonymous
     *  function in the method that instantiates it.
     *
     *  Other ClassDefs are emitted according to their nature:
     *  * Non-native JS class       -> `genNonNativeJSClass()`
     *  * Other JS type (<: js.Any) -> `genJSClassData()`
     *  * Interface                 -> `genInterface()`
     *  * Normal class              -> `genClass()`
     */
    override def apply(cunit: CompilationUnit): Unit = {
      try {
        def collectClassDefs(tree: Tree): List[ClassDef] = {
          tree match {
            case EmptyTree => Nil
            case PackageDef(_, stats) => stats flatMap collectClassDefs
            case cd: ClassDef => cd :: Nil
          }
        }
        val allClassDefs = collectClassDefs(cunit.body)

        /* There are three types of anonymous classes we want to generate
         * only once we need them so we can inline them at construction site:
         *
         * - anonymous class that are JS types, which includes:
         *   - lambdas for js.FunctionN and js.ThisFunctionN (SAMs). (We may
         *     not generate actual Scala classes for these).
         *   - anonymous (non-lambda) JS classes. These classes may not have
         *     their own prototype. Therefore, their constructor *must* be
         *     inlined.
         * - lambdas for scala.FunctionN. This is only an optimization and may
         *   fail. In the case of failure, we fall back to generating a
         *   fully-fledged Scala class.
         *
         * Since for all these, we don't know how they inter-depend, we just
         * store them in a map at this point.
         */
        val (lazyAnons, fullClassDefs) = allClassDefs.partition { cd =>
          val sym = cd.symbol
          isAnonymousJSClass(sym) || isJSFunctionDef(sym) || sym.isAnonymousFunction
        }

        lazilyGeneratedAnonClasses ++= lazyAnons.map(cd => cd.symbol -> cd)

        /* Finally, we emit true code for the remaining class defs. */
        for (cd <- fullClassDefs) {
          val sym = cd.symbol
          implicit val pos = sym.pos

          /* Do not actually emit code for primitive types nor scala.Array. */
          val isPrimitive =
            isPrimitiveValueClass(sym) || (sym == ArrayClass)

          if (!isPrimitive) {
            withScopedVars(
                currentClassSym := sym,
                fieldsMutatedInCurrentClass := mutable.Set.empty,
                generatedSAMWrapperCount := new VarBox(0)
            ) {
              val tree = if (isJSType(sym)) {
                if (!sym.isTraitOrInterface && isNonNativeJSClass(sym) &&
                    !isJSFunctionDef(sym)) {
                  genNonNativeJSClass(cd)
                } else {
                  genJSClassData(cd)
                }
              } else if (sym.isTraitOrInterface) {
                genInterface(cd)
              } else {
                genClass(cd)
              }

              generatedClasses += tree -> sym.pos
            }
          }
        }

        val clDefs: List[(js.ClassDef, Position)] = if (generatedStaticForwarderClasses.isEmpty) {
          /* Fast path, applicable under -Xno-forwarders, as well as when all
           * the `object`s of a compilation unit have a companion class.
           */
          generatedClasses.toList
        } else {
          val regularClasses = generatedClasses.toList

          /* #4148 Add generated static forwarder classes, except those that
           * would collide with regular classes on case insensitive file
           * systems.
           */

          /* I could not find any reference anywhere about what locale is used
           * by case insensitive file systems to compare case-insensitively.
           * In doubt, force the English locale, which is probably going to do
           * the right thing in virtually all cases (especially if users stick
           * to ASCII class names), and it has the merit of being deterministic,
           * as opposed to using the OS' default locale.
           * The JVM backend performs a similar test to emit a warning for
           * conflicting top-level classes. However, it uses `toLowerCase()`
           * without argument, which is not deterministic.
           */
          def caseInsensitiveNameOf(classDef: js.ClassDef): String =
            classDef.name.name.nameString.toLowerCase(java.util.Locale.ENGLISH)

          val generatedCaseInsensitiveNames =
            regularClasses.map(pair => caseInsensitiveNameOf(pair._1)).toSet
          val staticForwarderClasses = generatedStaticForwarderClasses.toList
            .withFilter { case (site, classDef) =>
              if (!generatedCaseInsensitiveNames.contains(caseInsensitiveNameOf(classDef))) {
                true
              } else {
                global.runReporting.warning(
                    site.pos,
                    s"Not generating the static forwarders of ${classDef.name.name.nameString} " +
                    "because its name differs only in case from the name of another class or " +
                    "trait in this compilation unit.",
                    WarningCategory.Other,
                    site)
                false
              }
            }
            .map(pair => (pair._2, pair._1.pos))

          regularClasses ::: staticForwarderClasses
        }

        for ((classDef, pos) <- clDefs) {
          try {
            val hashedClassDef = Hashers.hashClassDef(classDef)
            generatedJSAST(hashedClassDef)
            genIRFile(cunit, hashedClassDef)
          } catch {
            case e: ir.InvalidIRException =>
              e.tree match {
                case ir.Trees.Transient(UndefinedParam) =>
                  reporter.error(pos,
                      "Found a dangling UndefinedParam at " +
                      s"${e.tree.pos}. This is likely due to a bad " +
                      "interaction between a macro or a compiler plugin " +
                      "and the Scala.js compiler plugin. If you hit " +
                      "this, please let us know.")

                case _ =>
                  reporter.error(pos,
                      "The Scala.js compiler generated invalid IR for " +
                      "this class. Please report this as a bug. IR: " +
                      e.tree)
              }
          }
        }
      } catch {
        // Handle exceptions in exactly the same way as the JVM backend
        case ex: InterruptedException =>
          throw ex
        case ex: Throwable =>
          if (settings.debug.value)
            ex.printStackTrace()
          globalError(s"Error while emitting ${cunit.source}\n${ex.getMessage}")
      } finally {
        lazilyGeneratedAnonClasses.clear()
        generatedStaticForwarderClasses.clear()
        generatedClasses.clear()
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

      assert(!sym.isTraitOrInterface,
          "genClass() must be called only for normal classes: "+sym)
      assert(sym.superClass != NoSymbol, sym)

      if (hasDefaultCtorArgsAndJSModule(sym)) {
        reporter.error(pos,
            "Implementation restriction: constructors of " +
            "Scala classes cannot have default parameters " +
            "if their companion module is JS native.")
      }

      val classIdent = encodeClassNameIdent(sym)
      val originalName = originalNameOfClass(sym)
      val isHijacked = isHijackedClass(sym)

      // Optimizer hints

      val isDynamicImportThunk = sym.isSubClass(DynamicImportThunkClass)

      def isStdLibClassWithAdHocInlineAnnot(sym: Symbol): Boolean = {
        val fullName = sym.fullName
        (fullName.startsWith("scala.Tuple") && !fullName.endsWith("$")) ||
        (fullName.startsWith("scala.collection.mutable.ArrayOps$of"))
      }

      val shouldMarkInline = (
          isDynamicImportThunk ||
          sym.hasAnnotation(InlineAnnotationClass) ||
          (sym.isAnonymousFunction && !sym.isSubClass(PartialFunctionClass)) ||
          isStdLibClassWithAdHocInlineAnnot(sym))

      val optimizerHints =
        OptimizerHints.empty.
          withInline(shouldMarkInline).
          withNoinline(sym.hasAnnotation(NoinlineAnnotationClass))

      // Generate members (constructor + methods)

      val methodsBuilder = List.newBuilder[js.MethodDef]
      val jsNativeMembersBuilder = List.newBuilder[js.JSNativeMemberDef]

      def gen(tree: Tree): Unit = {
        tree match {
          case EmptyTree => ()
          case Template(_, _, body) => body foreach gen

          case ValDef(mods, name, tpt, rhs) =>
            () // fields are added via genClassFields()

          case dd: DefDef =>
            if (dd.symbol.hasAnnotation(JSNativeAnnotation))
              jsNativeMembersBuilder += genJSNativeMemberDef(dd)
            else
              methodsBuilder ++= genMethod(dd)

          case _ => abort("Illegal tree in gen of genClass(): " + tree)
        }
      }

      gen(impl)

      val fields = if (!isHijacked) genClassFields(cd) else Nil

      val jsNativeMembers = jsNativeMembersBuilder.result()
      val generatedMethods = methodsBuilder.result()

      val memberExports = genMemberExports(sym)

      val topLevelExportDefs = genTopLevelExports(sym)

      // Static initializer
      val optStaticInitializer = {
        // Initialization of reflection data, if required
        val reflectInit = {
          val enableReflectiveInstantiation = {
            (sym :: sym.ancestors).exists { ancestor =>
              ancestor.hasAnnotation(EnableReflectiveInstantiationAnnotation)
            }
          }
          if (enableReflectiveInstantiation)
            genRegisterReflectiveInstantiation(sym)
          else
            None
        }

        // Initialization of the module because of field exports
        val needsStaticModuleInit =
          topLevelExportDefs.exists(_.isInstanceOf[js.TopLevelFieldExportDef])
        val staticModuleInit =
          if (!needsStaticModuleInit) None
          else Some(genLoadModule(sym))

        val staticInitializerStats =
          reflectInit.toList ::: staticModuleInit.toList
        if (staticInitializerStats.nonEmpty) {
          List(genStaticConstructorWithStats(
              ir.Names.StaticInitializerName,
              js.Block(staticInitializerStats)))
        } else {
          Nil
        }
      }

      val optDynamicImportForwarder =
        if (isDynamicImportThunk) List(genDynamicImportForwarder(sym))
        else Nil

      val allMethodsExceptStaticForwarders: List[js.MethodDef] =
        generatedMethods ::: optStaticInitializer ::: optDynamicImportForwarder

      // Add static forwarders
      val allMethods = if (!isCandidateForForwarders(sym)) {
        allMethodsExceptStaticForwarders
      } else {
        if (sym.isModuleClass) {
          /* If the module class has no linked class, we must create one to
           * hold the static forwarders. Otherwise, this is going to be handled
           * when generating the companion class.
           */
          if (!sym.linkedClassOfClass.exists) {
            val forwarders = genStaticForwardersFromModuleClass(Nil, sym)
            if (forwarders.nonEmpty) {
              val forwardersClassDef = js.ClassDef(
                  js.ClassIdent(ClassName(classIdent.name.nameString.stripSuffix("$"))),
                  originalName,
                  ClassKind.Class,
                  None,
                  Some(js.ClassIdent(ir.Names.ObjectClass)),
                  Nil,
                  None,
                  None,
                  fields = Nil,
                  methods = forwarders,
                  jsConstructor = None,
                  jsMethodProps = Nil,
                  jsNativeMembers = Nil,
                  topLevelExportDefs = Nil
              )(js.OptimizerHints.empty)
              generatedStaticForwarderClasses += sym -> forwardersClassDef
            }
          }
          allMethodsExceptStaticForwarders
        } else {
          val forwarders = genStaticForwardersForClassOrInterface(
              allMethodsExceptStaticForwarders, sym)
          allMethodsExceptStaticForwarders ::: forwarders
        }
      }

      // The complete class definition
      val kind =
        if (isStaticModule(sym)) ClassKind.ModuleClass
        else if (isHijacked) ClassKind.HijackedClass
        else ClassKind.Class

      js.ClassDef(
          classIdent,
          originalName,
          kind,
          None,
          Some(encodeClassNameIdent(sym.superClass)),
          genClassInterfaces(sym, forJSClass = false),
          None,
          None,
          fields,
          allMethods,
          jsConstructor = None,
          memberExports,
          jsNativeMembers,
          topLevelExportDefs)(
          optimizerHints)
    }

    /** Gen the IR ClassDef for a non-native JS class. */
    def genNonNativeJSClass(cd: ClassDef): js.ClassDef = {
      val sym = cd.symbol
      implicit val pos = sym.pos

      assert(isNonNativeJSClass(sym),
          "genNonNativeJSClass() must be called only for " +
          s"non-native JS classes: $sym")
      assert(sym.superClass != NoSymbol, sym)

      if (hasDefaultCtorArgsAndJSModule(sym)) {
        reporter.error(pos,
            "Implementation restriction: constructors of " +
            "non-native JS classes cannot have default parameters " +
            "if their companion module is JS native.")
      }

      val classIdent = encodeClassNameIdent(sym)

      // Generate members (constructor + methods)

      val constructorTrees = new ListBuffer[DefDef]
      val generatedMethods = new ListBuffer[js.MethodDef]
      val dispatchMethodNames = new ListBuffer[JSName]

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
            } else if (exposed && sym.isAccessor && !sym.isLazy) {
              /* Exposed accessors must not be emitted, since the field they
               * access is enough.
               */
            } else if (sym.hasAnnotation(JSOptionalAnnotation)) {
              // Optional methods must not be emitted
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

      // Static members (exported from the companion object)
      val (staticFields, staticExports) = {
        /* Phase travel is necessary for non-top-level classes, because flatten
         * breaks their companionModule. This is tracked upstream at
         * https://github.com/scala/scala-dev/issues/403
         */
        val companionModuleClass =
          exitingPhase(currentRun.picklerPhase)(sym.linkedClassOfClass)
        if (companionModuleClass == NoSymbol) {
          (Nil, Nil)
        } else {
          val (staticFields, staticExports) = {
            withScopedVars(currentClassSym := companionModuleClass) {
              genStaticExports(companionModuleClass)
            }
          }

          if (staticFields.nonEmpty) {
            generatedMethods += genStaticConstructorWithStats(
                ir.Names.ClassInitializerName, genLoadModule(companionModuleClass))
          }

          (staticFields, staticExports)
        }
      }

      val topLevelExports = genTopLevelExports(sym)

      val (generatedCtor, jsClassCaptures) = withNewLocalNameScope {
        val isNested = isNestedJSClass(sym)

        if (isNested)
          reserveLocalName(JSSuperClassParamName)

        val (captures, ctor) =
          genJSClassCapturesAndConstructor(constructorTrees.toList)

        val jsClassCaptures = {
          if (isNested) {
            val superParam = js.ParamDef(
                js.LocalIdent(JSSuperClassParamName),
                NoOriginalName, jstpe.AnyType, mutable = false)

            Some(superParam :: captures)
          } else {
            assert(captures.isEmpty,
                s"found non nested JS class with captures $captures at $pos")
            None
          }
        }

        (ctor, jsClassCaptures)
      }

      // Generate fields (and add to methods + ctors)
      val fields = genClassFields(cd)

      val jsMethodProps =
        genJSClassDispatchers(sym, dispatchMethodNames.result().distinct) ::: staticExports

      // The complete class definition
      val kind =
        if (isStaticModule(sym)) ClassKind.JSModuleClass
        else ClassKind.JSClass

      js.ClassDef(
          classIdent,
          originalNameOfClass(sym),
          kind,
          jsClassCaptures,
          Some(encodeClassNameIdent(sym.superClass)),
          genClassInterfaces(sym, forJSClass = true),
          jsSuperClass = jsClassCaptures.map(_.head.ref),
          None,
          fields ::: staticFields,
          generatedMethods.toList,
          Some(generatedCtor),
          jsMethodProps,
          jsNativeMembers = Nil,
          topLevelExports)(
          OptimizerHints.empty)
    }

    /** Generate an instance of an anonymous (non-lambda) JS class inline
     *
     *  @param sym Class to generate the instance of
     *  @param jsSuperClassValue JS class value of the super class
     *  @param args Arguments to the Scala constructor, which map to JS class captures
     *  @param pos Position of the original New tree
     */
    def genAnonJSClassNew(sym: Symbol, jsSuperClassValue: js.Tree,
        args: List[js.Tree])(
        implicit pos: Position): js.Tree = {
      assert(isAnonymousJSClass(sym),
          "Generating AnonJSClassNew of non anonymous JS class")

      // Find the ClassDef for this anonymous class
      val classDef = consumeLazilyGeneratedAnonClass(sym)

      // Generate a normal, non-native JS class
      val origJsClass =
        nestedGenerateClass(sym)(genNonNativeJSClass(classDef))

      // Partition class members.
      val privateFieldDefs = ListBuffer.empty[js.FieldDef]
      val jsFieldDefs = ListBuffer.empty[js.JSFieldDef]

      origJsClass.fields.foreach {
        case fdef: js.FieldDef =>
          privateFieldDefs += fdef

        case fdef: js.JSFieldDef =>
          jsFieldDefs += fdef
      }

      assert(origJsClass.jsNativeMembers.isEmpty,
          "Found JS native members in anonymous JS class at " + pos)

      assert(origJsClass.topLevelExportDefs.isEmpty,
          "Found top-level exports in anonymous JS class at " + pos)

      // Make new class def with static members
      val newClassDef = {
        implicit val pos = origJsClass.pos
        val parent = js.ClassIdent(ir.Names.ObjectClass)
        js.ClassDef(origJsClass.name, origJsClass.originalName,
            ClassKind.AbstractJSType, None, Some(parent), interfaces = Nil,
            jsSuperClass = None, jsNativeLoadSpec = None, fields = Nil,
            methods = origJsClass.methods, jsConstructor = None, jsMethodProps = Nil,
            jsNativeMembers = Nil, topLevelExportDefs = Nil)(
            origJsClass.optimizerHints)
      }

      generatedClasses += newClassDef -> pos

      // Construct inline class definition

      val jsClassCaptures = origJsClass.jsClassCaptures.getOrElse {
        throw new AssertionError(
            s"no class captures for anonymous JS class at $pos")
      }
      val js.JSConstructorDef(_, ctorParams, ctorRestParam, ctorBody) = origJsClass.jsConstructor.getOrElse {
        throw new AssertionError("No ctor found")
      }
      assert(ctorParams.isEmpty && ctorRestParam.isEmpty,
          s"non-empty constructor params for anonymous JS class at $pos")

      /* The first class capture is always a reference to the super class.
       * This is enforced by genJSClassCapturesAndConstructor.
       */
      def jsSuperClassRef(implicit pos: ir.Position): js.VarRef =
        jsClassCaptures.head.ref

      /* The `this` reference.
       * FIXME This could clash with a local variable of the constructor or a JS
       * class capture. How do we avoid this?
       */
      val selfName = freshLocalIdent("this")(pos)
      def selfRef(implicit pos: ir.Position) =
        js.VarRef(selfName)(jstpe.AnyType)

      def memberLambda(params: List[js.ParamDef], restParam: Option[js.ParamDef],
          body: js.Tree)(implicit pos: ir.Position) = {
        js.Closure(arrow = false, captureParams = Nil, params, restParam, body,
            captureValues = Nil)
      }

      val fieldDefinitions = jsFieldDefs.toList.map { fdef =>
        implicit val pos = fdef.pos
        js.Assign(js.JSSelect(selfRef, fdef.name), jstpe.zeroOf(fdef.ftpe))
      }

      val memberDefinitions0 = origJsClass.jsMethodProps.toList.map {
        case mdef: js.JSMethodDef =>
          implicit val pos = mdef.pos
          val impl = memberLambda(mdef.args, mdef.restParam, mdef.body)
          js.Assign(js.JSSelect(selfRef, mdef.name), impl)

        case pdef: js.JSPropertyDef =>
          implicit val pos = pdef.pos
          val optGetter = pdef.getterBody.map { body =>
            js.StringLiteral("get") -> memberLambda(params = Nil, restParam = None, body)
          }
          val optSetter = pdef.setterArgAndBody.map { case (arg, body) =>
            js.StringLiteral("set") -> memberLambda(params = arg :: Nil, restParam = None, body)
          }
          val descriptor = js.JSObjectConstr(
              optGetter.toList :::
              optSetter.toList :::
              List(js.StringLiteral("configurable") -> js.BooleanLiteral(true))
          )
          js.JSMethodApply(js.JSGlobalRef("Object"),
              js.StringLiteral("defineProperty"),
              List(selfRef, pdef.name, descriptor))
      }

      val memberDefinitions1 = fieldDefinitions ::: memberDefinitions0

      val memberDefinitions = if (privateFieldDefs.isEmpty) {
        memberDefinitions1
      } else {
        /* Private fields, declared in FieldDefs, are stored in a separate
         * object, itself stored as a non-enumerable field of the `selfRef`.
         * The name of that field is retrieved at
         * `scala.scalajs.runtime.privateFieldsSymbol()`, and is a Symbol if
         * supported, or a randomly generated string that has the same enthropy
         * as a UUID (i.e., 128 random bits).
         *
         * This encoding solves two issues:
         *
         * - Hide private fields in anonymous JS classes from `JSON.stringify`
         *   and other cursory inspections in JS (#2748).
         * - Get around the fact that abstract JS types cannot declare
         *   FieldDefs (#3777).
         */
        val fieldsObjValue = {
          js.JSObjectConstr(privateFieldDefs.toList.map { fdef =>
            implicit val pos = fdef.pos
            js.StringLiteral(fdef.name.name.nameString) -> jstpe.zeroOf(fdef.ftpe)
          })
        }
        val definePrivateFieldsObj = {
          /* Object.defineProperty(selfRef, privateFieldsSymbol, {
           *   value: fieldsObjValue
           * });
           *
           * `writable`, `configurable` and `enumerable` are false by default.
           */
          js.JSMethodApply(
              js.JSGlobalRef("Object"),
              js.StringLiteral("defineProperty"),
              List(
                  selfRef,
                  genPrivateFieldsSymbol(),
                  js.JSObjectConstr(List(
                      js.StringLiteral("value") -> fieldsObjValue))
              )
          )
        }
        definePrivateFieldsObj :: memberDefinitions1
      }

      // Transform the constructor body.
      val inlinedCtorStats = {
        val beforeSuper = ctorBody.beforeSuper

        val superCall = {
          implicit val pos = ctorBody.superCall.pos
          val js.JSSuperConstructorCall(args) = ctorBody.superCall

          val newTree = {
            val ident =
              origJsClass.superClass.getOrElse(abort("No superclass"))
            if (args.isEmpty && ident.name == JSObjectClassName)
              js.JSObjectConstr(Nil)
            else
              js.JSNew(jsSuperClassRef, args)
          }

          val selfVarDef = js.VarDef(selfName, thisOriginalName, jstpe.AnyType, mutable = false, newTree)
          selfVarDef :: memberDefinitions
        }

        // After the super call, substitute `selfRef` for `This()`
        val afterSuper = new ir.Transformers.Transformer {
          override def transform(tree: js.Tree, isStat: Boolean): js.Tree = tree match {
            case js.This() =>
              selfRef(tree.pos)

            // Don't traverse closure boundaries
            case closure: js.Closure =>
              val newCaptureValues = closure.captureValues.map(transformExpr)
              closure.copy(captureValues = newCaptureValues)(closure.pos)

            case tree =>
              super.transform(tree, isStat)
          }
        }.transformStats(ctorBody.afterSuper)

        beforeSuper ::: superCall ::: afterSuper
      }

      val closure = js.Closure(arrow = true, jsClassCaptures, Nil, None,
          js.Block(inlinedCtorStats, selfRef), jsSuperClassValue :: args)
      js.JSFunctionApply(closure, Nil)
    }

    // Generate the class data of a JS class -----------------------------------

    /** Gen the IR ClassDef for a JS class or trait.
     */
    def genJSClassData(cd: ClassDef): js.ClassDef = {
      val sym = cd.symbol
      implicit val pos = sym.pos

      val classIdent = encodeClassNameIdent(sym)
      val kind = {
        if (sym.isTraitOrInterface) ClassKind.AbstractJSType
        else if (isJSFunctionDef(sym)) ClassKind.AbstractJSType
        else if (sym.isModuleClass) ClassKind.NativeJSModuleClass
        else ClassKind.NativeJSClass
      }
      val superClass =
        if (sym.isTraitOrInterface) None
        else Some(encodeClassNameIdent(sym.superClass))
      val jsNativeLoadSpec = jsNativeLoadSpecOfOption(sym)

      js.ClassDef(classIdent, originalNameOfClass(sym), kind, None, superClass,
          genClassInterfaces(sym, forJSClass = true), None, jsNativeLoadSpec,
          Nil, Nil, None, Nil, Nil, Nil)(
          OptimizerHints.empty)
    }

    // Generate an interface ---------------------------------------------------

    /** Gen the IR ClassDef for an interface definition.
     */
    def genInterface(cd: ClassDef): js.ClassDef = {
      val sym = cd.symbol
      implicit val pos = sym.pos

      val classIdent = encodeClassNameIdent(sym)

      // fill in class info builder
      def gen(tree: Tree): List[js.MethodDef] = {
        tree match {
          case EmptyTree            => Nil
          case Template(_, _, body) => body.flatMap(gen)

          case dd: DefDef =>
            genMethod(dd).toList

          case _ =>
            abort("Illegal tree in gen of genInterface(): " + tree)
        }
      }
      val generatedMethods = gen(cd.impl)
      val interfaces = genClassInterfaces(sym, forJSClass = false)

      val allMemberDefs =
        if (!isCandidateForForwarders(sym)) generatedMethods
        else generatedMethods ::: genStaticForwardersForClassOrInterface(generatedMethods, sym)

      js.ClassDef(classIdent, originalNameOfClass(sym), ClassKind.Interface,
          None, None, interfaces, None, None, fields = Nil, methods = allMemberDefs,
          None, Nil, Nil, Nil)(
          OptimizerHints.empty)
    }

    private lazy val jsTypeInterfacesBlacklist: Set[Symbol] =
      Set(DynamicClass, SerializableClass) // #3118, #3252

    private def genClassInterfaces(sym: Symbol, forJSClass: Boolean)(
        implicit pos: Position): List[js.ClassIdent] = {

      val blacklist =
        if (forJSClass) jsTypeInterfacesBlacklist
        else Set.empty[Symbol]

      for {
        parent <- sym.info.parents
        typeSym = parent.typeSymbol
        _ = assert(typeSym != NoSymbol, "parent needs symbol")
        if typeSym.isTraitOrInterface && !blacklist.contains(typeSym)
      } yield {
        encodeClassNameIdent(typeSym)
      }
    }

    // Static forwarders -------------------------------------------------------

    /* This mimics the logic in BCodeHelpers.addForwarders and the code that
     * calls it, except that we never have collisions with existing methods in
     * the companion class. This is because in the IR, only methods with the
     * same `MethodName` (including signature) and that are also
     * `PublicStatic` would collide. Since we never emit any `PublicStatic`
     * method otherwise, there can be no collision. If that assumption is broken,
     *  an error message is emitted asking the user to report a bug.
     *
     * It is important that we always emit forwarders, because some Java APIs
     * actually have a public static method and a public instance method with
     * the same name. For example the class `Integer` has a
     * `def hashCode(): Int` and a `static def hashCode(Int): Int`. The JVM
     * back-end considers them as colliding because they have the same name,
     * but we must not.
     *
     * By default, we only emit forwarders for top-level objects, like scalac.
     * However, if requested via a compiler option, we enable them for all
     * static objects. This is important so we can implement static methods
     * of nested static classes of JDK APIs (see #3950).
     */

    /** Is the given Scala class, interface or module class a candidate for
     *  static forwarders?
     */
    def isCandidateForForwarders(sym: Symbol): Boolean = {
      !settings.noForwarders.value && sym.isStatic && {
        // Reject non-top-level objects unless opted in via the appropriate option
        scalaJSOpts.genStaticForwardersForNonTopLevelObjects ||
        !sym.name.containsChar('$') // this is the same test that scalac performs
      }
    }

    /** Gen the static forwarders to the members of a class or interface for
     *  methods of its companion object.
     *
     *  This is only done if there exists a companion object and it is not a JS
     *  type.
     *
     *  Precondition: `isCandidateForForwarders(sym)` is true
     */
    def genStaticForwardersForClassOrInterface(
        existingMethods: List[js.MethodDef], sym: Symbol)(
        implicit pos: Position): List[js.MethodDef] = {
      /* Phase travel is necessary for non-top-level classes, because flatten
       * breaks their companionModule. This is tracked upstream at
       * https://github.com/scala/scala-dev/issues/403
       */
      val module = exitingPhase(currentRun.picklerPhase)(sym.companionModule)
      if (module == NoSymbol) {
        Nil
      } else {
        val moduleClass = module.moduleClass
        if (!isJSType(moduleClass))
          genStaticForwardersFromModuleClass(existingMethods, moduleClass)
        else
          Nil
      }
    }

    /** Gen the static forwarders for the methods of a module class.
     *
     *  Precondition: `isCandidateForForwarders(moduleClass)` is true
     */
    def genStaticForwardersFromModuleClass(existingMethods: List[js.MethodDef],
        moduleClass: Symbol)(
        implicit pos: Position): List[js.MethodDef] = {

      assert(moduleClass.isModuleClass, moduleClass)

      val hasAnyExistingPublicStaticMethod =
        existingMethods.exists(_.flags.namespace == js.MemberNamespace.PublicStatic)
      if (hasAnyExistingPublicStaticMethod) {
        reporter.error(pos,
            "Unexpected situation: found existing public static methods in " +
            s"the class ${moduleClass.fullName} while trying to generate " +
            "static forwarders for its companion object. " +
            "Please report this as a bug in Scala.js.")
      }

      def listMembersBasedOnFlags = {
        // Copy-pasted from BCodeHelpers.
        val ExcludedForwarderFlags: Long = {
          import scala.tools.nsc.symtab.Flags._
          SPECIALIZED | LIFTED | PROTECTED | STATIC | EXPANDEDNAME | PRIVATE | MACRO
        }

        moduleClass.info.membersBasedOnFlags(ExcludedForwarderFlags, symtab.Flags.METHOD)
      }

      // See BCodeHelprs.addForwarders in 2.12+ for why we use exitingUncurry.
      val members = exitingUncurry(listMembersBasedOnFlags)

      def isExcluded(m: Symbol): Boolean = {
        def isOfJLObject: Boolean = {
          val o = m.owner
          (o eq ObjectClass) || (o eq AnyRefClass) || (o eq AnyClass)
        }

        def isDefaultParamOfJSNativeDef: Boolean = {
          DefaultParamInfo.isApplicable(m) && {
            val info = new DefaultParamInfo(m)
            !info.isForConstructor && info.attachedMethod.hasAnnotation(JSNativeAnnotation)
          }
        }

        m.isDeferred || m.isConstructor || m.hasAccessBoundary ||
        isOfJLObject ||
        m.hasAnnotation(JSNativeAnnotation) || isDefaultParamOfJSNativeDef // #4557
      }

      val forwarders = for {
        m <- members
        if !isExcluded(m)
      } yield {
        withNewLocalNameScope {
          val flags = js.MemberFlags.empty.withNamespace(js.MemberNamespace.PublicStatic)
          val methodIdent = encodeMethodSym(m)
          val originalName = originalNameOfMethod(m)
          val jsParams = m.tpe.params.map(genParamDef(_))
          val resultType = toIRType(m.tpe.resultType)

          js.MethodDef(flags, methodIdent, originalName, jsParams, resultType, Some {
            genApplyMethod(genLoadModule(moduleClass), m, jsParams.map(_.ref))
          })(OptimizerHints.empty, Unversioned)
        }
      }

      forwarders.toList
    }

    // Generate the fields of a class ------------------------------------------

    /** Gen definitions for the fields of a class.
     *  The fields are initialized with the zero of their types.
     */
    def genClassFields(cd: ClassDef): List[js.AnyFieldDef] = {
      val classSym = cd.symbol
      assert(currentClassSym.get == classSym,
          "genClassFields called with a ClassDef other than the current one")

      val isJSClass = isNonNativeJSClass(classSym)

      // Non-method term members are fields, except for module members.
      (for {
        f <- classSym.info.decls
        if !f.isMethod && f.isTerm && !f.isModule
        if !f.hasAnnotation(JSOptionalAnnotation) && !f.hasAnnotation(JSNativeAnnotation)
        if jsInterop.staticExportsOf(f).isEmpty
      } yield {
        implicit val pos = f.pos

        val static = jsInterop.topLevelExportsOf(f).nonEmpty

        val mutable = {
          static || // static fields must always be mutable
          f.isMutable || // mutable fields can be mutated from anywhere
          fieldsMutatedInCurrentClass.contains(f.name) // the field is mutated in the current class
        }

        val namespace =
          if (static) js.MemberNamespace.PublicStatic
          else js.MemberNamespace.Public
        val flags =
          js.MemberFlags.empty.withNamespace(namespace).withMutable(mutable)

        val irTpe0 = {
          if (isJSClass) genExposedFieldIRType(f)
          else if (static) jstpe.AnyType
          else toIRType(f.tpe)
        }

        // #4370 Fields cannot have type NothingType
        val irTpe =
          if (irTpe0 == jstpe.NothingType) encodeClassType(RuntimeNothingClass)
          else irTpe0

        if (isJSClass && isExposed(f))
          js.JSFieldDef(flags, genExpr(jsNameOf(f)), irTpe)
        else
          js.FieldDef(flags, encodeFieldSym(f), originalNameOfField(f), irTpe)
      }).toList
    }

    def genExposedFieldIRType(f: Symbol): jstpe.Type = {
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
          jstpe.ClassType(encodeClassName(tpe.valueClazz))

        case _ =>
          /* Other types are not boxed, so we can initialize them to
           * their true zero.
           */
          toIRType(f.tpe)
      }
    }

    // Static initializers -----------------------------------------------------

    private def genStaticConstructorWithStats(name: MethodName, stats: js.Tree)(
        implicit pos: Position): js.MethodDef = {
      js.MethodDef(
          js.MemberFlags.empty.withNamespace(js.MemberNamespace.StaticConstructor),
          js.MethodIdent(name),
          NoOriginalName,
          Nil,
          jstpe.NoType,
          Some(stats))(
          OptimizerHints.empty, Unversioned)
    }

    private def genRegisterReflectiveInstantiation(sym: Symbol)(
        implicit pos: Position): Option[js.Tree] = {
      if (isStaticModule(sym))
        genRegisterReflectiveInstantiationForModuleClass(sym)
      else if (sym.isModuleClass)
        None // #3228
      else if (sym.isLifted && !sym.originalOwner.isClass)
        None // #3227
      else
        genRegisterReflectiveInstantiationForNormalClass(sym)
    }

    private def genRegisterReflectiveInstantiationForModuleClass(sym: Symbol)(
        implicit pos: Position): Option[js.Tree] = {
      val fqcnArg = js.StringLiteral(sym.fullName + "$")
      val runtimeClassArg = js.ClassOf(toTypeRef(sym.info))
      val loadModuleFunArg =
        js.Closure(arrow = true, Nil, Nil, None, genLoadModule(sym), Nil)

      val stat = genApplyMethod(
          genLoadModule(ReflectModule),
          Reflect_registerLoadableModuleClass,
          List(fqcnArg, runtimeClassArg, loadModuleFunArg))

      Some(stat)
    }

    private def genRegisterReflectiveInstantiationForNormalClass(sym: Symbol)(
        implicit pos: Position): Option[js.Tree] = {
      val ctors =
        if (sym.isAbstractClass) Nil
        else sym.info.member(nme.CONSTRUCTOR).alternatives.filter(_.isPublic)

      if (ctors.isEmpty) {
        None
      } else {
        val constructorsInfos = for {
          ctor <- ctors
        } yield {
          withNewLocalNameScope {
            val (parameterTypes, formalParams, actualParams) = (for {
              param <- ctor.tpe.params
            } yield {
              /* Note that we do *not* use `param.tpe` entering posterasure
               * (neither to compute `paramType` nor to give to `fromAny`).
               * Logic would tell us that we should do so, but we intentionally
               * do not to preserve the behavior on the JVM regarding value
               * classes. If a constructor takes a value class as parameter, as
               * in:
               *
               *   class ValueClass(val underlying: Int) extends AnyVal
               *   class Foo(val vc: ValueClass)
               *
               * then, from a reflection point of view, on the JVM, the
               * constructor of `Foo` takes an `Int`, not a `ValueClas`. It
               * must therefore be identified as the constructor whose
               * parameter types is `List(classOf[Int])`, and when invoked
               * reflectively, it must be given an `Int` (or `Integer`).
               */
              val paramType = js.ClassOf(toTypeRef(param.tpe))
              val paramDef = genParamDef(param, jstpe.AnyType)
              val actualParam = fromAny(paramDef.ref, param.tpe)
              (paramType, paramDef, actualParam)
            }).unzip3

            val paramTypesArray = js.JSArrayConstr(parameterTypes)

            val newInstanceFun = js.Closure(arrow = true, Nil, formalParams, None, {
              genNew(sym, ctor, actualParams)
            }, Nil)

            js.JSArrayConstr(List(paramTypesArray, newInstanceFun))
          }
        }

        val fqcnArg = js.StringLiteral(sym.fullName)
        val runtimeClassArg = js.ClassOf(toTypeRef(sym.info))
        val ctorsInfosArg = js.JSArrayConstr(constructorsInfos)

        val stat = genApplyMethod(
            genLoadModule(ReflectModule),
            Reflect_registerInstantiatableClass,
            List(fqcnArg, runtimeClassArg, ctorsInfosArg))

        Some(stat)
      }
    }

    // Constructor of a non-native JS class ------------------------------

    def genJSClassCapturesAndConstructor(constructorTrees: List[DefDef])(
        implicit pos: Position): (List[js.ParamDef], js.JSConstructorDef) = {
      /* We need to merge all Scala constructors into a single one because
       * JavaScript only allows a single one.
       *
       * We do this by applying:
       * 1. Applying runtime type based dispatch, just like exports.
       * 2. Splitting secondary ctors into parts before and after the `this` call.
       * 3. Topo-sorting all constructor statements and including/excluding
       *    them based on the overload that was chosen.
       */

      val (primaryTree :: Nil, secondaryTrees) =
        constructorTrees.partition(_.symbol.isPrimaryConstructor)

      val primaryCtor = genPrimaryJSClassCtor(primaryTree)
      val secondaryCtors = secondaryTrees.map(genSecondaryJSClassCtor(_))

      // VarDefs for the parameters of all constructors.
      val paramVarDefs = for {
        vparam <- constructorTrees.flatMap(_.vparamss.flatten)
      } yield {
        val sym = vparam.symbol
        val tpe = toIRType(sym.tpe)
        js.VarDef(encodeLocalSym(sym), originalNameOfLocal(sym), tpe, mutable = true,
            jstpe.zeroOf(tpe))(vparam.pos)
      }

      /* organize constructors in a called-by tree
       * (the implicit root is the primary constructor)
       */
      val ctorTree = {
        val ctorToChildren = secondaryCtors
          .groupBy(_.targetCtor)
          .withDefaultValue(Nil)

        /* when constructing the call-by tree, we use pre-order traversal to
         * assign overload numbers.
         * this puts all descendants of a ctor in a range of overloads numbers.
         *
         * this property is useful, later, when we need to make statements
         * conditional based on the chosen overload.
         */
        var nextOverloadNum = 0
        def subTree[T <: JSCtor](ctor: T): ConstructorTree[T] = {
          val overloadNum = nextOverloadNum
          nextOverloadNum += 1
          val subtrees = ctorToChildren(ctor.sym).map(subTree(_))
          new ConstructorTree(overloadNum, ctor, subtrees)
        }

        subTree(primaryCtor)
      }

      /* prepare overload dispatch for all constructors.
       * as a side-product, we retrieve the capture parameters.
       */
      val (exports, jsClassCaptures) = {
        val exports = List.newBuilder[Exported]
        val jsClassCaptures = List.newBuilder[js.ParamDef]

        def add(tree: ConstructorTree[_ <: JSCtor]): Unit = {
          val (e, c) = genJSClassCtorDispatch(tree.ctor.sym,
              tree.ctor.paramsAndInfo, tree.overloadNum)
          exports += e
          jsClassCaptures ++= c
          tree.subCtors.foreach(add(_))
        }

        add(ctorTree)

        (exports.result(), jsClassCaptures.result())
      }

      // The name 'constructor' is used for error reporting here
      val (formalArgs, restParam, overloadDispatchBody) =
        genOverloadDispatch(JSName.Literal("constructor"), exports, jstpe.IntType)

      val overloadVar = js.VarDef(freshLocalIdent("overload"), NoOriginalName,
          jstpe.IntType, mutable = false, overloadDispatchBody)

      val constructorBody = wrapJSCtorBody(
        paramVarDefs :+ overloadVar,
        genJSClassCtorBody(overloadVar.ref, ctorTree),
        js.Undefined() :: Nil
      )

      val constructorDef = js.JSConstructorDef(
          js.MemberFlags.empty.withNamespace(js.MemberNamespace.Constructor),
          formalArgs, restParam, constructorBody)(OptimizerHints.empty, Unversioned)

      (jsClassCaptures, constructorDef)
    }

    private def genPrimaryJSClassCtor(dd: DefDef): PrimaryJSCtor = {
      val DefDef(_, _, _, vparamss, _, Block(stats, _)) = dd
      val sym = dd.symbol
      assert(sym.isPrimaryConstructor, s"called with non-primary ctor: $sym")

      var preSuperStats = List.newBuilder[js.Tree]
      var jsSuperCall: Option[js.JSSuperConstructorCall] = None
      val postSuperStats = List.newBuilder[js.Tree]

      /* Move param accessor initializers and early initializers after the
       * super constructor call since JS cannot access `this` before the super
       * constructor call.
       *
       * scalac inserts statements before the super constructor call for early
       * initializers and param accessor initializers (including val's and var's
       * declared in the params). Those statements include temporary local `val`
       * definitions (for true early initializers only) and the assignments,
       * whose rhs'es are always simple Idents (either constructor params or the
       * temporary local `val`s).
       *
       * There can also be local `val`s before the super constructor call for
       * default arguments to the super constructor. These must remain before.
       *
       * Our strategy is therefore to move only the field assignments after the
       * super constructor call. They are therefore executed later than for a
       * Scala class (as specified for non-native JS classes semantics).
       * However, side effects and evaluation order of all the other
       * computations remains unchanged.
       *
       * For a somewhat extreme example of the shapes we can get here, consider
       * the source code:
       *
       *   class Parent(output: Any = "output", callbackObject: Any = "callbackObject") extends js.Object {
       *     println(s"Parent constructor; $output; $callbackObject")
       *   }
       *
       *   class Child(val foo: Int, callbackObject: Any, val bar: Int) extends {
       *     val xyz = foo + bar
       *     val yz = { println(xyz); xyz + 2 }
       *   } with Parent(callbackObject = { println(foo); xyz + bar }) {
       *     println("Child constructor")
       *     println(xyz)
       *   }
       *
       * At this phase, for the constructor of `Child`, we receive the following
       * scalac Tree:
       *
       *   def <init>(foo: Int, callbackObject: Object, bar: Int): helloworld.Child = {
       *     Child.this.foo = foo; // param accessor assignment, moved
       *     Child.this.bar = bar; // param accessor assignment, moved
       *     val xyz: Int = foo.+(bar); // note that these still use the ctor params, not the fields
       *     Child.this.xyz = xyz; // early initializer, moved
       *     val yz: Int = {
       *       scala.Predef.println(scala.Int.box(xyz)); // note that this uses the local val, not the field
       *       xyz.+(2)
       *     };
       *     Child.this.yz = yz; // early initializer, moved
       *     {
       *       <artifact> val x$1: Int = {
       *         scala.Predef.println(scala.Int.box(foo));
       *         xyz.+(bar) // here as well, we use the local vals, not the fields
       *       };
       *       <artifact> val x$2: Object = helloworld.this.Parent.<init>$default$1();
       *       Child.super.<init>(x$2, scala.Int.box(x$1))
       *     };
       *     scala.Predef.println("Child constructor");
       *     scala.Predef.println(scala.Int.box(Child.this.xyz()));
       *     ()
       *   }
       *
       */
      withPerMethodBodyState(sym) {
        def isThisFieldAssignment(tree: Tree): Boolean = tree match {
          case Assign(Select(ths: This, _), Ident(_)) => ths.symbol == currentClassSym.get
          case _                                      => false
        }

        flatStats(stats).foreach {
          case tree @ Apply(fun @ Select(Super(This(_), _), _), args)
                if fun.symbol.isClassConstructor =>
            assert(jsSuperCall.isEmpty, s"Found 2 JS Super calls at ${dd.pos}")
            implicit val pos = tree.pos
            jsSuperCall = Some(js.JSSuperConstructorCall(genPrimitiveJSArgs(fun.symbol, args)))

          case tree if jsSuperCall.isDefined =>
            // Once we're past the super constructor call, everything goes after.
            postSuperStats += genStat(tree)

          case tree if isThisFieldAssignment(tree) =>
            /* If that shape appears before the jsSuperCall, it is an early
             * initializer or param accessor initializer. We move it.
             */
            postSuperStats += genStat(tree)

          case tree @ OuterPointerNullCheck(outer, assign) if isThisFieldAssignment(assign) =>
            /* Variant of the above with an outer pointer null check. The actual
             * null check remains before the super call, while the associated
             * assignment is moved after.
             */
            preSuperStats += js.GetClass(genExpr(outer))(tree.pos)
            postSuperStats += genStat(assign)

          case stat =>
            // Other statements are left before.
            preSuperStats += genStat(stat)
        }
      }

      assert(jsSuperCall.isDefined, "Did not find Super call in primary JS " +
          s"construtor at ${dd.pos}")

      new PrimaryJSCtor(sym, genParamsAndInfo(sym, vparamss),
          js.JSConstructorBody(preSuperStats.result(), jsSuperCall.get, postSuperStats.result())(dd.pos))
    }

    private def genSecondaryJSClassCtor(dd: DefDef): SplitSecondaryJSCtor = {
      val DefDef(_, _, _, vparamss, _, Block(stats, _)) = dd
      val sym = dd.symbol
      assert(!sym.isPrimaryConstructor, s"called with primary ctor $sym")

      val beforeThisCall = List.newBuilder[js.Tree]
      var thisCall: Option[(Symbol, List[js.Tree])] = None
      val afterThisCall = List.newBuilder[js.Tree]

      withPerMethodBodyState(sym) {
        flatStats(stats).foreach {
          case tree @ Apply(fun @ Select(This(_), _), args)
              if fun.symbol.isClassConstructor =>
            assert(thisCall.isEmpty,
                s"duplicate this() call in secondary JS constructor at ${dd.pos}")

            implicit val pos = tree.pos
            val sym = fun.symbol
            thisCall = Some((sym, genActualArgs(sym, args)))

          case stat =>
            val jsStat = genStat(stat)
            if (thisCall.isEmpty)
              beforeThisCall += jsStat
            else
              afterThisCall += jsStat
        }
      }

      val Some((targetCtor, ctorArgs)) = thisCall

      new SplitSecondaryJSCtor(sym, genParamsAndInfo(sym, vparamss),
          beforeThisCall.result(), targetCtor, ctorArgs, afterThisCall.result())
    }

    private def genParamsAndInfo(ctorSym: Symbol,
        vparamss: List[List[ValDef]]): List[(js.VarRef, JSParamInfo)] = {
      implicit val pos = ctorSym.pos

      val paramSyms = if (vparamss.isEmpty) Nil else vparamss.head.map(_.symbol)

      for {
        (paramSym, info) <- paramSyms.zip(jsParamInfos(ctorSym))
      } yield {
        genVarRef(paramSym) -> info
      }
    }

    private def genJSClassCtorDispatch(ctorSym: Symbol,
        allParamsAndInfos: List[(js.VarRef, JSParamInfo)],
        overloadNum: Int): (Exported, List[js.ParamDef]) = {
      implicit val pos = ctorSym.pos

      /* `allParams` are the parameters as seen from *inside* the constructor
       * body. the symbols returned in jsParamInfos are the parameters as seen
       * from *outside* (i.e. from a caller).
       *
       * we need to use the symbols from inside to generate the right
       * identifiers (the ones generated by the trees in the constructor body).
       */
      val (captureParamsAndInfos, normalParamsAndInfos) =
        allParamsAndInfos.partition(_._2.capture)

      /* We use the *outer* param symbol to get different names than the *inner*
       * symbols. This is necessary so that we can forward captures properly
       * between constructor delegation calls.
       */
      val jsClassCaptures =
        captureParamsAndInfos.map(x => genParamDef(x._2.sym))

      val normalInfos = normalParamsAndInfos.map(_._2).toIndexedSeq

      val jsExport = new Exported(ctorSym, normalInfos) {
        def genBody(formalArgsRegistry: FormalArgsRegistry): js.Tree = {
          val captureAssigns = for {
            (param, info) <- captureParamsAndInfos
          } yield {
            js.Assign(param, genVarRef(info.sym))
          }

          val paramAssigns = for {
            ((param, info), i) <- normalParamsAndInfos.zipWithIndex
          } yield {
            val rhs = genScalaArg(sym, i, formalArgsRegistry, info, static = true,
                captures = captureParamsAndInfos.map(_._1))(
                prevArgsCount => normalParamsAndInfos.take(prevArgsCount).map(_._1))

            js.Assign(param, rhs)
          }

          js.Block(captureAssigns ::: paramAssigns, js.IntLiteral(overloadNum))
        }
      }

      (jsExport, jsClassCaptures)
    }

    /** Generates a JS constructor body based on a constructor tree. */
    private def genJSClassCtorBody(overloadVar: js.VarRef,
        ctorTree: ConstructorTree[PrimaryJSCtor])(implicit pos: Position): js.JSConstructorBody = {

      /* generates a statement that conditionally executes body iff the chosen
       * overload is any of the descendants of `tree` (including itself).
       *
       * here we use the property from building the trees, that a set of
       * descendants always has a range of overload numbers.
       */
      def ifOverload(tree: ConstructorTree[_], body: js.Tree): js.Tree = body match {
        case js.Skip() => js.Skip()

        case body =>
          val x = overloadVar
          val cond = {
            import tree.{lo, hi}

            if (lo == hi) {
              js.BinaryOp(js.BinaryOp.Int_==, js.IntLiteral(lo), x)
            } else {
              val lhs = js.BinaryOp(js.BinaryOp.Int_<=, js.IntLiteral(lo), x)
              val rhs = js.BinaryOp(js.BinaryOp.Int_<=, x, js.IntLiteral(hi))
              js.If(lhs, rhs, js.BooleanLiteral(false))(jstpe.BooleanType)
            }
          }

          js.If(cond, body, js.Skip())(jstpe.NoType)
      }

      /* preStats / postStats use pre/post order traversal respectively to
       * generate a topo-sorted sequence of statements.
       */

      def preStats(tree: ConstructorTree[SplitSecondaryJSCtor],
          nextParamsAndInfo: List[(js.VarRef, JSParamInfo)]): js.Tree = {
        val inner = tree.subCtors.map(preStats(_, tree.ctor.paramsAndInfo))

        assert(tree.ctor.ctorArgs.size == nextParamsAndInfo.size, "param count mismatch")
        val paramsInfosAndArgs = nextParamsAndInfo.zip(tree.ctor.ctorArgs)

        val (captureParamsInfosAndArgs, normalParamsInfosAndArgs) =
          paramsInfosAndArgs.partition(_._1._2.capture)

        val captureAssigns = for {
          ((param, _), arg) <- captureParamsInfosAndArgs
        } yield {
          js.Assign(param, arg)
        }

        val normalAssigns = for {
          (((param, info), arg), i) <- normalParamsInfosAndArgs.zipWithIndex
        } yield {
          val newArg = arg match {
            case js.Transient(UndefinedParam) =>
              assert(info.hasDefault,
                  s"unexpected UndefinedParam for non default param: $param")

              /* Go full circle: We have ignored the default param getter for
               * this, we'll create it again.
               *
               * This seems not optimal: We could simply not ignore the calls to
               * default param getters in the first place.
               *
               * However, this proves to be difficult: Because of translations in
               * earlier phases, calls to default param getters may be assigned
               * to temporary variables first (see the undefinedDefaultParams
               * ScopedVar). If this happens, it becomes increasingly difficult
               * to distinguish a default param getter call for a constructor
               * call of *this* instance (in which case we would want to keep
               * the default param getter call) from one for a *different*
               * instance (in which case we would want to discard the default
               * param getter call)
               *
               * Because of this, it ends up being easier to just re-create the
               * default param getter call if necessary.
               */
              genCallDefaultGetter(tree.ctor.sym, i, tree.ctor.sym.pos, static = false,
                  captures = captureParamsInfosAndArgs.map(_._1._1))(
                  prevArgsCount => normalParamsInfosAndArgs.take(prevArgsCount).map(_._1._1))

            case arg => arg
          }

          js.Assign(param, newArg)
        }

        ifOverload(tree, js.Block(
            inner ++ tree.ctor.beforeCall ++ captureAssigns ++ normalAssigns))
      }

      def postStats(tree: ConstructorTree[SplitSecondaryJSCtor]): js.Tree = {
        val inner = tree.subCtors.map(postStats(_))
        ifOverload(tree, js.Block(tree.ctor.afterCall ++ inner))
      }

      val primaryCtor = ctorTree.ctor
      val secondaryCtorTrees = ctorTree.subCtors

      wrapJSCtorBody(
          secondaryCtorTrees.map(preStats(_, primaryCtor.paramsAndInfo)),
          primaryCtor.body,
          secondaryCtorTrees.map(postStats(_))
      )
    }

    private def wrapJSCtorBody(before: List[js.Tree], body: js.JSConstructorBody,
        after: List[js.Tree]): js.JSConstructorBody = {
      js.JSConstructorBody(before ::: body.beforeSuper, body.superCall,
          body.afterSuper ::: after)(body.pos)
    }

    private sealed trait JSCtor {
      val sym: Symbol
      val paramsAndInfo: List[(js.VarRef, JSParamInfo)]
    }

    private class PrimaryJSCtor(val sym: Symbol,
        val paramsAndInfo: List[(js.VarRef, JSParamInfo)],
        val body: js.JSConstructorBody) extends JSCtor

    private class SplitSecondaryJSCtor(val sym: Symbol,
        val paramsAndInfo: List[(js.VarRef, JSParamInfo)],
        val beforeCall: List[js.Tree],
        val targetCtor: Symbol, val ctorArgs: List[js.Tree],
        val afterCall: List[js.Tree]) extends JSCtor

    private class ConstructorTree[Ctor <: JSCtor](
        val overloadNum: Int, val ctor: Ctor,
        val subCtors: List[ConstructorTree[SplitSecondaryJSCtor]]) {
      val lo: Int = overloadNum
      val hi: Int = subCtors.lastOption.fold(lo)(_.hi)

      assert(lo <= hi, "bad overload range")
    }

    // Generate a method -------------------------------------------------------

    /** Maybe gen JS code for a method definition.
     *
     *  Some methods are not emitted at all:
     *
     *  - Primitives, since they are never actually called (with exceptions)
     *  - Abstract methods in non-native JS classes
     *  - Default accessor of a native JS constructor
     *  - Constructors of hijacked classes
     */
    def genMethod(dd: DefDef): Option[js.MethodDef] = {
      val sym = dd.symbol
      val isAbstract = isAbstractMethod(dd)

      /* Is this method a default accessor that should be ignored?
       *
       * This is the case iff one of the following applies:
       * - It is a constructor default accessor and the linked class is a
       *   native JS class.
       * - It is a default accessor for a native JS def, but with the caveat
       *   that its rhs must be `js.native` because of #4553.
       *
       * Both of those conditions can only happen if the default accessor is in
       * a module class, so we use that as a fast way out. (But omitting that
       * condition would not change the result.)
       *
       * This is different than `isJSDefaultParam` in `genApply`: we do not
       * ignore default accessors of *non-native* JS types. Neither for
       * constructor default accessor nor regular default accessors. We also
       * do not need to worry about non-constructor members of native JS types,
       * since for those, the entire member list is ignored in `genJSClassData`.
       */
      def isIgnorableDefaultParam: Boolean = {
        DefaultParamInfo.isApplicable(sym) && sym.owner.isModuleClass && {
          val info = new DefaultParamInfo(sym)
          if (info.isForConstructor) {
            /* This is a default accessor for a constructor parameter. Check
             * whether the attached constructor is a native JS constructor,
             * which is the case iff the linked class is a native JS type.
             */
            isJSNativeClass(info.constructorOwner)
          } else {
            /* #4553 We need to ignore default accessors for JS native defs.
             * However, because Scala.js <= 1.7.0 actually emitted code calling
             * those accessors, we must keep default accessors that would
             * compile. The only accessors we can actually get rid of are those
             * that are `= js.native`.
             */
            !isJSType(sym.owner) &&
            info.attachedMethod.hasAnnotation(JSNativeAnnotation) && {
              dd.rhs match {
                case MaybeAsInstanceOf(Apply(fun, _)) =>
                  fun.symbol == JSPackage_native
                case _ =>
                  false
              }
            }
          }
        }
      }

      if (scalaPrimitives.isPrimitive(sym)) {
        None
      } else if (isAbstract && isNonNativeJSClass(currentClassSym)) {
        // #4409: Do not emit abstract methods in non-native JS classes
        None
      } else if (isIgnorableDefaultParam) {
        None
      } else if (sym.isClassConstructor && isHijackedClass(sym.owner)) {
        None
      } else {
        withNewLocalNameScope {
          Some(genMethodWithCurrentLocalNameScope(dd))
        }
      }
    }

    /** Gen JS code for a method definition in a class or in an impl class.
     *  On the JS side, method names are mangled to encode the full signature
     *  of the Scala method, as described in `JSEncoding`, to support
     *  overloading.
     *
     *  Constructors are emitted by generating their body as a statement.
     *
     *  Other (normal) methods are emitted with `genMethodDef()`.
     */
    def genMethodWithCurrentLocalNameScope(dd: DefDef): js.MethodDef = {
      implicit val pos = dd.pos
      val sym = dd.symbol

      withPerMethodBodyState(sym) {
        val methodName = encodeMethodSym(sym)
        val originalName = originalNameOfMethod(sym)

        val jsParams = {
          val vparamss = dd.vparamss
          assert(vparamss.isEmpty || vparamss.tail.isEmpty,
              "Malformed parameter list: " + vparamss)
          val params = if (vparamss.isEmpty) Nil else vparamss.head.map(_.symbol)
          params.map(genParamDef(_))
        }

        val jsMethodDef = if (isAbstractMethod(dd)) {
          js.MethodDef(js.MemberFlags.empty, methodName, originalName,
              jsParams, toIRType(sym.tpe.resultType), None)(
              OptimizerHints.empty, Unversioned)
        } else {
          val shouldMarkInline = {
            sym.hasAnnotation(InlineAnnotationClass) ||
            sym.name.startsWith(nme.ANON_FUN_NAME) ||
            adHocInlineMethods.contains(sym.fullName)
          }

          val shouldMarkNoinline = {
            sym.hasAnnotation(NoinlineAnnotationClass) &&
            !ignoreNoinlineAnnotation(sym)
          }

          val optimizerHints =
            OptimizerHints.empty.
              withInline(shouldMarkInline).
              withNoinline(shouldMarkNoinline)

          val methodDef = {
            if (sym.isClassConstructor) {
              val namespace = js.MemberNamespace.Constructor
              js.MethodDef(
                  js.MemberFlags.empty.withNamespace(namespace), methodName,
                  originalName, jsParams, jstpe.NoType, Some(genStat(dd.rhs)))(
                  optimizerHints, Unversioned)
            } else {
              val resultIRType = toIRType(sym.tpe.resultType)
              val namespace = {
                if (sym.isStaticMember) {
                  if (sym.isPrivate) js.MemberNamespace.PrivateStatic
                  else js.MemberNamespace.PublicStatic
                } else {
                  if (sym.isPrivate) js.MemberNamespace.Private
                  else js.MemberNamespace.Public
                }
              }
              genMethodDef(namespace, methodName, originalName, jsParams,
                  resultIRType, dd.rhs, optimizerHints)
            }
          }

          val methodDefWithoutUselessVars = {
            val unmutatedMutableLocalVars =
              (mutableLocalVars.diff(mutatedLocalVars)).toList
            val mutatedImmutableLocalVals =
              (mutatedLocalVars.diff(mutableLocalVars)).toList
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

          methodDefWithoutUselessVars
        }

        /* #3953 Patch the param defs to have the type advertised by the method's type.
         * This works around https://github.com/scala/bug/issues/11884, whose fix
         * upstream is blocked because it is not binary compatible. The fix here
         * only affects the inside of the js.MethodDef, so it is binary compat.
         */
        val paramTypeRewrites = jsParams.zip(sym.tpe.paramTypes.map(toIRType(_))).collect {
          case (js.ParamDef(name, _, tpe, _), sigType) if tpe != sigType => name.name -> sigType
        }
        if (paramTypeRewrites.isEmpty) {
          // Overwhelmingly common case: all the types match, so there is nothing to do
          jsMethodDef
        } else {
          devWarning(
              "Working around https://github.com/scala/bug/issues/11884 " +
              s"for ${sym.fullName} at ${sym.pos}")
          patchTypeOfParamDefs(jsMethodDef, paramTypeRewrites.toMap)
        }
      }
    }

    def isAbstractMethod(dd: DefDef): Boolean =
      dd.rhs == EmptyTree

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
        patches: Map[LocalName, Boolean]): js.MethodDef = {

      def newMutable(name: LocalName, oldMutable: Boolean): Boolean =
        patches.getOrElse(name, oldMutable)

      val js.MethodDef(flags, methodName, originalName, params, resultType, body) =
        methodDef
      val newParams = for {
        p @ js.ParamDef(name, originalName, ptpe, mutable) <- params
      } yield {
        js.ParamDef(name, originalName, ptpe, newMutable(name.name, mutable))(p.pos)
      }
      val transformer = new ir.Transformers.Transformer {
        override def transform(tree: js.Tree, isStat: Boolean): js.Tree = tree match {
          case js.VarDef(name, originalName, vtpe, mutable, rhs) =>
            assert(isStat, s"found a VarDef in expression position at ${tree.pos}")
            super.transform(js.VarDef(name, originalName, vtpe,
                newMutable(name.name, mutable), rhs)(tree.pos), isStat)
          case js.Closure(arrow, captureParams, params, restParam, body, captureValues) =>
            js.Closure(arrow, captureParams, params, restParam, body,
                captureValues.map(transformExpr))(tree.pos)
          case _ =>
            super.transform(tree, isStat)
        }
      }
      val newBody = body.map(
          b => transformer.transform(b, isStat = resultType == jstpe.NoType))
      js.MethodDef(flags, methodName, originalName, newParams, resultType,
          newBody)(methodDef.optimizerHints, Unversioned)(methodDef.pos)
    }

    /** Patches the type of selected param defs in a [[js.MethodDef]].
     *
     *  @param patches
     *    Map from local name to new type. For param defs not in the map, the
     *    type is untouched.
     */
    private def patchTypeOfParamDefs(methodDef: js.MethodDef,
        patches: Map[LocalName, jstpe.Type]): js.MethodDef = {

      def newType(name: js.LocalIdent, oldType: jstpe.Type): jstpe.Type =
        patches.getOrElse(name.name, oldType)

      val js.MethodDef(flags, methodName, originalName, params, resultType, body) =
        methodDef
      val newParams = for {
        p @ js.ParamDef(name, originalName, ptpe, mutable) <- params
      } yield {
        js.ParamDef(name, originalName, newType(name, ptpe), mutable)(p.pos)
      }
      val transformer = new ir.Transformers.Transformer {
        override def transform(tree: js.Tree, isStat: Boolean): js.Tree = tree match {
          case tree @ js.VarRef(name) =>
            js.VarRef(name)(newType(name, tree.tpe))(tree.pos)
          case js.Closure(arrow, captureParams, params, restParam, body, captureValues) =>
            js.Closure(arrow, captureParams, params, restParam, body,
                captureValues.map(transformExpr))(tree.pos)
          case _ =>
            super.transform(tree, isStat)
        }
      }
      val newBody = body.map(
          b => transformer.transform(b, isStat = resultType == jstpe.NoType))
      js.MethodDef(flags, methodName, originalName, newParams, resultType,
          newBody)(methodDef.optimizerHints, Unversioned)(methodDef.pos)
    }

    /** Generates the JSNativeMemberDef of a JS native method. */
    def genJSNativeMemberDef(tree: DefDef): js.JSNativeMemberDef = {
      implicit val pos = tree.pos

      val sym = tree.symbol
      val flags = js.MemberFlags.empty.withNamespace(js.MemberNamespace.PublicStatic)
      val methodName = encodeMethodSym(sym)
      val jsNativeLoadSpec = jsInterop.jsNativeLoadSpecOf(sym)
      js.JSNativeMemberDef(flags, methodName, jsNativeLoadSpec)
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
    def genMethodDef(namespace: js.MemberNamespace, methodName: js.MethodIdent,
        originalName: OriginalName, jsParams: List[js.ParamDef],
        resultIRType: jstpe.Type, tree: Tree,
        optimizerHints: OptimizerHints): js.MethodDef = {
      implicit val pos = tree.pos

      val bodyIsStat = resultIRType == jstpe.NoType

      def genBodyWithinReturnableScope(): js.Tree = tree match {
        case Block(
            (thisDef @ ValDef(_, nme.THIS, _, initialThis)) :: otherStats,
            rhs) =>
          // This method has tail jumps

          val thisSym = thisDef.symbol
          if (thisSym.isMutable)
            mutableLocalVars += thisSym

          val thisLocalIdent = encodeLocalSym(thisSym)
          val thisLocalType = currentThisType

          val genRhs = {
            /* #3267 In default methods, scalac will type its _$this
             * pseudo-variable as the *self-type* of the enclosing class,
             * instead of the enclosing class type itself. However, it then
             * considers *usages* of _$this as if its type were the
             * enclosing class type. The latter makes sense, since it is
             * compiled as `this` in the bytecode, which necessarily needs
             * to be the enclosing class type. Only the declared type of
             * _$this is wrong.
             *
             * In our case, since we generate an actual local variable for
             * _$this, we must make sure to type it correctly, as the
             * enclosing class type. However, this means the rhs of the
             * ValDef does not match, which is why we have to adapt it
             * here.
             */
            forceAdapt(genExpr(initialThis), thisLocalType)
          }

          val thisLocalVarDef = js.VarDef(thisLocalIdent, thisOriginalName,
              thisLocalType, thisSym.isMutable, genRhs)

          val innerBody = {
            withScopedVars(
              thisLocalVarIdent := Some(thisLocalIdent)
            ) {
              js.Block(otherStats.map(genStat) :+ (
                if (bodyIsStat) genStat(rhs)
                else            genExpr(rhs)))
            }
          }

          js.Block(thisLocalVarDef, innerBody)

        case _ =>
          if (bodyIsStat) genStat(tree)
          else            genExpr(tree)
      }

      def genBody(): js.Tree = {
        withNewReturnableScope(resultIRType) {
          genBodyWithinReturnableScope()
        }
      }

      if (!isNonNativeJSClass(currentClassSym) ||
          isJSFunctionDef(currentClassSym)) {
        val flags = js.MemberFlags.empty.withNamespace(namespace)
        val body = {
          if (currentClassSym.get == HackedStringClass) {
            /* Hijack the bodies of String.length and String.charAt and replace
             * them with String_length and String_charAt operations, respectively.
             */
            methodName.name match {
              case `lengthMethodName` =>
                js.UnaryOp(js.UnaryOp.String_length, genThis())
              case `charAtMethodName` =>
                js.BinaryOp(js.BinaryOp.String_charAt, genThis(), jsParams.head.ref)
              case _ =>
                genBody()
            }
          } else {
            genBody()
          }
        }
        js.MethodDef(flags, methodName, originalName, jsParams, resultIRType,
            Some(body))(
            optimizerHints, Unversioned)
      } else {
        assert(!namespace.isStatic, tree.pos)

        val thisLocalIdent = freshLocalIdent("this")
        withScopedVars(
          thisLocalVarIdent := Some(thisLocalIdent)
        ) {
          val staticNamespace =
            if (namespace.isPrivate) js.MemberNamespace.PrivateStatic
            else js.MemberNamespace.PublicStatic
          val flags =
            js.MemberFlags.empty.withNamespace(staticNamespace)
          val thisParamDef = js.ParamDef(thisLocalIdent, thisOriginalName,
              jstpe.AnyType, mutable = false)

          js.MethodDef(flags, methodName, originalName,
              thisParamDef :: jsParams, resultIRType, Some(genBody()))(
              optimizerHints, Unversioned)
        }
      }
    }

    /** Forces the given `tree` to a given type by adapting it.
     *
     *  @param tree
     *    The tree to adapt.
     *  @param tpe
     *    The target type, which must be either `AnyType` or `ClassType(_)`.
     */
    private def forceAdapt(tree: js.Tree, tpe: jstpe.Type): js.Tree = {
      if (tree.tpe == tpe || tpe == jstpe.AnyType) {
        tree
      } else {
        /* Remove the useless cast that scalac's erasure had to introduce to
         * work around their own ill-typed _$this. Note that the optimizer will
         * not be able to do that, since it won't be able to prove that the
         * underlying expression is indeed an instance of `tpe`.
         */
        tree match {
          case js.AsInstanceOf(underlying, _) if underlying.tpe == tpe =>
            underlying
          case _ =>
            js.AsInstanceOf(tree, tpe)(tree.pos)
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
        case js.Block(stats :+ expr) =>
          js.Block(stats :+ exprToStat(expr))
        case _:js.Literal | _:js.This | _:js.VarRef =>
          js.Skip()
        case _ =>
          tree
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

    /** Gen JS code for a tree in expression position (in the IR) or the
     *  global scope.
     */
    def genExprOrGlobalScope(tree: Tree): MaybeGlobalScope = {
      implicit def pos: Position = tree.pos

      tree match {
        case _: This =>
          val sym = tree.symbol
          if (sym != currentClassSym.get && sym.isModule)
            genLoadModuleOrGlobalScope(sym)
          else
            MaybeGlobalScope.NotGlobalScope(genExpr(tree))

        case _:Ident | _:Select =>
          val sym = tree.symbol
          if (sym.isModule) {
            assert(!sym.isPackageClass, "Cannot use package as value: " + tree)
            genLoadModuleOrGlobalScope(sym)
          } else {
            MaybeGlobalScope.NotGlobalScope(genExpr(tree))
          }

        case _ =>
          MaybeGlobalScope.NotGlobalScope(genExpr(tree))
      }
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
          genLabelDef(lblDf, isStat)

        /** Local val or var declaration */
        case ValDef(_, name, _, rhs) =>
          /* Must have been eliminated by the tail call transform performed
           * by genMethodDef().
           * If you ever change/remove this assertion, you need to update
           * genEnclosingLabelApply() regarding `nme.THIS`.
           */
          assert(name != nme.THIS,
              s"ValDef(_, nme.THIS, _, _) found at ${tree.pos}")

          val sym = tree.symbol
          val rhsTree =
            if (rhs == EmptyTree) jstpe.zeroOf(toIRType(sym.tpe))
            else genExpr(rhs)

          rhsTree match {
            case js.Transient(UndefinedParam) =>
              // This is an intermediate assignment for default params on a
              // js.Any. Add the symbol to the corresponding set to inform
              // the Ident resolver how to replace it and don't emit the symbol
              undefinedDefaultParams += sym
              js.Skip()
            case _ =>
              if (sym.isMutable)
                mutableLocalVars += sym
              js.VarDef(encodeLocalSym(sym), originalNameOfLocal(sym),
                  toIRType(sym.tpe), sym.isMutable, rhsTree)
          }

        case tree @ If(cond, thenp, elsep) =>
          def default: js.Tree = {
            val tpe =
              if (isStat) jstpe.NoType
              else toIRType(tree.tpe)

            js.If(genExpr(cond), genStatOrExpr(thenp, isStat),
                genStatOrExpr(elsep, isStat))(tpe)
          }

          if (isStat && currentMethodSym.isClassConstructor) {
            /* Nested classes that need an outer pointer have a weird shape for
             * assigning it, with an explicit null check. It looks like this:
             *
             *   if ($outer.eq(null))
             *     throw null
             *   else
             *     this.$outer = $outer
             *
             * This is a bad shape for our optimizations, notably because the
             * explicit null check cannot be considered UB at the IR level if
             * we compile it as is, although in the original *language*, that
             * would clearly fall into UB.
             *
             * Therefore, we intercept that shape and rewrite it as follows
             * instead:
             *
             *   <getClass>($outer)   // null check subject to UB
             *   this.$outer = $outer // the `else` branch in general
             */
            tree match {
              case OuterPointerNullCheck(outer, elsep) =>
                js.Block(
                  js.GetClass(genExpr(outer)), // null check
                  genStat(elsep)
                )
              case _ =>
                default
            }
          } else {
            default
          }

        case Return(expr) =>
          js.Return(toIRType(expr.tpe) match {
            case jstpe.NoType => js.Block(genStat(expr), js.Undefined())
            case _            => genExpr(expr)
          }, getEnclosingReturnLabel())

        case t: Try =>
          genTry(t, isStat)

        case Throw(expr) =>
          val ex = genExpr(expr)
          ex match {
            case js.New(cls, _, _) if cls != JavaScriptExceptionClassName =>
              // Common case where ex is neither null nor a js.JavaScriptException
              js.Throw(ex)
            case _ =>
              js.Throw(js.UnwrapFromThrowable(ex))
          }

        /* !!! Copy-pasted from `CleanUp.scala` upstream and simplified with
         * our `WrapArray` extractor.
         *
         * Replaces `Array(Predef.wrapArray(ArrayValue(...).$asInstanceOf[...]), <tag>)`
         * with just `ArrayValue(...)`
         *
         * See scala/bug#6611; we must *only* do this for literal vararg arrays.
         *
         * This is normally done by `cleanup` but it comes later than this phase.
         */
        case Apply(appMeth,
            Apply(wrapRefArrayMeth, StripCast(arg @ ArrayValue(elemtpt, elems)) :: Nil) :: classTagEvidence :: Nil)
            if WrapArray.isClassTagBasedWrapArrayMethod(wrapRefArrayMeth.symbol) &&
                appMeth.symbol == ArrayModule_genericApply &&
                !elemtpt.tpe.typeSymbol.isBottomClass &&
                !elemtpt.tpe.typeSymbol.isPrimitiveValueClass /* can happen via specialization.*/ =>
          classTagEvidence.attachments.get[analyzer.MacroExpansionAttachment] match {
            case Some(att)
                if att.expandee.symbol.name == nme.materializeClassTag && tree.isInstanceOf[ApplyToImplicitArgs] =>
              genArrayValue(arg)
            case _ =>
              val arrValue = genApplyMethod(
                  genExpr(classTagEvidence),
                  ClassTagClass.info.decl(nme.newArray),
                  js.IntLiteral(elems.size) :: Nil)
              val arrVarDef = js.VarDef(freshLocalIdent("arr"), NoOriginalName,
                  arrValue.tpe, mutable = false, arrValue)
              val stats = List.newBuilder[js.Tree]
              foreachWithIndex(elems) { (elem, i) =>
                stats += genApplyMethod(
                    genLoadModule(ScalaRunTimeModule),
                    currentRun.runDefinitions.arrayUpdateMethod,
                    arrVarDef.ref :: js.IntLiteral(i) :: genExpr(elem) :: Nil)
              }
              js.Block(arrVarDef :: stats.result(), arrVarDef.ref)
          }
        case Apply(appMeth, elem0 :: WrapArray(rest @ ArrayValue(elemtpt, _)) :: Nil)
            if appMeth.symbol == ArrayModule_apply(elemtpt.tpe) =>
          genArrayValue(rest, elem0 :: rest.elems)
        case Apply(appMeth, elem :: (nil: RefTree) :: Nil)
            if nil.symbol == NilModule && appMeth.symbol == ArrayModule_apply(elem.tpe.widen) &&
            treeInfo.isExprSafeToInline(nil) =>
          genArrayValue(tree, elem :: Nil)

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

          def unboxFieldValue(boxed: js.Tree): js.Tree = {
            fromAny(boxed,
                enteringPhase(currentRun.posterasurePhase)(sym.tpe))
          }

          if (sym.isModule) {
            assert(!sym.isPackageClass, "Cannot use package as value: " + tree)
            genLoadModule(sym)
          } else if (sym.isStaticMember) {
            genStaticField(sym)
          } else if (paramAccessorLocals contains sym) {
            paramAccessorLocals(sym).ref
          } else {
            val (field, boxed) = genAssignableField(sym, qualifier)
            if (boxed) unboxFieldValue(field)
            else field
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
              js.Transient(UndefinedParam)
            } else {
              genVarRef(sym)
            }
          } else {
            abort("Cannot use package as value: " + tree)
          }

        case Literal(value) =>
          value.tag match {
            case UnitTag =>
              js.Skip()
            case BooleanTag =>
              js.BooleanLiteral(value.booleanValue)
            case ByteTag =>
              js.ByteLiteral(value.byteValue)
            case ShortTag =>
              js.ShortLiteral(value.shortValue)
            case CharTag =>
              js.CharLiteral(value.charValue)
            case IntTag =>
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
              js.ClassOf(toTypeRef(value.typeValue))
            case EnumTag =>
              genStaticField(value.symbolValue)
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
          def genRhs = genExpr(rhs)
          lhs match {
            case Select(qualifier, _) =>
              /* Record assignments to fields of the current class.
               *
               * We only do that for fields of the current class sym. For other
               * fields, even if we recorded it, we would forget them when
               * `fieldsMutatedInCurrentClass` is reset when going out of the
               * class. If we assign to an immutable field in a different
               * class, it will be reported as an IR checking error.
               *
               * Assignments to `this.` fields in the constructor are valid
               * even for immutable fields, and are therefore not recorded.
               *
               * #3918 We record the *names* of the fields instead of their
               * symbols because, in rare cases, scalac has different fields in
               * the trees than in the class' decls. Since we only record fields
               * from the current class, names are non ambiguous. For the same
               * reason, we record assignments to *all* fields, not only the
               * immutable ones, because in 2.13 the symbol here can be mutable
               * but not the one in the class' decls.
               */
              if (sym.owner == currentClassSym.get) {
                val ctorAssignment = (
                    currentMethodSym.isClassConstructor &&
                    currentMethodSym.owner == qualifier.symbol &&
                    qualifier.isInstanceOf[This]
                )
                if (!ctorAssignment)
                  fieldsMutatedInCurrentClass += sym.name
              }

              def genBoxedRhs: js.Tree = {
                val tpeEnteringPosterasure =
                  enteringPhase(currentRun.posterasurePhase)(rhs.tpe)
                if ((tpeEnteringPosterasure eq null) && genRhs.isInstanceOf[js.Null]) {
                  devWarning(
                      "Working around https://github.com/scala-js/scala-js/issues/3422 " +
                      s"for ${sym.fullName} at ${sym.pos}")
                  // Fortunately, a literal `null` never needs to be boxed
                  genRhs
                } else {
                  ensureBoxed(genRhs, tpeEnteringPosterasure)
                }
              }

              if (sym.hasAnnotation(JSNativeAnnotation)) {
                /* This is an assignment to a @js.native field. Since we reject
                 * `@js.native var`s as compile errors, this can only happen in
                 * the constructor of the enclosing object.
                 * We simply ignore the assignment, since the field will not be
                 * emitted at all.
                 */
                js.Skip()
              } else {
                val (field, boxed) = genAssignableField(sym, qualifier)

                if (boxed) js.Assign(field, genBoxedRhs)
                else js.Assign(field,genRhs)
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

        /** Anonymous function */
        case fun: Function =>
          genAnonFunction(fun)

        case EmptyTree =>
          js.Skip()

        case _ =>
          abort("Unexpected tree in genExpr: " +
              tree + "/" + tree.getClass + " at: " + tree.pos)
      }
    } // end of GenJSCode.genExpr()

    /** Extractor for the shape of outer pointer null check.
     *
     *  See the comment in `case If(...) =>` of `genExpr`.
     *
     *  When successful, returns the pair `(outer, elsep)` where `outer` is the
     *  `Ident` of the outer constructor parameter, and `elsep` is the else
     *  branch of the condition.
     */
    private object OuterPointerNullCheck {
      def unapply(tree: If): Option[(Ident, Tree)] = tree match {
        case If(Apply(fun @ Select(outer: Ident, nme.eq), Literal(Constant(null)) :: Nil),
            Throw(Literal(Constant(null))), elsep)
            if outer.symbol.isOuterParam && fun.symbol == definitions.Object_eq =>
          Some((outer, elsep))
        case _ =>
          None
      }
    }

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
        js.This()(currentThisType)
      } { thisLocalIdent =>
        // .copy() to get the correct position
        js.VarRef(thisLocalIdent.copy())(currentThisType)
      }
    }

    /** Gen JS code for LabelDef.
     *
     *  If a LabelDef reaches this method, then the only valid jumps are from
     *  within it, which means it basically represents a loop. Other kinds of
     *  LabelDefs, notably those for matches, are caught upstream and
     *  transformed in ad hoc ways.
     *
     *  The general transformation for
     *  {{{
     *  labelName(...labelParams) {
     *    rhs
     *  }: T
     *  }}}
     *  is the following:
     *  {{{
     *  block[T]: {
     *    while (true) {
     *      labelName[void]: {
     *        return@block transformedRhs
     *      }
     *    }
     *  }
     *  }}}
     *  where all jumps to the label inside the rhs of the form
     *  {{{
     *  labelName(...args)
     *  }}}
     *  are transformed into
     *  {{{
     *  ...labelParams = ...args;
     *  return@labelName (void 0)
     *  }}}
     *
     *  This is always correct, so it can handle arbitrary labels and jumps
     *  such as those produced by loops, tail-recursive calls and even some
     *  compiler plugins (see for example #1148). However, the result is
     *  unnecessarily ugly for simple `while` and `do while` loops, so we have
     *  some post-processing to simplify those.
     */
    def genLabelDef(tree: LabelDef, isStat: Boolean): js.Tree = {
      implicit val pos = tree.pos
      val sym = tree.symbol

      val labelParamSyms = tree.params.map(_.symbol)
      val info = new EnclosingLabelDefInfoWithResultAsAssigns(labelParamSyms)

      val labelIdent = encodeLabelSym(sym)
      val labelName = labelIdent.name

      val transformedRhs = withScopedVars(
        enclosingLabelDefInfos := enclosingLabelDefInfos.get + (sym -> info)
      ) {
        genStatOrExpr(tree.rhs, isStat)
      }

      /** Matches a `js.Return` to the current `labelName`, and returns the
       *  `exprToStat()` of the returned expression.
       *  We only keep the `exprToStat()` because this label has a `void` type,
       *  so the expression is always discarded except for its side effects.
       */
      object ReturnFromThisLabel {
        def unapply(tree: js.Return): Option[js.Tree] = {
          if (tree.label.name == labelName) Some(exprToStat(tree.expr))
          else None
        }
      }

      def genDefault(): js.Tree = {
        if (transformedRhs.tpe == jstpe.NothingType) {
          // In this case, we do not need the outer block label
          js.While(js.BooleanLiteral(true), {
            js.Labeled(labelIdent, jstpe.NoType, {
              transformedRhs match {
                // Eliminate a trailing return@lab
                case js.Block(stats :+ ReturnFromThisLabel(exprAsStat)) =>
                  js.Block(stats :+ exprAsStat)
                case _ =>
                  transformedRhs
              }
            })
          })
        } else {
          // When all else has failed, we need the full machinery
          val blockLabelIdent = freshLabelIdent("block")
          val bodyType =
            if (isStat) jstpe.NoType
            else toIRType(tree.tpe)
          js.Labeled(blockLabelIdent, bodyType, {
            js.While(js.BooleanLiteral(true), {
              js.Labeled(labelIdent, jstpe.NoType, {
                if (isStat)
                  js.Block(transformedRhs, js.Return(js.Undefined(), blockLabelIdent))
                else
                  js.Return(transformedRhs, blockLabelIdent)
              })
            })
          })
        }
      }

      info.generatedReturns match {
        case 0 =>
          /* There are no jumps to the loop label. Therefore we can remove
           * the labeled block and and the loop altogether.
           * This happens for `while (false)` and `do while (false)` loops.
           */
          transformedRhs

        case 1 =>
          /* There is exactly one jump. Let us see if we can isolate where it
           * is to try and remove unnecessary labeled blocks and keep only
           * the loop.
           */
          transformedRhs match {
            /* { stats; return@lab expr }
             * -> while (true) { stats; expr }
             * This happens for `while (true)` and `do while (true)` loops.
             */
            case BlockOrAlone(stats, ReturnFromThisLabel(exprAsStat)) =>
              js.While(js.BooleanLiteral(true), {
                js.Block(stats, exprAsStat)
              })

            /* if (cond) { stats; return@lab expr } else elsep [; rest]
             * -> while (cond) { stats; expr }; elsep; rest
             * This happens for `while (cond)` loops with a non-constant `cond`.
             * There is a `rest` if the while loop is on the rhs of a case in a
             * patmat.
             */
            case FirstInBlockOrAlone(
                js.If(cond, BlockOrAlone(stats, ReturnFromThisLabel(exprAsStat)), elsep),
                rest) =>
              js.Block(
                  js.While(cond, {
                    js.Block(stats, exprAsStat)
                  }) ::
                  elsep ::
                  rest
              )

            /* { stats; if (cond) { return@lab pureExpr } else { skip } }
             *
             * !! `cond` could refer to VarDefs declared in stats, and we have
             * no way of telling (short of traversing `cond` again) so we
             * generate a `while` loop anyway:
             *
             * -> while ({ stats; cond }) { skip }
             *
             * The `pureExpr` must be pure because we cannot add it after the
             * `cond` above. It must be eliminated, which is only valid if it
             * is pure.
             *
             * This happens for `do while (cond)` loops with a non-constant
             * `cond`.
             *
             * There is no need for BlockOrAlone because the alone case would
             * also be caught by the `case js.If` above.
             */
            case js.Block(stats :+ js.If(cond, ReturnFromThisLabel(js.Skip()), js.Skip())) =>
              js.While(js.Block(stats, cond), js.Skip())

            /* { stats; if (cond) { return@lab pureExpr } else { skip }; literal }
             *
             * Same as above, but there is an additional `literal` at the end.
             *
             * This happens for `do while (cond)` loops with a non-constant
             * `cond` that are in the rhs of a case in a patmat.
             */
            case js.Block(stats :+ js.If(cond, ReturnFromThisLabel(js.Skip()), js.Skip()) :+ (res: js.Literal)) =>
              js.Block(js.While(js.Block(stats, cond), js.Skip()), res)

            case _ =>
              genDefault()
          }

        case moreThan1 =>
          genDefault()
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

      val resultType =
        if (isStat) jstpe.NoType
        else toIRType(tree.tpe)

      val handled =
        if (catches.isEmpty) blockAST
        else genTryCatch(blockAST, catches, resultType, isStat)

      genStat(finalizer) match {
        case js.Skip() => handled
        case ast       => js.TryFinally(handled, ast)
      }
    }

    private def genTryCatch(body: js.Tree, catches: List[CaseDef],
        resultType: jstpe.Type, isStat: Boolean)(
        implicit pos: Position): js.Tree = {

      catches match {
        case CaseDef(Ident(nme.WILDCARD), _, catchAllBody) :: Nil =>
          genTryCatchCatchIgnoreAll(body, catchAllBody, resultType, isStat)

        case CaseDef(Typed(Ident(nme.WILDCARD), tpt), _, catchAllBody) :: Nil
            if tpt.tpe.typeSymbol == ThrowableClass =>
          genTryCatchCatchIgnoreAll(body, catchAllBody, resultType, isStat)

        case _ =>
          genTryCatchNotIgnoreAll(body, catches, resultType, isStat)
      }
    }

    private def genTryCatchCatchIgnoreAll(body: js.Tree, catchAllBody: Tree,
        resultType: jstpe.Type, isStat: Boolean)(
        implicit pos: Position): js.Tree = {

      js.TryCatch(body, freshLocalIdent("e"), NoOriginalName,
          genStatOrExpr(catchAllBody, isStat))(
          resultType)
    }

    private def genTryCatchNotIgnoreAll(body: js.Tree, catches: List[CaseDef],
        resultType: jstpe.Type, isStat: Boolean)(
        implicit pos: Position): js.Tree = {

      val exceptIdent = freshLocalIdent("e")
      val origExceptVar = js.VarRef(exceptIdent)(jstpe.AnyType)

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
        val valDef = js.VarDef(freshLocalIdent("e"), NoOriginalName,
            encodeClassType(ThrowableClass), mutable = false, js.WrapAsThrowable(origExceptVar))
        (valDef, valDef.ref)
      } else {
        (js.Skip(), origExceptVar)
      }

      val elseHandler: js.Tree = js.Throw(origExceptVar)

      val handler = catches.foldRight(elseHandler) { (caseDef, elsep) =>
        implicit val pos = caseDef.pos
        val CaseDef(pat, _, body) = caseDef

        // Extract exception type and variable
        val (tpe, boundVar) = (pat match {
          case Typed(Ident(nme.WILDCARD), tpt) =>
            (tpt.tpe, None)
          case Ident(nme.WILDCARD) =>
            (ThrowableClass.tpe, None)
          case Bind(_, _) =>
            val ident = encodeLocalSym(pat.symbol)
            val origName = originalNameOfLocal(pat.symbol)
            (pat.symbol.tpe, Some((ident, origName)))
        })

        // Generate the body that must be executed if the exception matches
        val bodyWithBoundVar = (boundVar match {
          case None =>
            genStatOrExpr(body, isStat)
          case Some((boundVarIdent, boundVarOriginalName)) =>
            val castException = genAsInstanceOf(exceptVar, tpe)
            js.Block(
                js.VarDef(boundVarIdent, boundVarOriginalName, toIRType(tpe),
                    mutable = false, castException),
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

      js.TryCatch(body, exceptIdent, NoOriginalName,
          js.Block(exceptValDef, handler))(resultType)
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

      /* Is the method a JS default accessor, which should become an
       * `UndefinedParam` rather than being compiled normally.
       *
       * This is true iff one of the following conditions apply:
       * - It is a constructor default param for the constructor of a JS class.
       * - It is a default param of an instance method of a native JS type.
       * - It is a default param of an instance method of a non-native JS type
       *   and the attached method is exposed.
       * - It is a default param for a native JS def.
       *
       * This is different than `isIgnorableDefaultParam` in `genMethod`: we
       * include here the default accessors of *non-native* JS types (unless
       * the corresponding methods are not exposed). We also need to handle
       * non-constructor members of native JS types.
       */
      def isJSDefaultParam: Boolean = {
        DefaultParamInfo.isApplicable(sym) && {
          val info = new DefaultParamInfo(sym)
          if (info.isForConstructor) {
            /* This is a default accessor for a constructor parameter. Check
             * whether the attached constructor is a JS constructor, which is
             * the case iff the linked class is a JS type.
             */
            isJSType(info.constructorOwner)
          } else {
            if (isJSType(sym.owner)) {
              /* The default accessor is in a JS type. It is a JS default
               * param iff the enclosing class is native or the attached method
               * is exposed.
               */
              !isNonNativeJSClass(sym.owner) || isExposed(info.attachedMethod)
            } else {
              /* The default accessor is in a Scala type. It is a JS default
               * param iff the attached method is a native JS def. This can
               * only happen if the owner is a module class, which we test
               * first as a fast way out.
               */
              sym.owner.isModuleClass && info.attachedMethod.hasAnnotation(JSNativeAnnotation)
            }
          }
        }
      }

      fun match {
        case TypeApply(_, _) =>
          genApplyTypeApply(tree, isStat)

        case _ if isJSDefaultParam =>
          js.Transient(UndefinedParam)

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
            makePrimitiveBox(genExpr(arg), sym.firstParam.tpe)
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
     *
     *  Until 2.12.0-M5, only `isInstanceOf` and `asInstanceOf` kept their type
     *  argument until the backend. Since 2.12.0-RC1, `AnyRef.synchronized`
     *  does so too.
     */
    private def genApplyTypeApply(tree: Apply, isStat: Boolean): js.Tree = {
      implicit val pos = tree.pos
      val Apply(TypeApply(fun @ Select(obj, _), targs), args) = tree
      val sym = fun.symbol

      sym match {
        case Object_isInstanceOf =>
          genIsAsInstanceOf(obj, targs, cast = false)
        case Object_asInstanceOf =>
          genIsAsInstanceOf(obj, targs, cast = true)
        case Object_synchronized =>
          genSynchronized(obj, args.head, isStat)
        case _ =>
          abort("Unexpected type application " + fun +
              "[sym: " + sym.fullName + "]" + " in: " + tree)
      }
    }

    /** Gen `isInstanceOf` or `asInstanceOf`. */
    private def genIsAsInstanceOf(obj: Tree, targs: List[Tree], cast: Boolean)(
        implicit pos: Position): js.Tree = {
      genIsAsInstanceOf(genExpr(obj), obj.tpe, targs.head.tpe, cast)
    }

    /** Gen `isInstanceOf` or `asInstanceOf`. */
    private def genIsAsInstanceOf(expr: js.Tree, from: Type, to: Type,
        cast: Boolean)(
        implicit pos: Position): js.Tree = {
      val l = toIRType(from)
      val r = toIRType(to)

      def isValueType(tpe: jstpe.Type): Boolean = tpe match {
        case jstpe.NoType | jstpe.BooleanType | jstpe.CharType |
            jstpe.ByteType | jstpe.ShortType | jstpe.IntType | jstpe.LongType |
            jstpe.FloatType | jstpe.DoubleType =>
          true
        case _ =>
          false
      }

      val lIsValueType = isValueType(l)
      val rIsValueType = isValueType(r)

      if (lIsValueType && rIsValueType) {
        if (cast) {
          /* It is unclear whether this case can be reached for all type
           * conversions, but scalac handles all cases, so we do too.
           * Three known user code patterns that become code handled by this
           * case are `byte.##`, `short.##` and `char.##`, which become, e.g.,
           * `char.toChar().$asInstanceOf[Int]`.
           */
          genConversion(l, r, expr)
        } else {
          js.BooleanLiteral(l == r)
        }
      } else if (lIsValueType) {
        val result =
          if (cast) genThrowClassCastException()
          else js.BooleanLiteral(false)
        js.Block(expr, result) // eval and discard source
      } else if (rIsValueType) {
        assert(!cast, s"Unexpected asInstanceOf from ref type to value type")
        genIsInstanceOf(expr, boxedClass(to.typeSymbol).tpe)
      } else {
        if (cast)
          genAsInstanceOf(expr, to)
        else
          genIsInstanceOf(expr, to)
      }
    }

    private def genThrowClassCastException()(implicit pos: Position): js.Tree = {
      val ctor = ClassCastExceptionClass.info.member(
          nme.CONSTRUCTOR).suchThat(_.tpe.params.isEmpty)
      js.Throw(genNew(ClassCastExceptionClass, ctor, Nil))
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
      val Apply(fun @ Select(sup @ Super(qual, _), _), args) = tree
      val sym = fun.symbol

      if (isJSType(qual.tpe)) {
        if (sym.isMixinConstructor) {
          /* Do not emit a call to the $init$ method of JS traits.
           * This exception is necessary because @JSOptional fields cause the
           * creation of a $init$ method, which we must not call.
           */
          js.Skip()
        } else {
          genJSSuperCall(tree, isStat)
        }
      } else {
        /* #3013 `qual` can be `this.$outer()` in some cases since Scala 2.12,
         * so we call `genExpr(qual)`, not just `genThis()`.
         */
        val superCall = genApplyMethodStatically(
            genExpr(qual), sym, genActualArgs(sym, args))

        // Initialize the module instance just after the super constructor call.
        if (isStaticModule(currentClassSym) && !isModuleInitialized.value &&
            currentMethodSym.isClassConstructor) {
          isModuleInitialized.value = true
          js.Block(superCall, js.StoreModule())
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
     *  * new of a JS class
     *  * new Array
     *  * regular new
     */
    private def genApplyNew(tree: Apply): js.Tree = {
      implicit val pos = tree.pos
      val Apply(fun @ Select(New(tpt), nme.CONSTRUCTOR), args) = tree
      val ctor = fun.symbol
      val tpe = tpt.tpe
      val clsSym = tpe.typeSymbol

      assert(ctor.isClassConstructor,
          "'new' call to non-constructor: " + ctor.name)

      if (isHijackedClass(clsSym)) {
        genNewHijackedClass(clsSym, ctor, args.map(genExpr))
      } else if (isJSFunctionDef(clsSym)) {
        val classDef = consumeLazilyGeneratedAnonClass(clsSym)
        genJSFunction(classDef, args.map(genExpr))
      } else if (clsSym.isAnonymousFunction) {
        val classDef = consumeLazilyGeneratedAnonClass(clsSym)
        tryGenAnonFunctionClass(classDef, args.map(genExpr)).getOrElse {
          // Cannot optimize anonymous function class. Generate full class.
          generatedClasses += nestedGenerateClass(clsSym)(genClass(classDef)) -> clsSym.pos
          genNew(clsSym, ctor, genActualArgs(ctor, args))
        }
      } else if (isJSType(clsSym)) {
        genPrimitiveJSNew(tree)
      } else {
        toTypeRef(tpe) match {
          case jstpe.ClassRef(className) =>
            genNew(className, ctor, genActualArgs(ctor, args))
          case arr: jstpe.ArrayTypeRef =>
            genNewArray(arr, args.map(genExpr))
          case prim: jstpe.PrimRef =>
            abort(s"unexpected primitive type $prim in New at $pos")
        }
      }
    }

    /** Gen jump to a label.
     *
     *  Some label-applys are caught upstream (jumps to next case of a pattern
     *  match that are in tail-pos or their own case), but most are handled
     *  here, notably:
     *
     *  - Jumps to the beginning label of loops, including tail-recursive calls
     *  - Jumps to the next case label that are not in tail position
     *  - Jumps to the end of a pattern match
     */
    private def genLabelApply(tree: Apply): js.Tree = {
      implicit val pos = tree.pos
      val Apply(fun, args) = tree
      val sym = fun.symbol

      val info = enclosingLabelDefInfos.getOrElse(sym, {
        abort("Found unknown label apply at "+tree.pos+": "+tree)
      })

      val labelIdent = encodeLabelSym(sym)
      info.generatedReturns += 1

      def assertArgCountMatches(expected: Int): Unit = {
        assert(args.size == expected,
            s"argument count mismatch for label-apply at $pos: " +
            s"expected $expected but got ${args.size}")
      }

      info match {
        case info: EnclosingLabelDefInfoWithResultAsAssigns =>
          val paramSyms = info.paramSyms
          assertArgCountMatches(paramSyms.size)

          val jump = js.Return(js.Undefined(), labelIdent)

          if (args.isEmpty) {
            // fast path, applicable notably to loops and case labels
            jump
          } else {
            js.Block(genMultiAssign(paramSyms, args), jump)
          }

        case _: EnclosingLabelDefInfoWithResultAsReturn =>
          assertArgCountMatches(1)
          js.Return(genExpr(args.head), labelIdent)
      }
    }

    /** Gen multiple "parallel" assignments.
     *
     *  This is used when assigning the new value of multiple parameters of a
     *  label-def, notably for the ones generated for tail-recursive methods.
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
    private def genMultiAssign(targetSyms: List[Symbol], values: List[Tree])(
        implicit pos: Position): List[js.Tree] = {

      // Prepare quadruplets of (formalArg, irType, tempVar, actualArg)
      // Do not include trivial assignments (when actualArg == formalArg)
      val quadruplets = {
        val quadruplets =
          List.newBuilder[(js.VarRef, jstpe.Type, js.LocalIdent, js.Tree)]

        for ((formalArgSym, arg) <- targetSyms.zip(values)) {
          val formalArg = encodeLocalSym(formalArgSym)
          val actualArg = genExpr(arg)

          /* #3267 The formal argument representing the special `this` of a
           * tailrec method can have the wrong type in the scalac symbol table.
           * We need to patch it up, along with the actual argument, to be the
           * enclosing class type.
           * See the longer comment in genMethodDef() for more details.
           *
           * Note that only testing the `name` against `nme.THIS` is safe,
           * given that `genStatOrExpr()` for `ValDef` asserts that no local
           * variable named `nme.THIS` can happen, other than the ones
           * generated for tailrec methods.
           */
          val isTailJumpThisLocalVar = formalArgSym.name == nme.THIS

          val tpe =
            if (isTailJumpThisLocalVar) currentThisType
            else toIRType(formalArgSym.tpe)

          val fixedActualArg =
            if (isTailJumpThisLocalVar) forceAdapt(actualArg, tpe)
            else actualArg

          actualArg match {
            case js.VarRef(`formalArg`) =>
              // This is trivial assignment, we don't need it

            case _ =>
              mutatedLocalVars += formalArgSym
              quadruplets += ((js.VarRef(formalArg)(tpe), tpe,
                  freshLocalIdent(formalArg.name.withPrefix("temp$")),
                  fixedActualArg))
          }
        }

        quadruplets.result()
      }

      quadruplets match {
        case Nil =>
          Nil

        case (formalArg, _, _, actualArg) :: Nil =>
          js.Assign(formalArg, actualArg) :: Nil

        case _ =>
          val tempAssignments =
            for ((_, argType, tempArg, actualArg) <- quadruplets)
              yield js.VarDef(tempArg, NoOriginalName, argType, mutable = false, actualArg)
          val trueAssignments =
            for ((formalArg, argType, tempArg, _) <- quadruplets)
              yield js.Assign(formalArg, js.VarRef(tempArg)(argType))
          tempAssignments ::: trueAssignments
      }
    }

    /** Gen a "normal" apply (to a true method).
     *
     *  But even these are further refined into:
     *
     *  - Calls to methods of JS types.
     *  - Calls to methods in impl classes of traits.
     *  - Direct calls to constructors (from secondary constructor to another one).
     *  - Regular method calls.
     */
    private def genNormalApply(tree: Apply, isStat: Boolean): js.Tree = {
      implicit val pos = tree.pos
      val Apply(fun @ Select(receiver, _), args) = tree
      val sym = fun.symbol

      val inline = {
        tree.hasAttachment[InlineCallsiteAttachment.type] ||
        fun.hasAttachment[InlineCallsiteAttachment.type] // nullary methods
      }
      val noinline = {
        tree.hasAttachment[NoInlineCallsiteAttachment.type] ||
        fun.hasAttachment[NoInlineCallsiteAttachment.type]  // nullary methods
      }

      if (isJSType(receiver.tpe) && sym.owner != ObjectClass) {
        if (!isNonNativeJSClass(sym.owner) || isExposed(sym))
          genPrimitiveJSCall(tree, isStat)
        else
          genApplyJSClassMethod(genExpr(receiver), sym, genActualArgs(sym, args))
      } else if (sym.hasAnnotation(JSNativeAnnotation)) {
        genJSNativeMemberCall(tree, isStat)
      } else if (sym.isStaticMember) {
        if (sym.isMixinConstructor) {
          /* Do not emit a call to the $init$ method of JS traits.
           * This exception is necessary because optional JS fields cause the
           * creation of a $init$ method, which we must not call.
           */
          js.Skip()
        } else {
          genApplyStatic(sym, args.map(genExpr), inline = inline, noinline = noinline)
        }
      } else {
        genApplyMethodMaybeStatically(genExpr(receiver), sym,
            genActualArgs(sym, args), inline = inline, noinline = noinline)
      }
    }

    def genApplyMethodMaybeStatically(receiver: js.Tree,
        method: Symbol, arguments: List[js.Tree],
        inline: Boolean = false, noinline: Boolean = false)(
        implicit pos: Position): js.Tree = {
      if (method.isPrivate || method.isClassConstructor)
        genApplyMethodStatically(receiver, method, arguments, inline = inline, noinline = noinline)
      else
        genApplyMethod(receiver, method, arguments, inline = inline, noinline = noinline)
    }

    /** Gen JS code for a call to a Scala method. */
    def genApplyMethod(receiver: js.Tree,
        method: Symbol, arguments: List[js.Tree],
        inline: Boolean = false, noinline: Boolean = false)(
        implicit pos: Position): js.Tree = {
      assert(!method.isPrivate,
          s"Cannot generate a dynamic call to private method $method at $pos")
      val flags = js.ApplyFlags.empty
        .withInline(inline)
        .withNoinline(noinline)

      js.Apply(flags, receiver, encodeMethodSym(method), arguments)(
          toIRType(method.tpe.resultType))
    }

    def genApplyMethodStatically(receiver: js.Tree, method: Symbol,
        arguments: List[js.Tree], inline: Boolean = false, noinline: Boolean = false)(
        implicit pos: Position): js.Tree = {
      val flags = js.ApplyFlags.empty
        .withPrivate(method.isPrivate && !method.isClassConstructor)
        .withConstructor(method.isClassConstructor)
        .withInline(inline)
        .withNoinline(noinline)
      val methodIdent = encodeMethodSym(method)
      val resultType =
        if (method.isClassConstructor) jstpe.NoType
        else toIRType(method.tpe.resultType)
      js.ApplyStatically(flags, receiver, encodeClassName(method.owner),
          methodIdent, arguments)(resultType)
    }

    def genApplyJSClassMethod(receiver: js.Tree, method: Symbol,
        arguments: List[js.Tree], inline: Boolean = false)(
        implicit pos: Position): js.Tree = {
      genApplyStatic(method, receiver :: arguments, inline = inline)
    }

    def genApplyStatic(method: Symbol, arguments: List[js.Tree],
        inline: Boolean = false, noinline: Boolean = false)(
        implicit pos: Position): js.Tree = {
      val flags = js.ApplyFlags.empty
        .withPrivate(method.isPrivate)
        .withInline(inline)
        .withNoinline(noinline)
      js.ApplyStatic(flags, encodeClassName(method.owner),
          encodeMethodSym(method), arguments)(toIRType(method.tpe.resultType))
    }

    private def adaptPrimitive(value: js.Tree, to: jstpe.Type)(
        implicit pos: Position): js.Tree = {
      genConversion(value.tpe, to, value)
    }

    /* This method corresponds to the method of the same name in
     * BCodeBodyBuilder of the JVM back-end. It ends up calling the method
     * BCodeIdiomatic.emitT2T, whose logic we replicate here.
     */
    private def genConversion(from: jstpe.Type, to: jstpe.Type, value: js.Tree)(
        implicit pos: Position): js.Tree = {
      import js.UnaryOp._

      if (from == to || from == jstpe.NothingType) {
        value
      } else if (from == jstpe.BooleanType || to == jstpe.BooleanType) {
        throw new AssertionError(s"Invalid genConversion from $from to $to")
      } else {
        def intValue = (from: @unchecked) match {
          case jstpe.IntType    => value
          case jstpe.CharType   => js.UnaryOp(CharToInt, value)
          case jstpe.ByteType   => js.UnaryOp(ByteToInt, value)
          case jstpe.ShortType  => js.UnaryOp(ShortToInt, value)
          case jstpe.LongType   => js.UnaryOp(LongToInt, value)
          case jstpe.FloatType  => js.UnaryOp(DoubleToInt, js.UnaryOp(FloatToDouble, value))
          case jstpe.DoubleType => js.UnaryOp(DoubleToInt, value)
        }

        def doubleValue = from match {
          case jstpe.DoubleType => value
          case jstpe.FloatType  => js.UnaryOp(FloatToDouble, value)
          case jstpe.LongType   => js.UnaryOp(LongToDouble, value)
          case _                => js.UnaryOp(IntToDouble, intValue)
        }

        (to: @unchecked) match {
          case jstpe.CharType =>
            js.UnaryOp(IntToChar, intValue)
          case jstpe.ByteType =>
            js.UnaryOp(IntToByte, intValue)
          case jstpe.ShortType =>
            js.UnaryOp(IntToShort, intValue)
          case jstpe.IntType =>
            intValue
          case jstpe.LongType =>
            from match {
              case jstpe.FloatType | jstpe.DoubleType =>
                js.UnaryOp(DoubleToLong, doubleValue)
              case _ =>
                js.UnaryOp(IntToLong, intValue)
            }
          case jstpe.FloatType =>
            if (from == jstpe.LongType)
              js.UnaryOp(js.UnaryOp.LongToFloat, value)
            else
              js.UnaryOp(js.UnaryOp.DoubleToFloat, doubleValue)
          case jstpe.DoubleType =>
            doubleValue
        }
      }
    }

    /** Gen JS code for an isInstanceOf test (for reference types only) */
    def genIsInstanceOf(value: js.Tree, to: Type)(
        implicit pos: Position): js.Tree = {

      val sym = to.typeSymbol

      if (sym == ObjectClass) {
        js.BinaryOp(js.BinaryOp.!==, value, js.Null())
      } else if (isJSType(sym)) {
        if (sym.isTrait) {
          reporter.error(pos,
              s"isInstanceOf[${sym.fullName}] not supported because it is a JS trait")
          js.BooleanLiteral(true)
        } else {
          js.AsInstanceOf(
              js.JSBinaryOp(js.JSBinaryOp.instanceof, value, genPrimitiveJSClass(sym)),
              jstpe.BooleanType)
        }
      } else {
        // The Scala type system prevents x.isInstanceOf[Null] and ...[Nothing]
        assert(sym != NullClass && sym != NothingClass,
            s"Found a .isInstanceOf[$sym] at $pos")
        js.IsInstanceOf(value, toIRType(to))
      }
    }

    /** Gen JS code for an asInstanceOf cast (for reference types only) */
    def genAsInstanceOf(value: js.Tree, to: Type)(
        implicit pos: Position): js.Tree = {

      def default: js.Tree =
        js.AsInstanceOf(value, toIRType(to))

      val sym = to.typeSymbol

      if (sym == ObjectClass || isJSType(sym)) {
        /* asInstanceOf[Object] always succeeds, and
         * asInstanceOf to a JS type is completely erased.
         */
        value
      } else if (sym == NullClass) {
        js.If(
            js.BinaryOp(js.BinaryOp.===, value, js.Null()),
            js.Null(),
            genThrowClassCastException())(
            jstpe.NullType)
      } else if (sym == NothingClass) {
        js.Block(value, genThrowClassCastException())
      } else {
        default
      }
    }

    /** Gen JS code for a call to a Scala class constructor. */
    def genNew(clazz: Symbol, ctor: Symbol, arguments: List[js.Tree])(
        implicit pos: Position): js.Tree = {
      assert(!isJSFunctionDef(clazz),
          s"Trying to instantiate a JS function def $clazz")
      genNew(encodeClassName(clazz), ctor, arguments)
    }

    /** Gen JS code for a call to a Scala class constructor. */
    def genNew(className: ClassName, ctor: Symbol, arguments: List[js.Tree])(
        implicit pos: Position): js.Tree = {
      js.New(className, encodeMethodSym(ctor), arguments)
    }

    /** Gen JS code for a call to a constructor of a hijacked class.
     *  Reroute them to the `new` method with the same signature in the
     *  companion object.
     */
    private def genNewHijackedClass(clazz: Symbol, ctor: Symbol,
        args: List[js.Tree])(implicit pos: Position): js.Tree = {

      val flags = js.ApplyFlags.empty
      val className = encodeClassName(clazz)

      val initName = encodeMethodSym(ctor).name
      val newName = MethodName(newSimpleMethodName, initName.paramTypeRefs,
          jstpe.ClassRef(className))
      val newMethodIdent = js.MethodIdent(newName)

      js.ApplyStatic(flags, className, newMethodIdent, args)(
          jstpe.ClassType(className))
    }

    /** Gen JS code for creating a new Array: new Array[T](length)
     *  For multidimensional arrays (dimensions > 1), the arguments can
     *  specify up to `dimensions` lengths for the first dimensions of the
     *  array.
     */
    def genNewArray(arrayTypeRef: jstpe.ArrayTypeRef, arguments: List[js.Tree])(
        implicit pos: Position): js.Tree = {
      assert(arguments.length <= arrayTypeRef.dimensions,
          "too many arguments for array constructor: found " + arguments.length +
          " but array has only " + arrayTypeRef.dimensions +
          " dimension(s)")

      js.NewArray(arrayTypeRef, arguments)
    }

    /** Gen JS code for an array literal. */
    def genArrayValue(tree: ArrayValue): js.Tree = {
      val ArrayValue(tpt @ TypeTree(), elems) = tree
      genArrayValue(tree, elems)
    }

    /** Gen JS code for an array literal, in the context of `tree` (its `tpe`
     *  and `pos`) but with the elements `elems`.
     */
    def genArrayValue(tree: Tree, elems: List[Tree]): js.Tree = {
      implicit val pos = tree.pos
      val arrayTypeRef = toTypeRef(tree.tpe).asInstanceOf[jstpe.ArrayTypeRef]
      js.ArrayValue(arrayTypeRef, elems.map(genExpr))
    }

    /** Gen JS code for a Match, i.e., a switch-able pattern match.
     *
     *  In most cases, this straightforwardly translates to a Match in the IR,
     *  which will eventually become a `switch` in JavaScript.
     *
     *  However, sometimes there is a guard in here, despite the fact that
     *  matches cannot have guards (in the JVM nor in the IR). The JVM backend
     *  emits a jump to the default clause when a guard is not fulfilled. We
     *  cannot do that, since we do not have arbitrary jumps. We therefore use
     *  a funny encoding with two nested `Labeled` blocks. For example,
     *  {{{
     *  x match {
     *    case 1 if y > 0 => a
     *    case 2          => b
     *    case _          => c
     *  }
     *  }}}
     *  arrives at the back-end as
     *  {{{
     *  x match {
     *    case 1 =>
     *      if (y > 0)
     *        a
     *      else
     *        default()
     *    case 2 =>
     *      b
     *    case _ =>
     *      default() {
     *        c
     *      }
     *  }
     *  }}}
     *  which we then translate into the following IR:
     *  {{{
     *  matchResult[I]: {
     *    default[V]: {
     *      x match {
     *        case 1 =>
     *          return(matchResult) if (y > 0)
     *            a
     *          else
     *            return(default) (void 0)
     *        case 2 =>
     *          return(matchResult) b
     *        case _ =>
     *          ()
     *      }
     *    }
     *    c
     *  }
     *  }}}
     */
    def genMatch(tree: Tree, isStat: Boolean): js.Tree = {
      implicit val pos = tree.pos
      val Match(selector, cases) = tree

      /* Although GenBCode adapts the scrutinee and the cases to `int`, only
       * true `int`s can reach the back-end, as asserted by the String-switch
       * transformation in `cleanup`. Therefore, we do not adapt, preserving
       * the `string`s and `null`s that come out of the pattern matching in
       * Scala 2.13.2+.
       */
      val genSelector = genExpr(selector)

      val resultType =
        if (isStat) jstpe.NoType
        else toIRType(tree.tpe)

      val defaultLabelSym = cases.collectFirst {
        case CaseDef(Ident(nme.WILDCARD), EmptyTree,
            body @ LabelDef(_, Nil, rhs)) if hasSynthCaseSymbol(body) =>
          body.symbol
      }.getOrElse(NoSymbol)

      var clauses: List[(List[js.MatchableLiteral], js.Tree)] = Nil
      var optElseClause: Option[js.Tree] = None
      var optElseClauseLabel: Option[js.LabelIdent] = None

      def genJumpToElseClause(implicit pos: ir.Position): js.Tree = {
        if (optElseClauseLabel.isEmpty)
          optElseClauseLabel = Some(freshLabelIdent("default"))
        js.Return(js.Undefined(), optElseClauseLabel.get)
      }

      for (caze @ CaseDef(pat, guard, body) <- cases) {
        assert(guard == EmptyTree, s"found a case guard at ${caze.pos}")

        def genBody(body: Tree): js.Tree = body match {
          case app @ Apply(_, Nil) if app.symbol == defaultLabelSym =>
            genJumpToElseClause
          case Block(List(app @ Apply(_, Nil)), _) if app.symbol == defaultLabelSym =>
            genJumpToElseClause

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

        def invalidCase(tree: Tree): Nothing =
          abort(s"Invalid case in alternative in switch-like pattern match: $tree at: ${tree.pos}")

        def genMatchableLiteral(tree: Literal): js.MatchableLiteral = {
          genExpr(tree) match {
            case matchableLiteral: js.MatchableLiteral => matchableLiteral
            case otherExpr                             => invalidCase(tree)
          }
        }

        pat match {
          case lit: Literal =>
            clauses = (List(genMatchableLiteral(lit)), genBody(body)) :: clauses
          case Ident(nme.WILDCARD) =>
            optElseClause = Some(body match {
              case LabelDef(_, Nil, rhs) if hasSynthCaseSymbol(body) =>
                genBody(rhs)
              case _ =>
                genBody(body)
            })
          case Alternative(alts) =>
            val genAlts = {
              alts map {
                case lit: Literal => genMatchableLiteral(lit)
                case _            => invalidCase(tree)
              }
            }
            clauses = (genAlts, genBody(body)) :: clauses
          case _ =>
            invalidCase(tree)
        }
      }

      val elseClause = optElseClause.getOrElse(
          throw new AssertionError("No elseClause in pattern match"))

      /* Builds a `js.Match`, but simplifies it to a `js.If` if there is only
       * one case with one alternative, and to a `js.Block` if there is no case
       * at all. This happens in practice in the standard library. Having no
       * case is a typical product of `match`es that are full of
       * `case n if ... =>`, which are used instead of `if` chains for
       * convenience and/or readability.
       */
      def buildMatch(cases: List[(List[js.MatchableLiteral], js.Tree)],
          default: js.Tree, tpe: jstpe.Type): js.Tree = {

        def isInt(tree: js.Tree): Boolean = tree.tpe == jstpe.IntType

        cases match {
          case Nil =>
            /* Completely remove the Match. Preserve the side-effects of
             * `genSelector`.
             */
            js.Block(exprToStat(genSelector), default)

          case (uniqueAlt :: Nil, caseRhs) :: Nil =>
            /* Simplify the `match` as an `if`, so that the optimizer has less
             * work to do, and we emit less code at the end of the day.
             * Use `Int_==` instead of `===` if possible, since it is a common
             * case.
             */
            val op =
              if (isInt(genSelector) && isInt(uniqueAlt)) js.BinaryOp.Int_==
              else js.BinaryOp.===
            js.If(js.BinaryOp(op, genSelector, uniqueAlt), caseRhs, default)(tpe)

          case _ =>
            // We have more than one case: use a js.Match
            js.Match(genSelector, cases, default)(tpe)
        }
      }

      optElseClauseLabel.fold[js.Tree] {
        buildMatch(clauses.reverse, elseClause, resultType)
      } { elseClauseLabel =>
        val matchResultLabel = freshLabelIdent("matchResult")
        val patchedClauses = for ((alts, body) <- clauses) yield {
          implicit val pos = body.pos
          val newBody =
            if (isStat) js.Block(body, js.Return(js.Undefined(), matchResultLabel))
            else js.Return(body, matchResultLabel)
          (alts, newBody)
        }
        js.Labeled(matchResultLabel, resultType, js.Block(List(
            js.Labeled(elseClauseLabel, jstpe.NoType, {
              buildMatch(patchedClauses.reverse, js.Skip(), jstpe.NoType)
            }),
            elseClause
        )))
      }
    }

    /** Flatten nested Blocks that can be flattened without compromising the
     *  identification of pattern matches.
     */
    private def flatStats(stats: List[Tree]): Iterator[Tree] = {
      /* #4581 Never decompose a Block with <case> LabelDef's, as they need to
       * be processed by genBlockWithCaseLabelDefs.
       */
      stats.iterator.flatMap {
        case Block(stats, expr) if !stats.exists(isCaseLabelDef(_)) =>
          stats.iterator ++ Iterator.single(expr)
        case tree =>
          Iterator.single(tree)
      }
    }

    /** Predicate satisfied by LabelDefs produced by the pattern matcher,
     *  except matchEnd's.
     */
    private def isCaseLabelDef(tree: Tree): Boolean = {
      tree.isInstanceOf[LabelDef] && hasSynthCaseSymbol(tree) &&
      !tree.symbol.name.startsWith("matchEnd")
    }

    /** Predicate satisfied by matchEnd LabelDefs produced by the pattern
     *  matcher.
     */
    private def isMatchEndLabelDef(tree: LabelDef): Boolean =
      hasSynthCaseSymbol(tree) && tree.symbol.name.startsWith("matchEnd")

    private def genBlock(tree: Block, isStat: Boolean): js.Tree = {
      implicit val pos = tree.pos
      val Block(stats, expr) = tree

      val genStatsAndExpr = if (!stats.exists(isCaseLabelDef(_))) {
        // #4684 Collapse { <undefined-param>; BoxedUnit } to <undefined-param>
        val genStatsAndExpr0 = stats.map(genStat(_)) :+ genStatOrExpr(expr, isStat)
        genStatsAndExpr0 match {
          case (undefParam @ js.Transient(UndefinedParam)) :: js.Undefined() :: Nil =>
            undefParam :: Nil
          case _ =>
            genStatsAndExpr0
        }
      } else {
        genBlockWithCaseLabelDefs(stats :+ expr, isStat)
      }

      /* A bit of dead code elimination: we drop all statements and
       * expressions after the first statement of type `NothingType`.
       * This helps other optimizations.
       */
      val (nonNothing, rest) = genStatsAndExpr.span(_.tpe != jstpe.NothingType)
      if (rest.isEmpty || rest.tail.isEmpty)
        js.Block(genStatsAndExpr)
      else
        js.Block(nonNothing, rest.head)
    }

    private def genBlockWithCaseLabelDefs(trees: List[Tree], isStat: Boolean)(
        implicit pos: Position): List[js.Tree] = {

      val (prologue, casesAndRest) = trees.span(!isCaseLabelDef(_))

      if (casesAndRest.isEmpty) {
        if (prologue.isEmpty) Nil
        else if (isStat) prologue.map(genStat(_))
        else prologue.init.map(genStat(_)) :+ genExpr(prologue.last)
      } else {
        val genPrologue = prologue.map(genStat(_))

        val (cases0, rest) = casesAndRest.span(isCaseLabelDef(_))
        val cases = cases0.asInstanceOf[List[LabelDef]]

        val genCasesAndRest = rest match {
          case (matchEnd: LabelDef) :: more if isMatchEndLabelDef(matchEnd) =>
            val translatedMatch = genTranslatedMatch(cases, matchEnd)
            translatedMatch :: genBlockWithCaseLabelDefs(more, isStat)

          // Sometimes the pattern matcher casts its final result
          case Apply(TypeApply(Select(matchEnd: LabelDef, nme.asInstanceOf_Ob),
              List(targ)), Nil) :: more
              if isMatchEndLabelDef(matchEnd) =>
            val translatedMatch = genTranslatedMatch(cases, matchEnd)
            genIsAsInstanceOf(translatedMatch, matchEnd.tpe, targ.tpe,
                cast = true) :: genBlockWithCaseLabelDefs(more, isStat)

          // Peculiar shape generated by `return x match {...}` - #2928
          case Return(matchEnd: LabelDef) :: more if isMatchEndLabelDef(matchEnd) =>
            val translatedMatch = genTranslatedMatch(cases, matchEnd)
            val genMore = genBlockWithCaseLabelDefs(more, isStat)
            val label = getEnclosingReturnLabel()
            if (translatedMatch.tpe == jstpe.NoType) {
              // Could not actually reproduce this, but better be safe than sorry
              translatedMatch :: js.Return(js.Undefined(), label) :: genMore
            } else {
              js.Return(translatedMatch, label) :: genMore
            }

          // Otherwise, there is no matchEnd, only consecutive cases
          case Nil =>
            genTranslatedCases(cases, isStat)
          case _ =>
            genTranslatedCases(cases, isStat = false) ::: genBlockWithCaseLabelDefs(rest, isStat)
        }

        genPrologue ::: genCasesAndRest
      }
    }

    /** Gen JS code for a translated match.
     *
     *  A translated match consists of consecutive `case` LabelDefs directly
     *  followed by a `matchEnd` LabelDef.
     */
    private def genTranslatedMatch(cases: List[LabelDef], matchEnd: LabelDef)(
        implicit pos: Position): js.Tree = {
      genMatchEnd(matchEnd) {
        genTranslatedCases(cases, isStat = true)
      }
    }

    /** Gen JS code for the cases of a patmat-transformed match.
     *
     *  This implementation relies heavily on the patterns of trees emitted
     *  by the pattern match phase, including its variants across versions of
     *  scalac that we support.
     *
     *  The trees output by the pattern matcher are assumed to follow these
     *  rules:
     *
     *  - Each case LabelDef (in `cases`) must not take any argument.
     *  - Jumps to case label-defs are restricted to jumping to the very next
     *    case.
     *
     *  There is an optimization to avoid generating jumps that are in tail
     *  position of a case, if they are in positions denoted by <jump> in:
     *  {{{
     *  <case-body> ::=
     *      If(_, <case-body>, <case-body>)
     *    | Block(_, <case-body>)
     *    | <jump>
     *    | _
     *  }}}
     *  Since all but the last case (which cannot have jumps) are in statement
     *  position, those jumps in tail position can be replaced by `skip`.
     */
    private def genTranslatedCases(cases: List[LabelDef], isStat: Boolean)(
        implicit pos: Position): List[js.Tree] = {

      assert(!cases.isEmpty,
          s"genTranslatedCases called with no cases at $pos")

      val translatedCasesInit = for {
        (caseLabelDef, nextCaseSym) <- cases.zip(cases.tail.map(_.symbol))
      } yield {
        implicit val pos = caseLabelDef.pos
        assert(caseLabelDef.params.isEmpty,
            s"found case LabelDef with parameters at $pos")

        val info = new EnclosingLabelDefInfoWithResultAsAssigns(Nil)

        val translatedBody = withScopedVars(
            enclosingLabelDefInfos :=
              enclosingLabelDefInfos.get + (nextCaseSym -> info)
        ) {
          /* Eager optimization of jumps in tail position, following the shapes
           * produced by scala until 2.12.8. 2.12.9 introduced flat patmat
           * translation, which does not trigger those optimizations.
           * These shapes are also often produced by the async transformation.
           */
          def genCaseBody(tree: Tree): js.Tree = {
            implicit val pos = tree.pos
            tree match {
              case If(cond, thenp, elsep) =>
                js.If(genExpr(cond), genCaseBody(thenp), genCaseBody(elsep))(
                    jstpe.NoType)

              case Block(stats, Literal(Constant(()))) =>
                // Generated a lot by the async transform
                if (stats.isEmpty) js.Skip()
                else js.Block(stats.init.map(genStat(_)), genCaseBody(stats.last))

              case Block(stats, expr) =>
                js.Block((stats map genStat) :+ genCaseBody(expr))

              case Apply(_, Nil) if tree.symbol == nextCaseSym =>
                js.Skip()

              case _ =>
                genStat(tree)
            }
          }

          genCaseBody(caseLabelDef.rhs)
        }

        genOptimizedCaseLabeled(encodeLabelSym(nextCaseSym), translatedBody,
            info.generatedReturns)
      }

      val translatedLastCase = genStatOrExpr(cases.last.rhs, isStat)

      translatedCasesInit :+ translatedLastCase
    }

    /** Gen JS code for a match-end label def following match-cases.
     *
     *  The preceding cases, which are allowed to jump to this match-end, must
     *  be generated in the `genTranslatedCases` callback. During the execution
     *  of this callback, the enclosing label infos contain appropriate info
     *  for this match-end.
     *
     *  The translation of the match-end itself is straightforward, but is
     *  augmented with several optimizations to remove as many labeled blocks
     *  as possible.
     *
     *  Most of the time, a match-end label has exactly one parameter. However,
     *  with the async transform, it can sometimes have no parameter instead.
     *  We handle those cases very differently.
     */
    private def genMatchEnd(matchEnd: LabelDef)(
        genTranslatedCases: => List[js.Tree])(
        implicit pos: Position): js.Tree = {

      val sym = matchEnd.symbol
      val labelIdent = encodeLabelSym(sym)
      val matchEndBody = matchEnd.rhs

      def genMatchEndBody(): js.Tree = {
        genStatOrExpr(matchEndBody,
            isStat = toIRType(matchEndBody.tpe) == jstpe.NoType)
      }

      matchEnd.params match {
        // Optimizable common case produced by the regular pattern matcher
        case List(matchEndParam) =>
          val info = new EnclosingLabelDefInfoWithResultAsReturn()

          val translatedCases = withScopedVars(
              enclosingLabelDefInfos := enclosingLabelDefInfos.get + (sym -> info)
          ) {
            genTranslatedCases
          }

          val innerResultType = toIRType(matchEndParam.tpe)
          val optimized = genOptimizedMatchEndLabeled(encodeLabelSym(sym),
              innerResultType, translatedCases, info.generatedReturns)

          matchEndBody match {
            case Ident(_) if matchEndParam.symbol == matchEndBody.symbol =>
              // matchEnd is identity.
              optimized

            case Literal(Constant(())) =>
              // Unit return type.
              optimized

            case _ =>
              // matchEnd does something.
              js.Block(
                  js.VarDef(encodeLocalSym(matchEndParam.symbol),
                      originalNameOfLocal(matchEndParam.symbol),
                      innerResultType, mutable = false, optimized),
                  genMatchEndBody())
          }

        /* Other cases, notably the matchEnd's produced by the async transform,
         * which have no parameters. The case of more than one parameter is
         * hypothetical, but it costs virtually nothing to handle it here.
         */
        case params =>
          val paramSyms = params.map(_.symbol)
          val varDefs = for (s <- paramSyms) yield {
            implicit val pos = s.pos
            val irType = toIRType(s.tpe)
            js.VarDef(encodeLocalSym(s), originalNameOfLocal(s), irType,
                mutable = true, jstpe.zeroOf(irType))
          }
          val info = new EnclosingLabelDefInfoWithResultAsAssigns(paramSyms)
          val translatedCases = withScopedVars(
              enclosingLabelDefInfos := enclosingLabelDefInfos.get + (sym -> info)
          ) {
            genTranslatedCases
          }
          val optimized = genOptimizedMatchEndLabeled(labelIdent, jstpe.NoType,
              translatedCases, info.generatedReturns)
          js.Block(varDefs ::: optimized :: genMatchEndBody() :: Nil)
      }
    }

    /** Gen JS code for a Labeled block from a pattern match'es case, while
     *  trying to optimize it away as a reversed If.
     *
     *  If there was no `return` to the label at all, simply avoid generating
     *  the `Labeled` block altogether.
     *
     *  If there was more than one `return`, do not optimize anything, as
     *  nothing could be good enough for `genOptimizedMatchEndLabeled` to do
     *  anything useful down the line.
     *
     *  If however there was a single `return`, we try and get rid of it by
     *  identifying the following shape:
     *
     *  {{{
     *  {
     *    ...stats1
     *    if (test)
     *      return(nextCaseSym)
     *    ...stats2
     *  }
     *  }}}
     *
     *  which we then rewrite as
     *
     *  {{{
     *  {
     *    ...stats1
     *    if (!test) {
     *      ...stats2
     *    }
     *  }
     *  }}}
     *
     *  The above rewrite is important for `genOptimizedMatchEndLabeled` below
     *  to be able to do its job, which in turn is important for the IR
     *  optimizer to perform a better analysis.
     *
     *  This whole thing is only necessary in Scala 2.12.9+, with the new flat
     *  patmat ASTs. In previous versions, `returnCount` is always 0 because
     *  all jumps to case labels are already caught upstream by `genCaseBody()`
     *  inside `genTranslatedMatch()`.
     */
    private def genOptimizedCaseLabeled(label: js.LabelIdent,
        translatedBody: js.Tree, returnCount: Int)(
        implicit pos: Position): js.Tree = {

      def default: js.Tree =
        js.Labeled(label, jstpe.NoType, translatedBody)

      if (returnCount == 0) {
        translatedBody
      } else if (returnCount > 1) {
        default
      } else {
        translatedBody match {
          case js.Block(stats) =>
            val (stats1, testAndStats2) = stats.span {
              case js.If(_, js.Return(js.Undefined(), `label`), js.Skip()) =>
                false
              case _ =>
                true
            }

            testAndStats2 match {
              case js.If(cond, _, _) :: stats2 =>
                val notCond = cond match {
                  case js.UnaryOp(js.UnaryOp.Boolean_!, notCond) =>
                    notCond
                  case _ =>
                    js.UnaryOp(js.UnaryOp.Boolean_!, cond)
                }
                js.Block(stats1 :+ js.If(notCond, js.Block(stats2), js.Skip())(jstpe.NoType))

              case _ :: _ =>
                throw new AssertionError("unreachable code")

              case Nil =>
                default
            }

          case _ =>
            default
        }
      }
    }

    /** Gen JS code for a Labeled block from a pattern match'es match-end,
     *  while trying to optimize it away as an If chain.
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
    def genOptimizedMatchEndLabeled(label: js.LabelIdent, tpe: jstpe.Type,
        translatedCases: List[js.Tree], returnCount: Int)(
        implicit pos: Position): js.Tree = {
      def default: js.Tree =
        js.Labeled(label, tpe, js.Block(translatedCases))

      @tailrec
      def createRevAlts(xs: List[js.Tree],
          acc: List[(js.Tree, js.Tree)]): (List[(js.Tree, js.Tree)], js.Tree) = xs match {
        case js.If(cond, body, js.Skip()) :: xr =>
          createRevAlts(xr, (cond, body) :: acc)
        case remaining =>
          (acc, js.Block(remaining)(remaining.head.pos))
      }
      val (revAlts, elsep) = createRevAlts(translatedCases, Nil)

      if (revAlts.size == returnCount - 1) {
        def tryDropReturn(body: js.Tree): Option[js.Tree] = body match {
          case js.Return(result, `label`) =>
            Some(result)

          case js.Block(prep :+ js.Return(result, `label`)) =>
            Some(js.Block(prep :+ result)(body.pos))

          case _ =>
            None
        }

        @tailrec
        def constructOptimized(revAlts: List[(js.Tree, js.Tree)],
            elsep: js.Tree): js.Tree = {
          revAlts match {
            case (cond, body) :: revAltsRest =>
              // cannot use flatMap due to tailrec
              tryDropReturn(body) match {
                case Some(newBody) =>
                  constructOptimized(revAltsRest,
                      js.If(cond, newBody, elsep)(tpe)(cond.pos))

                case None =>
                  default
              }
            case Nil =>
              elsep
          }
        }

        tryDropReturn(elsep).fold(default)(constructOptimized(revAlts, _))
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
        genSynchronized(receiver, args.head, isStat)
      else if (isCoercion(code))
        genCoercion(tree, receiver, code)
      else if (jsPrimitives.isJavaScriptPrimitive(code))
        genJSPrimitive(tree, args, code, isStat)
      else
        abort("Unknown primitive operation: " + sym.fullName + "(" +
            fun.symbol.simpleName + ") " + " at: " + (tree.pos))
    }

    private def genPrimitiveOpForReflectiveCall(sym: Symbol, receiver: js.Tree,
        args: List[js.Tree])(
        implicit pos: Position): js.Tree = {

      import scalaPrimitives._

      if (!isPrimitive(sym)) {
        abort(
            "Trying to reflectively call a method of a primitive type that " +
            "is not itself a primitive method: " + sym.fullName + " at " + pos)
      }
      val code = getPrimitive(sym)

      if (isArithmeticOp(code) || isLogicalOp(code) || isComparisonOp(code)) {
        genSimpleOp(sym.owner.tpe :: sym.tpe.paramTypes, sym.tpe.resultType,
            receiver :: args, code)
      } else if (code == CONCAT) {
        js.BinaryOp(js.BinaryOp.String_+, receiver, args.head)
      } else if (isCoercion(code)) {
        adaptPrimitive(receiver, toIRType(sym.tpe.resultType))
      } else {
        abort(
            "Unknown primitive operation for reflective call: " + sym.fullName +
            " at " + pos)
      }
    }

    /** Gen JS code for a simple operation (arithmetic, logical, or comparison) */
    private def genSimpleOp(tree: Apply, args: List[Tree], code: Int): js.Tree = {
      implicit val pos = tree.pos

      genSimpleOp(args.map(_.tpe), tree.tpe, args.map(genExpr), code)
    }

    /** Gen JS code for a simple operation (arithmetic, logical, or comparison) */
    private def genSimpleOp(argTpes: List[Type], resultTpe: Type,
        sources: List[js.Tree], code: Int)(
        implicit pos: Position): js.Tree = {

      import scalaPrimitives._

      sources match {
        // Unary operation
        case List(src_in) =>
          val opType = toIRType(resultTpe)
          val src = adaptPrimitive(src_in, opType)

          (code match {
            case POS =>
              src
            case NEG =>
              (opType: @unchecked) match {
                case jstpe.IntType =>
                  js.BinaryOp(js.BinaryOp.Int_-, js.IntLiteral(0), src)
                case jstpe.LongType =>
                  js.BinaryOp(js.BinaryOp.Long_-, js.LongLiteral(0), src)
                case jstpe.FloatType =>
                  js.BinaryOp(js.BinaryOp.Float_*, js.FloatLiteral(-1.0f), src)
                case jstpe.DoubleType =>
                  js.BinaryOp(js.BinaryOp.Double_*, js.DoubleLiteral(-1.0), src)
              }
            case NOT =>
              (opType: @unchecked) match {
                case jstpe.IntType =>
                  js.BinaryOp(js.BinaryOp.Int_^, js.IntLiteral(-1), src)
                case jstpe.LongType =>
                  js.BinaryOp(js.BinaryOp.Long_^, js.LongLiteral(-1), src)
              }
            case ZNOT =>
              js.UnaryOp(js.UnaryOp.Boolean_!, src)
            case _ =>
              abort("Unknown unary operation code: " + code)
          })

        // Binary operation
        case List(lsrc_in, rsrc_in) =>
          import js.BinaryOp._

          val isShift = isShiftOp(code)
          val leftIRType = toIRType(argTpes(0))
          val rightIRType = toIRType(argTpes(1))

          val opType = {
            if (isShift) {
              if (leftIRType == jstpe.LongType) jstpe.LongType
              else jstpe.IntType
            } else {
              (leftIRType, rightIRType) match {
                case (jstpe.DoubleType, _) | (_, jstpe.DoubleType) =>
                  jstpe.DoubleType
                case (jstpe.FloatType, _) | (_, jstpe.FloatType) =>
                  jstpe.FloatType
                case (jstpe.LongType, _) | (_, jstpe.LongType) =>
                  jstpe.LongType
                case (jstpe.IntType | jstpe.CharType | jstpe.ByteType | jstpe.ShortType, _) |
                    (_, jstpe.IntType | jstpe.CharType | jstpe.ByteType | jstpe.ShortType) =>
                  jstpe.IntType
                case (jstpe.BooleanType, _) | (_, jstpe.BooleanType) =>
                  jstpe.BooleanType
                case _ =>
                  jstpe.AnyType
              }
            }
          }

          val lsrc =
            if (opType == jstpe.AnyType) lsrc_in
            else adaptPrimitive(lsrc_in, opType)
          val rsrc =
            if (opType == jstpe.AnyType) rsrc_in
            else adaptPrimitive(rsrc_in, if (isShift) jstpe.IntType else opType)

          (opType: @unchecked) match {
            case jstpe.IntType =>
              val op = (code: @switch) match {
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
                case EQ  => Int_==
                case NE  => Int_!=
                case LT  => Int_<
                case LE  => Int_<=
                case GT  => Int_>
                case GE  => Int_>=
              }
              js.BinaryOp(op, lsrc, rsrc)

            case jstpe.LongType =>
              val op = (code: @switch) match {
                case ADD => Long_+
                case SUB => Long_-
                case MUL => Long_*
                case DIV => Long_/
                case MOD => Long_%
                case OR  => Long_|
                case XOR => Long_^
                case AND => Long_&
                case LSL => Long_<<
                case LSR => Long_>>>
                case ASR => Long_>>
                case EQ  => Long_==
                case NE  => Long_!=
                case LT  => Long_<
                case LE  => Long_<=
                case GT  => Long_>
                case GE  => Long_>=
              }
              js.BinaryOp(op, lsrc, rsrc)

            case jstpe.FloatType =>
              def withFloats(op: Int): js.Tree =
                js.BinaryOp(op, lsrc, rsrc)

              def toDouble(value: js.Tree): js.Tree =
                js.UnaryOp(js.UnaryOp.FloatToDouble, value)

              def withDoubles(op: Int): js.Tree =
                js.BinaryOp(op, toDouble(lsrc), toDouble(rsrc))

              (code: @switch) match {
                case ADD => withFloats(Float_+)
                case SUB => withFloats(Float_-)
                case MUL => withFloats(Float_*)
                case DIV => withFloats(Float_/)
                case MOD => withFloats(Float_%)

                case EQ  => withDoubles(Double_==)
                case NE  => withDoubles(Double_!=)
                case LT  => withDoubles(Double_<)
                case LE  => withDoubles(Double_<=)
                case GT  => withDoubles(Double_>)
                case GE  => withDoubles(Double_>=)
              }

            case jstpe.DoubleType =>
              val op = (code: @switch) match {
                case ADD => Double_+
                case SUB => Double_-
                case MUL => Double_*
                case DIV => Double_/
                case MOD => Double_%
                case EQ  => Double_==
                case NE  => Double_!=
                case LT  => Double_<
                case LE  => Double_<=
                case GT  => Double_>
                case GE  => Double_>=
              }
              js.BinaryOp(op, lsrc, rsrc)

            case jstpe.BooleanType =>
              (code: @switch) match {
                case OR =>
                  js.BinaryOp(Boolean_|, lsrc, rsrc)
                case AND =>
                  js.BinaryOp(Boolean_&, lsrc, rsrc)
                case EQ =>
                  js.BinaryOp(Boolean_==, lsrc, rsrc)
                case XOR | NE =>
                  js.BinaryOp(Boolean_!=, lsrc, rsrc)
                case ZOR =>
                  js.If(lsrc, js.BooleanLiteral(true), rsrc)(jstpe.BooleanType)
                case ZAND =>
                  js.If(lsrc, rsrc, js.BooleanLiteral(false))(jstpe.BooleanType)
              }

            case jstpe.AnyType =>
              def genAnyEquality(eqeq: Boolean, not: Boolean): js.Tree = {
                // Arrays, Null, Nothing never have a custom equals() method
                def canHaveCustomEquals(tpe: jstpe.Type): Boolean = tpe match {
                  case jstpe.AnyType | jstpe.ClassType(_) => true
                  case _                                  => false
                }
                if (eqeq &&
                    // don't call equals if we have a literal null at either side
                    !lsrc.isInstanceOf[js.Null] &&
                    !rsrc.isInstanceOf[js.Null] &&
                    canHaveCustomEquals(leftIRType)) {
                  val body = genEqEqPrimitive(argTpes(0), argTpes(1), lsrc, rsrc)
                  if (not) js.UnaryOp(js.UnaryOp.Boolean_!, body) else body
                } else {
                  js.BinaryOp(
                      if (not) js.BinaryOp.!== else js.BinaryOp.===,
                      lsrc, rsrc)
                }
              }

              (code: @switch) match {
                case EQ => genAnyEquality(eqeq = true, not = false)
                case NE => genAnyEquality(eqeq = true, not = true)
                case ID => genAnyEquality(eqeq = false, not = false)
                case NI => genAnyEquality(eqeq = false, not = true)
              }
          }

        case _ =>
          abort("Too many arguments for primitive function at " + pos)
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
       * **which includes when either is a JS type**.
       *
       * When it is statically known that both sides are equal and subtypes of
       * Number or Character, not using the rich equality is possible (their
       * own equals method will do ok), except for java.lang.Float and
       * java.lang.Double: their `equals` have different behavior around `NaN`
       * and `-0.0`, see Javadoc (scala-dev#329, #2799).
       */
      val mustUseAnyComparator: Boolean = {
        val lsym = ltpe.typeSymbol
        val rsym = rtpe.typeSymbol
        isJSType(lsym) || isJSType(rsym) || {
          isMaybeBoxed(lsym) && isMaybeBoxed(rsym) && {
            val areSameFinals =
              ltpe.isFinalType && rtpe.isFinalType && lsym == rsym
            !areSameFinals || (lsym == BoxedFloatClass || lsym == BoxedDoubleClass)
          }
        }
      }

      if (mustUseAnyComparator) {
        val equalsMethod: Symbol = {
          // scalastyle:off line.size.limit
          if (ltpe <:< BoxedNumberClass.tpe) {
            if (rtpe <:< BoxedNumberClass.tpe) platform.externalEqualsNumNum
            else if (rtpe <:< BoxedCharacterClass.tpe) platform.externalEqualsNumObject // will be externalEqualsNumChar in 2.12, SI-9030
            else platform.externalEqualsNumObject
          } else platform.externalEquals
          // scalastyle:on line.size.limit
        }
        if (BoxesRunTimeClass.isJavaDefined)
          genApplyStatic(equalsMethod, List(lsrc, rsrc))
        else // this happens when in the same compilation run as BoxesRunTime
          genApplyMethod(genLoadModule(BoxesRunTimeClass), equalsMethod, List(lsrc, rsrc))
      } else {
        // if (lsrc eq null) rsrc eq null else lsrc.equals(rsrc)
        if (isStringType(ltpe)) {
          // String.equals(that) === (this eq that)
          js.BinaryOp(js.BinaryOp.===, lsrc, rsrc)
        } else {
          /* This requires to evaluate both operands in local values first.
           * The optimizer will eliminate them if possible.
           */
          val ltemp = js.VarDef(freshLocalIdent(), NoOriginalName, lsrc.tpe,
              mutable = false, lsrc)
          val rtemp = js.VarDef(freshLocalIdent(), NoOriginalName, rsrc.tpe,
              mutable = false, rsrc)
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
      assert(!isPrimitiveValueType(receiver.tpe) || isStringType(args.head.tpe),
          s"unexpected signature for string-concat call at $pos")
      assert(!isPrimitiveValueType(args.head.tpe),
          s"unexpected signature for string-concat call at $pos")

      val rhs = genExpr(args.head)

      val lhs = {
        val lhs0 = genExpr(receiver)
        // Box the receiver if it is a primitive value
        if (!isPrimitiveValueType(receiver.tpe)) lhs0
        else makePrimitiveBox(lhs0, receiver.tpe)
      }

      js.BinaryOp(js.BinaryOp.String_+, lhs, rhs)
    }

    /** Gen JS code for a call to `Any.##`.
     *
     *  This method unconditionally generates a call to `Statics.anyHash`.
     *  On the JVM, `anyHash` is only called as of 2.12.0-M5. Previous versions
     *  emitted a call to `ScalaRunTime.hash`. However, since our `anyHash`
     *  is always consistent with `ScalaRunTime.hash`, we always use it.
     */
    private def genScalaHash(tree: Apply, receiver: Tree): js.Tree = {
      implicit val pos = tree.pos

      val instance = genLoadModule(RuntimeStaticsModule)
      val arguments = List(genExpr(receiver))
      val sym = getMember(RuntimeStaticsModule, jsnme.anyHash)

      genApplyMethod(instance, sym, arguments)
    }

    /** Gen JS code for an array operation (get, set or length) */
    private def genArrayOp(tree: Tree, code: Int): js.Tree = {
      import scalaPrimitives._

      implicit val pos = tree.pos

      val Apply(fun @ Select(arrayObj, _), args) = tree
      val arrayValue = genExpr(arrayObj)
      val arguments = args map genExpr

      def genSelect(elemType: Type) =
        js.ArraySelect(arrayValue, arguments(0))(toIRType(elemType))

      if (scalaPrimitives.isArrayGet(code)) {
        // get an item of the array
        assert(args.length == 1,
            s"Array get requires 1 argument, found ${args.length} in $tree")
        genSelect(fun.tpe.resultType)
      } else if (scalaPrimitives.isArraySet(code)) {
        // set an item of the array
        assert(args.length == 2,
            s"Array set requires 2 arguments, found ${args.length} in $tree")
        js.Assign(genSelect(fun.tpe.paramTypes(1)), arguments(1))
      } else {
        // length of the array
        js.ArrayLength(arrayValue)
      }
    }

    /** Gen JS code for a call to AnyRef.synchronized */
    private def genSynchronized(receiver: Tree, arg: Tree, isStat: Boolean)(
        implicit pos: Position): js.Tree = {
      /* JavaScript is single-threaded, so we can drop the
       * synchronization altogether.
       */
      val newReceiver = genExpr(receiver)
      val newArg = genStatOrExpr(arg, isStat)
      newReceiver match {
        case js.This() =>
          // common case for which there is no side-effect nor NPE
          newArg
        case _ =>
          js.Block(
              js.GetClass(newReceiver), // null check
              newArg)
      }
    }

    /** Gen JS code for a coercion */
    private def genCoercion(tree: Apply, receiver: Tree, code: Int): js.Tree = {
      implicit val pos = tree.pos

      val source = genExpr(receiver)
      val resultType = toIRType(tree.tpe)
      adaptPrimitive(source, resultType)
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
      val name = sym.name
      val params = sym.tpe.params

      /* Is this a primitive method introduced in AnyRef?
       * The concerned methods are `eq`, `ne` and `synchronized`.
       *
       * If it is, it can be defined in a custom value class. Calling it
       * reflectively works on the JVM in that case. However, it does not work
       * if the reflective call should in fact resolve to the method in
       * `AnyRef` (it causes a `NoSuchMethodError`). We maintain bug
       * compatibility for these methods: they work if redefined in a custom
       * AnyVal, and fail at run-time (with a `TypeError`) otherwise.
       */
      val isAnyRefPrimitive = {
        (name == nme.eq || name == nme.ne || name == nme.synchronized_) &&
        params.size == 1 && params.head.tpe.typeSymbol == ObjectClass
      }

      /** check if the method we are invoking conforms to a method on
       *  scala.Array. If this is the case, we check that case specially at
       *  runtime to avoid having reflective call proxies on scala.Array.
       *  (Also, note that the element type of Array#update is not erased and
       *  therefore the method name mangling would turn out wrong)
       *
       *  Note that we cannot check if the expected return type is correct,
       *  since this type information is already erased.
       */
      def isArrayLikeOp = name match {
        case nme.update =>
          params.size == 2 && params.head.tpe.typeSymbol == IntClass
        case nme.apply =>
          params.size == 1 && params.head.tpe.typeSymbol == IntClass
        case nme.length =>
          params.size == 0
        case nme.clone_ =>
          params.size == 0
        case _ =>
          false
      }

      /**
       * Tests whether one of our reflective "boxes" for primitive types
       * implements the particular method. If this is the case
       * (result != NoSymbol), we generate a runtime instance check if we are
       * dealing with the appropriate primitive type.
       */
      def matchingSymIn(clazz: Symbol) = clazz.tpe.member(name).suchThat { s =>
        val sParams = s.tpe.params
        !s.isBridge &&
        params.size == sParams.size &&
        (params zip sParams).forall { case (s1,s2) =>
          s1.tpe =:= s2.tpe
        }
      }

      val ApplyDynamic(receiver, args) = tree

      val receiverType = toIRType(receiver.tpe)
      val callTrgIdent = freshLocalIdent()
      val callTrgVarDef = js.VarDef(callTrgIdent, NoOriginalName, receiverType,
          mutable = false, genExpr(receiver))
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

      var callStatement: js.Tree = js.Apply(js.ApplyFlags.empty, callTrg,
          encodeMethodSym(sym, reflProxy = true), arguments)(jstpe.AnyType)

      if (!isAnyRefPrimitive) {
        def boxIfNeeded(call: js.Tree, returnType: Type): js.Tree = {
          if (isPrimitiveValueType(returnType))
            makePrimitiveBox(call, returnType)
          else
            call
        }

        if (isArrayLikeOp) {
          def genRTCall(method: Symbol, args: js.Tree*) =
            genApplyMethod(genLoadModule(ScalaRunTimeModule),
                method, args.toList)
          val isArrayTree =
            genRTCall(ScalaRunTime_isArray, callTrg, js.IntLiteral(1))
          callStatement = js.If(isArrayTree, {
            name match {
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

        /* Add an explicit type test for a hijacked class with a call to a
         * hijacked method, if applicable (i.e., if there is a matching method
         * in the given hijacked class). This is necessary because hijacked
         * classes of the IR do not support reflective proxy calls.
         *
         * Returns true if this treatment was applicable.
         */
        def addCallToHijackedMethodIfApplicable(hijackedClass: Symbol): Boolean = {
          val hijackedMethod = matchingSymIn(hijackedClass)
          val isApplicable =
            hijackedMethod != NoSymbol && hijackedMethod.isPublic
          if (isApplicable) {
            val hijackedClassTpe = hijackedClass.tpe
            callStatement = js.If(genIsInstanceOf(callTrg, hijackedClassTpe), {
              boxIfNeeded(
                  genApplyMethod(genAsInstanceOf(callTrg, hijackedClassTpe),
                      hijackedMethod, arguments),
                  hijackedMethod.tpe.resultType)
            }, { // else
              callStatement
            })(jstpe.AnyType)
          }
          isApplicable
        }

        // String is a hijacked class
        addCallToHijackedMethodIfApplicable(StringClass)

        /* For primitive types, we need to handle two cases. The method could
         * either be defined in the boxed class of the primitive type (which is
         * hijacked), or it could be defined in the primitive class itself.
         * If the hijacked class treatment is not applicable, we also try the
         * primitive treatment, in which case we directly generate the
         * primitive operation.
         */

        def addCallForPrimitive(primitiveClass: Symbol): Boolean = {
          val boxedClass = definitions.boxedClass(primitiveClass)
          if (addCallToHijackedMethodIfApplicable(boxedClass)) {
            true
          } else {
            val methodInPrimClass = matchingSymIn(primitiveClass)
            if (methodInPrimClass != NoSymbol && methodInPrimClass.isPublic) {
              def isIntOrLong(tpe: jstpe.Type): Boolean = tpe match {
                case jstpe.ByteType | jstpe.ShortType | jstpe.IntType | jstpe.LongType =>
                  true
                case _ =>
                  false
              }
              val ignoreBecauseItMustBeAnInt = {
                primitiveClass == DoubleClass &&
                toIRType(methodInPrimClass.tpe.resultType) == jstpe.DoubleType &&
                isIntOrLong(toIRType(sym.tpe.resultType))
              }
              if (ignoreBecauseItMustBeAnInt) {
                // Fall through to the Int case that will come next
                false
              } else {
                val boxedTpe = boxedClass.tpe
                callStatement = js.If(genIsInstanceOf(callTrg, boxedTpe), {
                  val castCallTrg =
                    makePrimitiveUnbox(callTrg, primitiveClass.tpe)
                  val call = genPrimitiveOpForReflectiveCall(methodInPrimClass,
                      castCallTrg, arguments)
                  boxIfNeeded(call, methodInPrimClass.tpe.resultType)
                }, { // else
                  callStatement
                })(jstpe.AnyType)
                true
              }
            } else {
              false
            }
          }
        }

        addCallForPrimitive(BooleanClass)
        addCallForPrimitive(LongClass)
        addCallForPrimitive(CharClass)

        /* For primitive numeric types that box as JS numbers, find the first
         * one that matches. It will be able to handle the subsequent cases.
         */
        Seq(DoubleClass, IntClass, FloatClass, ShortClass, ByteClass).find(
            addCallForPrimitive)
      }

      js.Block(callTrgVarDef, callStatement)
    }

    /** Ensures that the value of the given tree is boxed when used as a method result value.
     *  @param expr Tree to be boxed if needed.
     *  @param sym Method symbol this is the result of.
      */
    def ensureResultBoxed(expr: js.Tree, methodSym: Symbol)(
        implicit pos: Position): js.Tree = {
      val tpeEnteringPosterasure =
        enteringPhase(currentRun.posterasurePhase)(methodSym.tpe.resultType)
      ensureBoxed(expr, tpeEnteringPosterasure)
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
      toIRType(tpe) match {
        case jstpe.NoType => // for JS interop cases
          js.Block(expr, js.Undefined())
        case jstpe.BooleanType | jstpe.CharType | jstpe.ByteType |
            jstpe.ShortType | jstpe.IntType | jstpe.LongType | jstpe.FloatType |
            jstpe.DoubleType =>
          expr // box is identity for all those primitive types
        case _ =>
          abort(s"makePrimitiveBox requires a primitive type, found $tpe at $pos")
      }
    }

    /** Gen an unboxing operation (tpe is the primitive type) */
    def makePrimitiveUnbox(expr: js.Tree, tpe: Type)(
        implicit pos: Position): js.Tree = {
      toIRType(tpe) match {
        case jstpe.NoType => expr // for JS interop cases
        case irTpe        => js.AsInstanceOf(expr, irTpe)
      }
    }

    /** Gen JS code for a Scala.js-specific primitive method */
    private def genJSPrimitive(tree: Apply, args: List[Tree], code: Int,
        isStat: Boolean): js.Tree = {
      import jsPrimitives._

      implicit val pos = tree.pos

      def genArgs1: js.Tree = {
        assert(args.size == 1,
            s"Expected exactly 1 argument for JS primitive $code but got " +
            s"${args.size} at $pos")
        genExpr(args.head)
      }

      def genArgs2: (js.Tree, js.Tree) = {
        assert(args.size == 2,
            s"Expected exactly 2 arguments for JS primitive $code but got " +
            s"${args.size} at $pos")
        (genExpr(args.head), genExpr(args.tail.head))
      }

      def genArgsVarLength: List[js.TreeOrJSSpread] =
        genPrimitiveJSArgs(tree.symbol, args)

      def resolveReifiedJSClassSym(arg: Tree): Symbol = {
        def fail(): Symbol = {
          reporter.error(pos,
              tree.symbol.nameString + " must be called with a constant " +
              "classOf[T] representing a class extending js.Any " +
              "(not a trait nor an object)")
          NoSymbol
        }
        arg match {
          case Literal(value) if value.tag == ClazzTag =>
            val classSym = value.typeValue.typeSymbol
            if (isJSType(classSym) && !classSym.isTrait && !classSym.isModuleClass)
              classSym
            else
              fail()
          case _ =>
            fail()
        }
      }

      (code: @switch) match {
        case DYNNEW =>
          // js.Dynamic.newInstance(clazz)(actualArgs: _*)
          val (jsClass, actualArgs) = extractFirstArg(genArgsVarLength)
          js.JSNew(jsClass, actualArgs)

        case ARR_CREATE =>
          // js.Array(elements: _*)
          js.JSArrayConstr(genArgsVarLength)

        case CONSTRUCTOROF =>
          // runtime.constructorOf(clazz)
          val classSym = resolveReifiedJSClassSym(args.head)
          if (classSym == NoSymbol)
            js.Undefined() // compile error emitted by resolveReifiedJSClassSym
          else
            genPrimitiveJSClass(classSym)

        case CREATE_INNER_JS_CLASS | CREATE_LOCAL_JS_CLASS =>
          // runtime.createInnerJSClass(clazz, superClass)
          // runtime.createLocalJSClass(clazz, superClass, fakeNewInstances)
          val classSym = resolveReifiedJSClassSym(args(0))
          val superClassValue = genExpr(args(1))
          if (classSym == NoSymbol) {
            js.Undefined() // compile error emitted by resolveReifiedJSClassSym
          } else {
            val captureValues = {
              if (code == CREATE_INNER_JS_CLASS) {
                val outer = genThis()
                List.fill(classSym.info.decls.count(_.isClassConstructor))(outer)
              } else {
                val ArrayValue(_, fakeNewInstances) = args(2)
                fakeNewInstances.flatMap(genCaptureValuesFromFakeNewInstance(_))
              }
            }
            js.CreateJSClass(encodeClassName(classSym),
                superClassValue :: captureValues)
          }

        case WITH_CONTEXTUAL_JS_CLASS_VALUE =>
          // withContextualJSClassValue(jsclass, inner)
          val jsClassValue = genExpr(args(0))
          withScopedVars(
              contextualJSClassValue := Some(jsClassValue)
          ) {
            genStatOrExpr(args(1), isStat)
          }

        case LINKING_INFO =>
          // runtime.linkingInfo
          js.JSLinkingInfo()

        case IDENTITY_HASH_CODE =>
          // runtime.identityHashCode(arg)
          val arg = genArgs1
          js.IdentityHashCode(arg)

        case DEBUGGER =>
          // js.special.debugger()
          js.Debugger()

        case UNITVAL =>
          // BoxedUnit.UNIT, which is the boxed version of ()
          js.Undefined()

        case JS_NATIVE =>
          // js.native
          reporter.error(pos,
              "js.native may only be used as stub implementation in facade types")
          js.Undefined()

        case TYPEOF =>
          // js.typeOf(arg)
          val arg = genArgs1
          val typeofExpr = arg match {
            case arg: js.JSGlobalRef => js.JSTypeOfGlobalRef(arg)
            case _                   => js.JSUnaryOp(js.JSUnaryOp.typeof, arg)
          }
          genAsInstanceOf(typeofExpr, StringClass.tpe)

        case JS_NEW_TARGET =>
          // js.new.target
          val valid = currentMethodSym.isClassConstructor && isNonNativeJSClass(currentClassSym)
          if (!valid) {
            reporter.error(pos,
                "Illegal use of js.`new`.target.\n" +
                "It can only be used in the constructor of a JS class, " +
                "as a statement or in the rhs of a val or var.\n" +
                "It cannot be used inside a lambda or by-name parameter, nor in any other location.")
          }
          js.JSNewTarget()

        case JS_IMPORT =>
          // js.import(arg)
          val arg = genArgs1
          js.JSImportCall(arg)

        case JS_IMPORT_META =>
          // js.import.meta
          js.JSImportMeta()

        case DYNAMIC_IMPORT =>
          assert(args.size == 1,
              s"Expected exactly 1 argument for JS primitive $code but got " +
              s"${args.size} at $pos")

          args.head match {
            case Block(stats, expr @ Apply(fun @ Select(New(tpt), _), args)) =>
              /* stats is always empty if no other compiler plugin is present.
               * However, code instrumentation (notably scoverage) might add
               * statements here. If this is the case, the thunk anonymous class
               * has already been created when the other plugin runs (i.e. the
               * plugin ran after jsinterop).
               *
               * Therefore, it is OK to leave the statements on our side of the
               * dynamic loading boundary.
               */

              val clsSym = tpt.symbol
              val ctor = fun.symbol

              assert(clsSym.isSubClass(DynamicImportThunkClass),
                  s"expected subclass of DynamicImportThunk, got: $clsSym at: ${expr.pos}")
              assert(ctor.isPrimaryConstructor,
                  s"expected primary constructor, got: $ctor at: ${expr.pos}")

              js.Block(
                  stats.map(genStat(_)),
                  js.ApplyDynamicImport(
                      js.ApplyFlags.empty,
                      encodeClassName(clsSym),
                      encodeDynamicImportForwarderIdent(ctor.tpe.params),
                      genActualArgs(ctor, args))
              )

            case tree =>
              abort("Unexpected argument tree in dynamicImport: " +
                  tree + "/" + tree.getClass + " at: " + tree.pos)
          }

        case STRICT_EQ =>
          // js.special.strictEquals(arg1, arg2)
          val (arg1, arg2) = genArgs2
          js.JSBinaryOp(js.JSBinaryOp.===, arg1, arg2)

        case IN =>
          // js.special.in(arg1, arg2)
          val (arg1, arg2) = genArgs2
          js.AsInstanceOf(js.JSBinaryOp(js.JSBinaryOp.in, arg1, arg2),
              jstpe.BooleanType)

        case INSTANCEOF =>
          // js.special.instanceof(arg1, arg2)
          val (arg1, arg2) = genArgs2
          js.AsInstanceOf(js.JSBinaryOp(js.JSBinaryOp.instanceof, arg1, arg2),
              jstpe.BooleanType)

        case DELETE =>
          // js.special.delete(arg1, arg2)
          val (arg1, arg2) = genArgs2
          js.JSDelete(arg1, arg2)

        case FORIN =>
          /* js.special.forin(arg1, arg2)
           *
           * We must generate:
           *
           * val obj = arg1
           * val f = arg2
           * for (val key in obj) {
           *   f(key)
           * }
           *
           * with temporary vals, because `arg2` must be evaluated only
           * once, and after `arg1`.
           */
          val (arg1, arg2) = genArgs2
          val objVarDef = js.VarDef(freshLocalIdent("obj"), NoOriginalName,
              jstpe.AnyType, mutable = false, arg1)
          val fVarDef = js.VarDef(freshLocalIdent("f"), NoOriginalName,
              jstpe.AnyType, mutable = false, arg2)
          val keyVarIdent = freshLocalIdent("key")
          val keyVarRef = js.VarRef(keyVarIdent)(jstpe.AnyType)
          js.Block(
              objVarDef,
              fVarDef,
              js.ForIn(objVarDef.ref, keyVarIdent, NoOriginalName, {
                js.JSFunctionApply(fVarDef.ref, List(keyVarRef))
              }))

        case JS_THROW =>
          // js.special.throw(arg)
          js.Throw(genArgs1)

        case JS_TRY_CATCH =>
          /* js.special.tryCatch(arg1, arg2)
           *
           * We must generate:
           *
           * val body = arg1
           * val handler = arg2
           * try {
           *   body()
           * } catch (e) {
           *   handler(e)
           * }
           *
           * with temporary vals, because `arg2` must be evaluated before
           * `body` executes. Moreover, exceptions thrown while evaluating
           * the function values `arg1` and `arg2` must not be caught.
           */
          val (arg1, arg2) = genArgs2
          val bodyVarDef = js.VarDef(freshLocalIdent("body"), NoOriginalName,
              jstpe.AnyType, mutable = false, arg1)
          val handlerVarDef = js.VarDef(freshLocalIdent("handler"), NoOriginalName,
              jstpe.AnyType, mutable = false, arg2)
          val exceptionVarIdent = freshLocalIdent("e")
          val exceptionVarRef = js.VarRef(exceptionVarIdent)(jstpe.AnyType)
          js.Block(
            bodyVarDef,
            handlerVarDef,
            js.TryCatch(
              js.JSFunctionApply(bodyVarDef.ref, Nil),
              exceptionVarIdent,
              NoOriginalName,
              js.JSFunctionApply(handlerVarDef.ref, List(exceptionVarRef))
            )(jstpe.AnyType)
          )

        case WRAP_AS_THROWABLE =>
          // js.special.wrapAsThrowable(arg)
          js.WrapAsThrowable(genArgs1)

        case UNWRAP_FROM_THROWABLE =>
          // js.special.unwrapFromThrowable(arg)
          js.UnwrapFromThrowable(genArgs1)

        case LINKTIME_IF =>
          // linkingInfo.linkTimeIf(cond, thenp, elsep)
          assert(args.size == 3,
              s"Expected exactly 3 arguments for JS primitive $code but got " +
              s"${args.size} at $pos")
          val condp = genLinkTimeTree(args(0))
          val thenp = genExpr(args(1))
          val elsep = genExpr(args(2))
          js.LinkTimeIf(condp, thenp, elsep)(toIRType(tree.tpe))
      }
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
      val sym = tree.symbol
      val Apply(fun @ Select(receiver0, _), args0) = tree

      implicit val pos = tree.pos

      val receiver = genExprOrGlobalScope(receiver0)
      val args = genPrimitiveJSArgs(sym, args0)

      genJSCallGeneric(sym, receiver, args, isStat)
    }

    /** Gen JS code for a call to a native JS def or val. */
    private def genJSNativeMemberCall(tree: Apply, isStat: Boolean): js.Tree = {
      val sym = tree.symbol
      val Apply(_, args0) = tree

      implicit val pos = tree.pos

      val jsNativeMemberValue =
        js.SelectJSNativeMember(encodeClassName(sym.owner), encodeMethodSym(sym))

      val boxedResult =
        if (jsInterop.isJSGetter(sym)) jsNativeMemberValue
        else js.JSFunctionApply(jsNativeMemberValue, genPrimitiveJSArgs(sym, args0))

      fromAny(boxedResult, enteringPhase(currentRun.posterasurePhase) {
        sym.tpe.resultType
      })
    }

    private def genJSSuperCall(tree: Apply, isStat: Boolean): js.Tree = {
      acquireContextualJSClassValue { explicitJSSuperClassValue =>
        implicit val pos = tree.pos
        val Apply(fun @ Select(sup @ Super(qual, _), _), args) = tree
        val sym = fun.symbol

        /* #3013 `qual` can be `this.$outer()` in some cases since Scala 2.12,
         * so we call `genExpr(qual)`, not just `genThis()`.
         */
        val genReceiver = genExpr(qual)
        lazy val genScalaArgs = genActualArgs(sym, args)
        lazy val genJSArgs = genPrimitiveJSArgs(sym, args)

        if (sym.owner == ObjectClass) {
          // Normal call anyway
          assert(!sym.isClassConstructor,
              "Trying to call the super constructor of Object in a " +
              s"non-native JS class at $pos")
          genApplyMethod(genReceiver, sym, genScalaArgs)
        } else if (sym.isClassConstructor) {
          throw new AssertionError("calling a JS super constructor should " +
              s"have happened in genPrimaryJSClassCtor at $pos")
        } else if (isNonNativeJSClass(sym.owner) && !isExposed(sym)) {
          // Reroute to the static method
          genApplyJSClassMethod(genReceiver, sym, genScalaArgs)
        } else {
          val jsSuperClassValue = explicitJSSuperClassValue.orElse {
            Some(genPrimitiveJSClass(currentClassSym.superClass))
          }
          genJSCallGeneric(sym, MaybeGlobalScope.NotGlobalScope(genReceiver),
              genJSArgs, isStat, jsSuperClassValue)
        }
      }
    }

    private def genJSCallGeneric(sym: Symbol, receiver: MaybeGlobalScope,
        args: List[js.TreeOrJSSpread], isStat: Boolean,
        jsSuperClassValue: Option[js.Tree] = None)(
        implicit pos: Position): js.Tree = {

      def argsNoSpread: List[js.Tree] = {
        assert(!args.exists(_.isInstanceOf[js.JSSpread]),
            s"Unexpected spread at $pos")
        args.asInstanceOf[List[js.Tree]]
      }

      val argc = args.size // meaningful only for methods that don't have varargs

      def requireNotSuper(): Unit = {
        if (jsSuperClassValue.isDefined) {
          reporter.error(pos,
              "Illegal super call in non-native JS class")
        }
      }

      def genSuperReference(propName: js.Tree): js.AssignLhs = {
        jsSuperClassValue.fold[js.AssignLhs] {
          genJSBracketSelectOrGlobalRef(receiver, propName)
        } { superClassValue =>
          js.JSSuperSelect(superClassValue,
              ruleOutGlobalScope(receiver), propName)
        }
      }

      def genSelectGet(propName: js.Tree): js.Tree =
        genSuperReference(propName)

      def genSelectSet(propName: js.Tree, value: js.Tree): js.Tree =
        js.Assign(genSuperReference(propName), value)

      def genCall(methodName: js.Tree,
          args: List[js.TreeOrJSSpread]): js.Tree = {
        jsSuperClassValue.fold[js.Tree] {
          genJSBracketMethodApplyOrGlobalRefApply(
              receiver, methodName, args)
        } { superClassValue =>
          js.JSSuperMethodCall(superClassValue,
              ruleOutGlobalScope(receiver), methodName, args)
        }
      }

      val boxedResult = JSCallingConvention.of(sym) match {
        case JSCallingConvention.UnaryOp(code) =>
          requireNotSuper()
          assert(argc == 0, s"bad argument count ($argc) for unary op at $pos")
          js.JSUnaryOp(code, ruleOutGlobalScope(receiver))

        case JSCallingConvention.BinaryOp(code) =>
          requireNotSuper()
          assert(argc == 1, s"bad argument count ($argc) for binary op at $pos")
          js.JSBinaryOp(code, ruleOutGlobalScope(receiver), argsNoSpread.head)

        case JSCallingConvention.Call =>
          requireNotSuper()

          if (sym.owner.isSubClass(JSThisFunctionClass)) {
            genJSBracketMethodApplyOrGlobalRefApply(receiver,
                js.StringLiteral("call"), args)
          } else {
            js.JSFunctionApply(ruleOutGlobalScope(receiver), args)
          }

        case JSCallingConvention.Property(jsName) =>
          argsNoSpread match {
            case Nil          => genSelectGet(genExpr(jsName))
            case value :: Nil => genSelectSet(genExpr(jsName), value)

            case _ =>
              throw new AssertionError(
                  s"property methods should have 0 or 1 non-varargs arguments at $pos")
          }

        case JSCallingConvention.BracketAccess =>
          argsNoSpread match {
            case keyArg :: Nil =>
              genSelectGet(keyArg)
            case keyArg :: valueArg :: Nil =>
              genSelectSet(keyArg, valueArg)
            case _ =>
              throw new AssertionError(
                  s"@JSBracketAccess methods should have 1 or 2 non-varargs arguments at $pos")
          }

        case JSCallingConvention.BracketCall =>
          val (methodName, actualArgs) = extractFirstArg(args)
          genCall(methodName, actualArgs)

        case JSCallingConvention.Method(jsName) =>
          genCall(genExpr(jsName), args)
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

    /** Extract the first argument to a primitive JS call.
     *  This is nothing else than decomposing into head and tail, except that
     *  we assert that the first element is not a JSSpread.
     */
    private def extractFirstArg(
        args: List[js.TreeOrJSSpread]): (js.Tree, List[js.TreeOrJSSpread]) = {
      assert(args.nonEmpty,
          "Trying to extract the first argument of an empty argument list")
      val firstArg = args.head match {
        case firstArg: js.Tree =>
          firstArg
        case firstArg: js.JSSpread =>
          throw new AssertionError(
              "Trying to extract the first argument of an argument list starting " +
              "with a Spread argument: " + firstArg)
      }
      (firstArg, args.tail)
    }

    /** Gen JS code for a new of a JS class (subclass of js.Any) */
    private def genPrimitiveJSNew(tree: Apply): js.Tree = {
      acquireContextualJSClassValue { jsClassValue =>
        implicit val pos = tree.pos

        val Apply(fun @ Select(New(tpt), _), args0) = tree
        val cls = tpt.tpe.typeSymbol
        val ctor = fun.symbol

        val nestedJSClass = isNestedJSClass(cls)
        assert(jsClassValue.isDefined == nestedJSClass,
            s"$cls at $pos: jsClassValue.isDefined = ${jsClassValue.isDefined} " +
            s"but isInnerNonNativeJSClass = $nestedJSClass")

        def args = genPrimitiveJSArgs(ctor, args0)

        if (cls == JSObjectClass && args0.isEmpty)
          js.JSObjectConstr(Nil)
        else if (cls == JSArrayClass && args0.isEmpty)
          js.JSArrayConstr(Nil)
        else if (isAnonymousJSClass(cls))
          genAnonJSClassNew(cls, jsClassValue.get, args0.map(genExpr))(fun.pos)
        else if (!nestedJSClass)
          js.JSNew(genPrimitiveJSClass(cls), args)
        else if (!cls.isModuleClass)
          js.JSNew(jsClassValue.get, args)
        else
          genCreateInnerJSModule(cls, jsClassValue.get, args0.map(genExpr))
      }
    }

    /** Gen JS code representing a JS class (subclass of js.Any) */
    private def genPrimitiveJSClass(sym: Symbol)(
        implicit pos: Position): js.Tree = {
      assert(!isStaticModule(sym) && !sym.isTraitOrInterface,
          s"genPrimitiveJSClass called with non-class $sym")
      js.LoadJSConstructor(encodeClassName(sym))
    }

    /** Gen JS code to create the JS class of an inner JS module class. */
    private def genCreateInnerJSModule(sym: Symbol,
        jsSuperClassValue: js.Tree, args: List[js.Tree])(
        implicit pos: Position): js.Tree = {
      js.JSNew(js.CreateJSClass(encodeClassName(sym),
          jsSuperClassValue :: args), Nil)
    }

    /** Gen actual actual arguments to Scala method call.
     *
     *  Returns a list of the transformed arguments.
     *
     *  This tries to optimize repeated arguments (varargs) by turning them
     *  into JS arrays wrapped in the appropriate Seq, rather than Scala
     *  arrays.
     */
    private def genActualArgs(sym: Symbol, args: List[Tree])(
        implicit pos: Position): List[js.Tree] = {
      val wereRepeated = exitingPhase(currentRun.typerPhase) {
        /* Do NOT use `params` instead of `paramss.flatten` here! Exiting
         * typer, `params` only contains the *first* parameter list.
         * This was causing #2265 and #2741.
         */
        sym.tpe.paramss.flatten.map(p => isScalaRepeatedParamType(p.tpe))
      }

      if (wereRepeated.size > args.size) {
        // Should not happen, but let's not crash
        args.map(genExpr)
      } else {
        /* Arguments that are in excess compared to the type signature after
         * typer are lambda-lifted arguments. They cannot be repeated, hence
         * the extension to `false`.
         */
        for ((arg, wasRepeated) <- args.zipAll(wereRepeated, EmptyTree, false)) yield {
          if (wasRepeated) {
            tryGenRepeatedParamAsJSArray(arg, handleNil = false).fold {
              genExpr(arg)
            } { genArgs =>
              genJSArrayToVarArgs(js.JSArrayConstr(genArgs))
            }
          } else {
            genExpr(arg)
          }
        }
      }
    }

    /** Info about a Scala method param when called as JS method.
     *
     *  @param sym Parameter symbol as seen now.
     *  @param tpe Parameter type (type of a single element if repeated)
     *  @param repeated Whether the parameter is repeated.
     *  @param capture Whether the parameter is a capture.
     */
    final class JSParamInfo(val sym: Symbol, val tpe: Type,
        val repeated: Boolean = false, val capture: Boolean = false) {
      assert(!repeated || !capture, "capture cannot be repeated")
      def hasDefault: Boolean = sym.hasFlag(Flags.DEFAULTPARAM)
    }

    def jsParamInfos(sym: Symbol): List[JSParamInfo] = {
      assert(sym.isMethod, s"trying to take JS param info of non-method: $sym")

      /* For constructors of nested JS classes (*), explicitouter and
       * lambdalift have introduced some parameters for the outer parameter and
       * captures. We must ignore those, as captures and outer pointers are
       * taken care of by `explicitinerjs` for such classes.
       *
       * Unfortunately, for some reason lambdalift creates new symbol *even for
       * parameters originally in the signature* when doing so! That is why we
       * use the *names* of the parameters as a link through time, rather than
       * the symbols, to identify which ones already existed at the time of
       * explicitinerjs.
       *
       * This is pretty fragile, but fortunately we have a huge test suite to
       * back us up should scalac alter its behavior.
       *
       * In addition, for actual parameters that we keep, we have to look back
       * in time to see whether they were repeated and what was their type.
       *
       * (*) Note that we are not supposed to receive in genPrimitiveJSArgs a
       *     method symbol that would have such captures *and* would not be a
       *     class constructors. Indeed, such methods would have started their
       *     life as local defs, which are not exposed.
       */

      val uncurryParams = enteringPhase(currentRun.uncurryPhase) {
        for {
          paramUncurry <- sym.tpe.paramss.flatten
        } yield {
          val v = {
            if (isRepeated(paramUncurry))
              Some(repeatedToSingle(paramUncurry.tpe))
            else
              None
          }

          paramUncurry.name -> v
        }
      }.toMap

      val paramTpes = enteringPhase(currentRun.posterasurePhase) {
        for (param <- sym.tpe.params)
          yield param.name -> param.tpe
      }.toMap

      for {
        paramSym <- sym.tpe.params
      } yield {
        uncurryParams.get(paramSym.name) match {
          case None =>
            // This is a capture parameter introduced by explicitouter or lambdalift
            new JSParamInfo(paramSym, paramSym.tpe, capture = true)

          case Some(Some(tpe)) =>
            new JSParamInfo(paramSym, tpe, repeated = true)

          case Some(None) =>
            val tpe = paramTpes.getOrElse(paramSym.name, paramSym.tpe)
            new JSParamInfo(paramSym, tpe)
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
        implicit pos: Position): List[js.TreeOrJSSpread] = {

      var reversedArgs: List[js.TreeOrJSSpread] = Nil

      for ((arg, info) <- args.zip(jsParamInfos(sym))) {
        if (info.repeated) {
          reversedArgs =
            genPrimitiveJSRepeatedParam(arg) reverse_::: reversedArgs
        } else if (info.capture) {
          // Ignore captures
          assert(sym.isClassConstructor,
              s"Found an unknown param ${info.sym.name} in method " +
              s"${sym.fullName}, which is not a class constructor, at $pos")
        } else {
          val unboxedArg = genExpr(arg)
          val boxedArg = unboxedArg match {
            case js.Transient(UndefinedParam) =>
              unboxedArg
            case _ =>
              ensureBoxed(unboxedArg, info.tpe)
          }

          reversedArgs ::= boxedArg
        }
      }

      /* Remove all consecutive UndefinedParam's at the end of the argument
       * list. No check is performed whether they may be there, since they will
       * only be placed where default arguments can be anyway.
       */
      reversedArgs = reversedArgs.dropWhile {
        case js.Transient(UndefinedParam) => true
        case _                            => false
      }

      // Find remaining UndefinedParam's and replace by js.Undefined. This can
      // happen with named arguments or when multiple argument lists are present
      reversedArgs = reversedArgs map {
        case js.Transient(UndefinedParam) => js.Undefined()
        case arg                          => arg
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
    private def genPrimitiveJSRepeatedParam(arg: Tree): List[js.TreeOrJSSpread] = {
      tryGenRepeatedParamAsJSArray(arg, handleNil = true) getOrElse {
        /* Fall back to calling runtime.toJSVarArgs to perform the conversion
         * to js.Array, then wrap in a Spread operator.
         */
        implicit val pos = arg.pos
        val jsArrayArg = genApplyMethod(
            genLoadModule(RuntimePackageModule),
            Runtime_toJSVarArgs,
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
      private val wrapArrayModule =
        if (hasNewCollections) ScalaRunTimeModule
        else PredefModule

      val wrapRefArrayMethod: Symbol =
        getMemberMethod(wrapArrayModule, nme.wrapRefArray)

      val genericWrapArrayMethod: Symbol =
        getMemberMethod(wrapArrayModule, nme.genericWrapArray)

      def isClassTagBasedWrapArrayMethod(sym: Symbol): Boolean =
        sym == wrapRefArrayMethod || sym == genericWrapArrayMethod

      private val isWrapArray: Set[Symbol] = {
        Seq(
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
            nme.genericWrapArray
        ).map(getMemberMethod(wrapArrayModule, _)).toSet
      }

      def unapply(tree: Apply): Option[Tree] = tree match {
        case Apply(wrapArray_?, List(wrapped))
        if isWrapArray(wrapArray_?.symbol) =>
          Some(wrapped)
        case _ =>
          None
      }
    }

    /** Wraps a `js.Array` to use as varargs. */
    def genJSArrayToVarArgs(arrayRef: js.Tree)(
        implicit pos: Position): js.Tree = {
      genApplyMethod(genLoadModule(RuntimePackageModule),
          Runtime_toScalaVarArgs, List(arrayRef))
    }

    /** Gen the actual capture values for a JS constructor based on its fake
     *  `new` invocation.
     */
    private def genCaptureValuesFromFakeNewInstance(
        tree: Tree): List[js.Tree] = {

      implicit val pos = tree.pos

      val Apply(fun @ Select(New(_), _), args) = tree
      val sym = fun.symbol

      /* We use the same strategy as genPrimitiveJSArgs to detect which
       * parameters were introduced by explicitouter or lambdalift (but
       * reversed, of course).
       */

      val existedBeforeUncurry = enteringPhase(currentRun.uncurryPhase) {
        for {
          params <- sym.tpe.paramss
          param <- params
        } yield {
          param.name
        }
      }.toSet

      for {
        (arg, paramSym) <- args.zip(sym.tpe.params)
        if !existedBeforeUncurry(paramSym.name)
      } yield {
        genExpr(arg)
      }
    }

    // Synthesizers for JS functions -------------------------------------------

    /** Try and generate JS code for an anonymous function class.
     *
     *  Returns Some(<js code>) if the class could be rewritten that way, None
     *  otherwise.
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
     *  we generate a function:
     *
     *    lambda<o, c1, ..., cM>[notype](
     *        outer, capture1, ..., captureM, param1, ..., paramN) {
     *      <body>
     *    }
     *
     *  so that, at instantiation point, we can write:
     *
     *    new AnonFunctionN(function)
     *
     *  the latter tree is returned in case of success.
     *
     *  Trickier things apply when the function is specialized.
     */
    private def tryGenAnonFunctionClass(cd: ClassDef,
        capturedArgs: List[js.Tree]): Option[js.Tree] = {
      // scalastyle:off return
      implicit val pos = cd.pos
      val sym = cd.symbol
      assert(sym.isAnonymousFunction,
          s"tryGenAndRecordAnonFunctionClass called with non-anonymous function $cd")

      if (!sym.superClass.fullName.startsWith("scala.runtime.AbstractFunction")) {
        /* This is an anonymous class for a non-LMF capable SAM in 2.12.
         * We must not rewrite it, as it would then not inherit from the
         * appropriate parent class and/or interface.
         */
        None
      } else {
        nestedGenerateClass(sym) {
          val (functionBase, arity) =
            tryGenAnonFunctionClassGeneric(cd, capturedArgs)(_ => return None)

          Some(genJSFunctionToScala(functionBase, arity))
        }
      }
      // scalastyle:on return
    }

    /** Gen a conversion from a JavaScript function into a Scala function. */
    private def genJSFunctionToScala(jsFunction: js.Tree, arity: Int)(
        implicit pos: Position): js.Tree = {
      val clsSym = getRequiredClass("scala.scalajs.runtime.AnonFunction" + arity)
      val ctor = clsSym.primaryConstructor
      genNew(clsSym, ctor, List(jsFunction))
    }

    /** Gen JS code for a JS function class.
     *
     *  This is called when emitting a ClassDef that represents an anonymous
     *  class extending `js.FunctionN`. These are generated by the SAM
     *  synthesizer when the target type is a `js.FunctionN`. Since JS
     *  functions are not classes, we deconstruct the ClassDef, then
     *  reconstruct it to be a genuine Closure.
     *
     *  Compared to `tryGenAnonFunctionClass()`, this function must
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
     *  we generate a function:
     *
     *    lambda<o, c1, ..., cM>[notype](
     *        outer, capture1, ..., captureM, param1, ..., paramN) {
     *      outer.lambdaImpl(param1, ..., paramN, capture1, ..., captureM)
     *    }
     */
    def genJSFunction(cd: ClassDef, captures: List[js.Tree]): js.Tree = {
      val sym = cd.symbol
      assert(isJSFunctionDef(sym),
          s"genAndRecordJSFunctionClass called with non-JS function $cd")

      nestedGenerateClass(sym) {
        val (function, _) = tryGenAnonFunctionClassGeneric(cd, captures)(msg =>
            abort(s"Could not generate function for JS function: $msg"))

        function
      }
    }

    /** Code common to tryGenAndRecordAnonFunctionClass and
     *  genAndRecordJSFunctionClass.
     */
    private def tryGenAnonFunctionClassGeneric(cd: ClassDef,
        initialCapturedArgs: List[js.Tree])(
        fail: (=> String) => Nothing): (js.Tree, Int) = {
      implicit val pos = cd.pos
      val sym = cd.symbol

      // First checks

      if (sym.isSubClass(PartialFunctionClass))
        fail(s"Cannot rewrite PartialFunction $cd")

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
              } else if (ddsym.hasAnnotation(JSOptionalAnnotation)) {
                // Ignore (this is useful for default parameters in custom JS function types)
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
        val ctorParamDefs = usedCtorParams.map(genParamDef(_))

        // Third step: emit the body of the apply method def

        val applyMethod = withScopedVars(
            paramAccessorLocals := (paramAccessors zip ctorParamDefs).toMap,
            tryingToGenMethodAsJSFunction := true
        ) {
          try {
            genMethodWithCurrentLocalNameScope(applyDef)
          } catch {
            case e: CancelGenMethodAsJSFunction =>
              fail(e.getMessage)
          }
        }

        // Fourth step: patch the body to unbox parameters and box result

        val hasRepeatedParam = {
          sym.superClass == JSFunctionClass && // Scala functions are known not to have repeated params
          enteringUncurry {
            applyDef.symbol.paramss.flatten.lastOption.exists(isRepeated(_))
          }
        }

        val js.MethodDef(_, _, _, params, _, body) = applyMethod
        val (patchedParams, paramsLocals) = {
          val nonRepeatedParams =
            if (hasRepeatedParam) params.init
            else params
          patchFunParamsWithBoxes(applyDef.symbol, nonRepeatedParams,
              useParamsBeforeLambdaLift = false)
        }

        val (patchedRepeatedParam, repeatedParamLocal) = {
          /* Instead of this circus, simply `unzip` would be nice.
           * But that lowers the type to iterable.
           */
          if (hasRepeatedParam) {
            val (p, l) = genPatchedParam(params.last, genJSArrayToVarArgs(_))
            (Some(p), Some(l))
          } else {
            (None, None)
          }
        }

        val patchedBody =
          js.Block(paramsLocals ++ repeatedParamLocal :+ ensureResultBoxed(body.get, applyDef.symbol))

        // Fifth step: build the js.Closure

        val isThisFunction = sym.isSubClass(JSThisFunctionClass) && {
          val ok = patchedParams.nonEmpty
          if (!ok) {
            reporter.error(pos,
                "The SAM or apply method for a js.ThisFunction must have a " +
                "leading non-varargs parameter")
          }
          ok
        }

        val capturedArgs =
          if (hasUnusedOuterCtorParam) initialCapturedArgs.tail
          else initialCapturedArgs
        assert(capturedArgs.size == ctorParamDefs.size,
            s"$capturedArgs does not match $ctorParamDefs")

        val closure = {
          if (isThisFunction) {
            val thisParam :: actualParams = patchedParams
            js.Closure(
                arrow = false,
                ctorParamDefs,
                actualParams,
                patchedRepeatedParam,
                js.Block(
                    js.VarDef(thisParam.name, thisParam.originalName,
                        thisParam.ptpe, mutable = false,
                        js.This()(thisParam.ptpe)(thisParam.pos))(thisParam.pos),
                    patchedBody),
                capturedArgs)
          } else {
            js.Closure(arrow = true, ctorParamDefs, patchedParams,
                patchedRepeatedParam, patchedBody, capturedArgs)
          }
        }

        val arity = params.size

        (closure, arity)
      }
    }

    /** Generate JS code for an anonymous function
     *
     *  Anonymous functions survive until the backend for any
     *  LambdaMetaFactory-capable type.
     *
     *  When they do, their body is always of the form
     *  {{{
     *  EnclosingClass.this.someMethod(args)
     *  }}}
     *  where the args are either formal parameters of the lambda, or local
     *  variables or the enclosing def. The latter must be captured.
     *
     *  We identify the captures using the same method as the `delambdafy`
     *  phase. We have an additional hack for `this`.
     *
     *  To translate them, we first construct a JS closure for the body:
     *  {{{
     *  lambda<this, capture1, ..., captureM>(
     *      _this, capture1, ..., captureM, arg1, ..., argN) {
     *    _this.someMethod(arg1, ..., argN, capture1, ..., captureM)
     *  }
     *  }}}
     *  In the closure, input params are unboxed before use, and the result of
     *  `someMethod()` is boxed back.
     *
     *  Then, we wrap that closure in a class satisfying the expected type.
     *  For Scala function types, we use the existing
     *  `scala.scalajs.runtime.AnonFunctionN` from the library. For other
     *  LMF-capable types, we generate a class on the fly, which looks like
     *  this:
     *  {{{
     *  class AnonFun extends Object with FunctionalInterface {
     *    val f: any
     *    def <init>(f: any) {
     *      super();
     *      this.f = f
     *    }
     *    def theSAMMethod(params: Types...): Type =
     *      unbox((this.f)(boxParams...))
     *  }
     *  }}}
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

      val formalCaptures = captureSyms.toList.map(genParamDef(_, pos))
      val actualCaptures = formalCaptures.map(_.ref)

      val formalArgs = params.map(genParamDef(_))

      val (allFormalCaptures, body, allActualCaptures) = if (!target.isStaticMember) {
        val thisActualCapture = genExpr(receiver)
        val thisFormalCapture = js.ParamDef(
            freshLocalIdent("this")(receiver.pos), thisOriginalName,
            thisActualCapture.tpe, mutable = false)(receiver.pos)
        val thisCaptureArg = thisFormalCapture.ref

        val body = if (isJSType(receiver.tpe) && target.owner != ObjectClass) {
          assert(isNonNativeJSClass(target.owner) && !isExposed(target),
              s"A Function lambda is trying to call an exposed JS method ${target.fullName}")
          genApplyJSClassMethod(thisCaptureArg, target, allArgs)
        } else {
          genApplyMethodMaybeStatically(thisCaptureArg, target, allArgs)
        }

        (thisFormalCapture :: formalCaptures,
            body, thisActualCapture :: actualCaptures)
      } else {
        val body = genApplyStatic(target, allArgs)

        (formalCaptures, body, actualCaptures)
      }

      val (patchedFormalArgs, paramsLocals) =
        patchFunParamsWithBoxes(target, formalArgs, useParamsBeforeLambdaLift = true)

      val patchedBody =
        js.Block(paramsLocals :+ ensureResultBoxed(body, target))

      val closure = js.Closure(
          arrow = true,
          allFormalCaptures,
          patchedFormalArgs,
          restParam = None,
          patchedBody,
          allActualCaptures)

      // Wrap the closure in the appropriate box for the SAM type
      val funSym = originalFunction.tpe.typeSymbolDirect
      if (isFunctionSymbol(funSym)) {
        /* This is a scala.FunctionN. We use the existing AnonFunctionN
         * wrapper.
         */
        genJSFunctionToScala(closure, params.size)
      } else {
        /* This is an arbitrary SAM type (can only happen in 2.12).
         * We have to synthesize a class like LambdaMetaFactory would do on
         * the JVM.
         */
        val sam = originalFunction.attachments.get[SAMFunction].getOrElse {
          abort(s"Cannot find the SAMFunction attachment on $originalFunction at $pos")
        }

        val samWrapperClassName = synthesizeSAMWrapper(funSym, sam)
        js.New(samWrapperClassName, js.MethodIdent(ObjectArgConstructorName),
            List(closure))
      }
    }

    private def synthesizeSAMWrapper(funSym: Symbol, samInfo: SAMFunction)(
        implicit pos: Position): ClassName = {
      val intfName = encodeClassName(funSym)

      val suffix = {
        generatedSAMWrapperCount.value += 1
        // LambdaMetaFactory names classes like this
        "$$Lambda$" + generatedSAMWrapperCount.value
      }
      val className = encodeClassName(currentClassSym).withSuffix(suffix)

      val classType = jstpe.ClassType(className)

      // val f: Any
      val fFieldIdent = js.FieldIdent(FieldName(className, SimpleFieldName("f")))
      val fFieldDef = js.FieldDef(js.MemberFlags.empty, fFieldIdent,
          NoOriginalName, jstpe.AnyType)

      // def this(f: Any) = { this.f = f; super() }
      val ctorDef = {
        val fParamDef = js.ParamDef(js.LocalIdent(LocalName("f")),
            NoOriginalName, jstpe.AnyType, mutable = false)
        js.MethodDef(
            js.MemberFlags.empty.withNamespace(js.MemberNamespace.Constructor),
            js.MethodIdent(ObjectArgConstructorName),
            NoOriginalName,
            List(fParamDef),
            jstpe.NoType,
            Some(js.Block(List(
                js.Assign(
                    js.Select(js.This()(classType), fFieldIdent)(jstpe.AnyType),
                    fParamDef.ref),
                js.ApplyStatically(js.ApplyFlags.empty.withConstructor(true),
                    js.This()(classType),
                    ir.Names.ObjectClass,
                    js.MethodIdent(ir.Names.NoArgConstructorName),
                    Nil)(jstpe.NoType)))))(
            js.OptimizerHints.empty, Unversioned)
      }

      // Compute the set of method symbols that we need to implement
      val sams = {
        val samsBuilder = List.newBuilder[Symbol]
        val seenMethodNames = mutable.Set.empty[MethodName]

        /* scala/bug#10512: any methods which `samInfo.sam` overrides need
         * bridges made for them.
         * On Scala < 2.12.5, `synthCls` is polyfilled to `NoSymbol` and hence
         * `samBridges` will always be empty. This causes our compiler to be
         * bug-compatible on these versions.
         */
        val synthCls = samInfo.synthCls
        val samBridges = if (synthCls == NoSymbol) {
          Nil
        } else {
          import scala.reflect.internal.Flags.BRIDGE
          synthCls.info.findMembers(excludedFlags = 0L, requiredFlags = BRIDGE).toList
        }

        for (sam <- samInfo.sam :: samBridges) {
          /* Remove duplicates, e.g., if we override the same method declared
           * in two super traits.
           */
          if (seenMethodNames.add(encodeMethodSym(sam).name))
            samsBuilder += sam
        }

        samsBuilder.result()
      }

      // def samMethod(...params): resultType = this.f(...params)
      val samMethodDefs = for (sam <- sams) yield {
        val jsParams = sam.tpe.params.map(genParamDef(_, pos))
        val resultType = toIRType(sam.tpe.finalResultType)

        val actualParams = enteringPhase(currentRun.posterasurePhase) {
          for ((formal, param) <- jsParams.zip(sam.tpe.params))
            yield (formal.ref, param.tpe)
        }.map((ensureBoxed _).tupled)

        val call = js.JSFunctionApply(
            js.Select(js.This()(classType), fFieldIdent)(jstpe.AnyType),
            actualParams)

        val body = fromAny(call, enteringPhase(currentRun.posterasurePhase) {
          sam.tpe.finalResultType
        })

        js.MethodDef(js.MemberFlags.empty, encodeMethodSym(sam),
            originalNameOfMethod(sam), jsParams, resultType,
            Some(body))(
            js.OptimizerHints.empty, Unversioned)
      }

      // The class definition
      val classDef = js.ClassDef(
          js.ClassIdent(className),
          NoOriginalName,
          ClassKind.Class,
          None,
          Some(js.ClassIdent(ir.Names.ObjectClass)),
          List(js.ClassIdent(intfName)),
          None,
          None,
          fields = fFieldDef :: Nil,
          methods = ctorDef :: samMethodDefs,
          jsConstructor = None,
          Nil,
          Nil,
          Nil)(
          js.OptimizerHints.empty.withInline(true))

      generatedClasses += classDef -> pos

      className
    }

    private def patchFunParamsWithBoxes(methodSym: Symbol,
        params: List[js.ParamDef], useParamsBeforeLambdaLift: Boolean)(
        implicit pos: Position): (List[js.ParamDef], List[js.VarDef]) = {
      // See the comment in genPrimitiveJSArgs for a rationale about this
      val paramTpes = enteringPhase(currentRun.posterasurePhase) {
        for (param <- methodSym.tpe.params)
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

      (for {
        (param, paramSym) <- params zip paramSyms
      } yield {
        val paramTpe = paramTpes.getOrElse(paramSym.name, paramSym.tpe)
        genPatchedParam(param, fromAny(_, paramTpe))
      }).unzip
    }

    private def genPatchedParam(param: js.ParamDef, rhs: js.VarRef => js.Tree)(
        implicit pos: Position): (js.ParamDef, js.VarDef) = {
      val paramNameIdent = param.name
      val origName = param.originalName
      val newNameIdent = freshLocalIdent(paramNameIdent.name)(paramNameIdent.pos)
      val newOrigName = origName.orElse(paramNameIdent.name)
      val patchedParam = js.ParamDef(newNameIdent, newOrigName, jstpe.AnyType,
          mutable = false)(param.pos)
      val paramLocal = js.VarDef(paramNameIdent, origName, param.ptpe,
          mutable = false, rhs(patchedParam.ref))
      (patchedParam, paramLocal)
    }

    /** Generates a static method instantiating and calling this
     *  DynamicImportThunk's `apply`:
     *
     *  {{{
     *  static def dynamicImport$;<params>;Ljava.lang.Object(<params>): any = {
     *    new <clsSym>.<init>;<params>:V(<params>).apply;Ljava.lang.Object()
     *  }
     *  }}}
     */
    private def genDynamicImportForwarder(clsSym: Symbol)(
        implicit pos: Position): js.MethodDef = {
      withNewLocalNameScope {
        val ctor = clsSym.primaryConstructor
        val paramSyms = ctor.tpe.params
        val paramDefs = paramSyms.map(genParamDef(_))

        val body = {
          val inst = genNew(clsSym, ctor, paramDefs.map(_.ref))
          genApplyMethod(inst, DynamicImportThunkClass_apply, Nil)
        }

        js.MethodDef(
            js.MemberFlags.empty.withNamespace(js.MemberNamespace.PublicStatic),
            encodeDynamicImportForwarderIdent(paramSyms),
            NoOriginalName,
            paramDefs,
            jstpe.AnyType,
            Some(body))(OptimizerHints.empty, Unversioned)
      }
    }

    // Methods to deal with JSName ---------------------------------------------

    def genExpr(name: JSName)(implicit pos: Position): js.Tree = name match {
      case JSName.Literal(name) => js.StringLiteral(name)
      case JSName.Computed(sym) => genComputedJSName(sym)
    }

    private def genComputedJSName(sym: Symbol)(implicit pos: Position): js.Tree = {
      /* By construction (i.e. restriction in PrepJSInterop), we know that sym
       * must be a static method.
       * Therefore, at this point, we can invoke it by loading its owner and
       * calling it.
       */
      def moduleOrGlobalScope = genLoadModuleOrGlobalScope(sym.owner)
      def module = genLoadModule(sym.owner)

      if (isJSType(sym.owner)) {
        if (!isNonNativeJSClass(sym.owner) || isExposed(sym))
          genJSCallGeneric(sym, moduleOrGlobalScope, args = Nil, isStat = false)
        else
          genApplyJSClassMethod(module, sym, arguments = Nil)
      } else {
        genApplyMethod(module, sym, arguments = Nil)
      }
    }

    // Utilities ---------------------------------------------------------------

    def genVarRef(sym: Symbol)(implicit pos: Position): js.VarRef =
      js.VarRef(encodeLocalSym(sym))(toIRType(sym.tpe))

    def genParamDef(sym: Symbol): js.ParamDef =
      genParamDef(sym, toIRType(sym.tpe))

    private def genParamDef(sym: Symbol, ptpe: jstpe.Type): js.ParamDef =
      genParamDef(sym, ptpe, sym.pos)

    private def genParamDef(sym: Symbol, pos: Position): js.ParamDef =
      genParamDef(sym, toIRType(sym.tpe), pos)

    private def genParamDef(sym: Symbol, ptpe: jstpe.Type,
        pos: Position): js.ParamDef = {
      js.ParamDef(encodeLocalSym(sym)(pos), originalNameOfLocal(sym), ptpe,
          mutable = false)(pos)
    }

    /** Generates a call to `runtime.privateFieldsSymbol()` */
    private def genPrivateFieldsSymbol()(implicit pos: Position): js.Tree = {
      genApplyMethod(genLoadModule(RuntimePackageModule),
          Runtime_privateFieldsSymbol, Nil)
    }

    /** Generate loading of a module value.
     *
     *  Can be given either the module symbol or its module class symbol.
     *
     *  If the module we load refers to the global scope (i.e., it is
     *  annotated with `@JSGlobalScope`), report a compile error specifying
     *  that a global scope object should only be used as the qualifier of a
     *  `.`-selection.
     */
    def genLoadModule(sym0: Symbol)(implicit pos: Position): js.Tree =
      ruleOutGlobalScope(genLoadModuleOrGlobalScope(sym0))

    /** Generate loading of a module value or the global scope.
     *
     *  Can be given either the module symbol of its module class symbol.
     *
     *  Unlike `genLoadModule`, this method does not fail if the module we load
     *  refers to the global scope.
     */
    def genLoadModuleOrGlobalScope(sym0: Symbol)(
        implicit pos: Position): MaybeGlobalScope = {

      require(sym0.isModuleOrModuleClass,
          "genLoadModule called with non-module symbol: " + sym0)

      if (sym0.isModule && sym0.isScala3Defined && sym0.hasAttachment[DottyEnumSingletonCompat.type]) {
        /* #4739 This is a reference to a singleton `case` from a Scala 3 `enum`.
         * It is not a module. Instead, it is a static field (accessed through
         * a static getter) in the `enum` class.
         * We use `originalOwner` and `rawname` because that's what the JVM back-end uses.
         */
        val className = encodeClassName(sym0.originalOwner)
        val getterSimpleName = sym0.rawname.toString()
        val getterMethodName = MethodName(getterSimpleName, Nil, toTypeRef(sym0.tpe))
        val tree = js.ApplyStatic(js.ApplyFlags.empty, className, js.MethodIdent(getterMethodName), Nil)(
            toIRType(sym0.tpe))
        MaybeGlobalScope.NotGlobalScope(tree)
      } else {
        val sym = if (sym0.isModule) sym0.moduleClass else sym0

        // Does that module refer to the global scope?
        if (sym.hasAnnotation(JSGlobalScopeAnnotation)) {
          MaybeGlobalScope.GlobalScope(pos)
        } else {
          if (sym == currentClassSym.get && isModuleInitialized.get != null && isModuleInitialized.value) {
            /* This is a LoadModule(myClass) after the StoreModule(). It is
             * guaranteed to always return the `this` value. We eagerly replace
             * it by a `This()` node to help the elidable constructors analysis
             * of the linker. If we don't do this, then the analysis must
             * tolerate `LoadModule(myClass)` after `StoreModule()` to be
             * side-effect-free, but that would weaken the guarantees resulting
             * from the analysis. In particular, it cannot guarantee that the
             * result of a `LoadModule()` of a module with elidable constructors
             * is always fully initialized.
             */
            MaybeGlobalScope.NotGlobalScope(genThis())
          } else {
            val className = encodeClassName(sym)
            val tree =
              if (isJSType(sym)) js.LoadJSModule(className)
              else js.LoadModule(className)
            MaybeGlobalScope.NotGlobalScope(tree)
          }
        }
      }
    }

    private final val GenericGlobalObjectInformationMsg = {
      "\n  " +
      "See https://www.scala-js.org/doc/interoperability/global-scope.html " +
      "for further information."
    }

    /** Rule out the `GlobalScope` case of a `MaybeGlobalScope` and extract the
     *  value tree.
     *
     *  If `tree` represents the global scope, report a compile error.
     */
    private def ruleOutGlobalScope(tree: MaybeGlobalScope): js.Tree = {
      tree match {
        case MaybeGlobalScope.NotGlobalScope(t) =>
          t
        case MaybeGlobalScope.GlobalScope(pos) =>
          reportErrorLoadGlobalScope()(pos)
      }
    }

    /** Report a compile error specifying that the global scope cannot be
     *  loaded as a value.
     */
    private def reportErrorLoadGlobalScope()(implicit pos: Position): js.Tree = {
      reporter.error(pos,
          "Loading the global scope as a value (anywhere but as the " +
          "left-hand-side of a `.`-selection) is not allowed." +
          GenericGlobalObjectInformationMsg)
      js.Undefined()(pos)
    }

    /** Gen a JS bracket select or a `JSGlobalRef`.
     *
     *  If the receiver is a normal value, i.e., not the global scope, then
     *  emit a `JSBracketSelect`.
     *
     *  Otherwise, if the `item` is a constant string that is a valid
     *  JavaScript identifier, emit a `JSGlobalRef`.
     *
     *  Otherwise, report a compile error.
     */
    private def genJSBracketSelectOrGlobalRef(qual: MaybeGlobalScope,
        item: js.Tree)(implicit pos: Position): js.AssignLhs = {
      qual match {
        case MaybeGlobalScope.NotGlobalScope(qualTree) =>
          js.JSSelect(qualTree, item)

        case MaybeGlobalScope.GlobalScope(_) =>
          genJSGlobalRef(item, "Selecting a field", "selection")
      }
    }

    /** Gen a JS bracket method apply or an apply of a `GlobalRef`.
     *
     *  If the receiver is a normal value, i.e., not the global scope, then
     *  emit a `JSBracketMethodApply`.
     *
     *  Otherwise, if the `method` is a constant string that is a valid
     *  JavaScript identifier, emit a `JSFunctionApply(JSGlobalRef(...), ...)`.
     *
     *  Otherwise, report a compile error.
     */
    private def genJSBracketMethodApplyOrGlobalRefApply(
        receiver: MaybeGlobalScope, method: js.Tree,
        args: List[js.TreeOrJSSpread])(
        implicit pos: Position): js.Tree = {
      receiver match {
        case MaybeGlobalScope.NotGlobalScope(receiverTree) =>
          js.JSMethodApply(receiverTree, method, args)

        case MaybeGlobalScope.GlobalScope(_) =>
          val globalRef = genJSGlobalRef(method, "Calling a method", "call")
          js.JSFunctionApply(globalRef, args)
      }
    }

    private def genJSGlobalRef(propName: js.Tree,
        actionFull: String, actionSimpleNoun: String)(
        implicit pos: Position): js.JSGlobalRef = {
      propName match {
        case js.StringLiteral(value) =>
          if (js.JSGlobalRef.isValidJSGlobalRefName(value)) {
            if (value == "await") {
              global.runReporting.warning(pos,
                  s"$actionFull of the global scope with the name '$value' is deprecated.\n" +
                  "  It may produce invalid JavaScript code causing a SyntaxError in some environments." +
                  GenericGlobalObjectInformationMsg,
                  WarningCategory.Deprecation,
                  currentMethodSym.get)
            }
            js.JSGlobalRef(value)
          } else if (js.JSGlobalRef.ReservedJSIdentifierNames.contains(value)) {
            reporter.error(pos,
                s"Invalid $actionSimpleNoun in the global scope of the reserved identifier name `$value`." +
                GenericGlobalObjectInformationMsg)
            js.JSGlobalRef("erroneous")
          } else {
            reporter.error(pos,
                s"$actionFull of the global scope whose name is not a valid JavaScript identifier is not allowed." +
                GenericGlobalObjectInformationMsg)
            js.JSGlobalRef("erroneous")
          }

        case _ =>
          reporter.error(pos,
              s"$actionFull of the global scope with a dynamic name is not allowed." +
              GenericGlobalObjectInformationMsg)
          js.JSGlobalRef("erroneous")
      }
    }

    private def genAssignableField(sym: Symbol, qualifier: Tree)(
        implicit pos: Position): (js.AssignLhs, Boolean) = {
      def qual = genExpr(qualifier)

      if (isNonNativeJSClass(sym.owner)) {
        val f = if (isExposed(sym)) {
          js.JSSelect(qual, genExpr(jsNameOf(sym)))
        } else if (isAnonymousJSClass(sym.owner)) {
          js.JSSelect(
              js.JSSelect(qual, genPrivateFieldsSymbol()),
              encodeFieldSymAsStringLiteral(sym))
        } else {
          js.JSPrivateSelect(qual, encodeFieldSym(sym))
        }

        (f, true)
      } else if (jsInterop.topLevelExportsOf(sym).nonEmpty) {
        val f = js.SelectStatic(encodeFieldSym(sym))(jstpe.AnyType)
        (f, true)
      } else if (jsInterop.staticExportsOf(sym).nonEmpty) {
        val exportInfo = jsInterop.staticExportsOf(sym).head
        val companionClass = patchedLinkedClassOfClass(sym.owner)
        val f = js.JSSelect(
            genPrimitiveJSClass(companionClass),
            js.StringLiteral(exportInfo.jsName))

        (f, true)
      } else {
        val fieldIdent = encodeFieldSym(sym)

        /* #4370 Fields cannot have type NothingType, so we box them as
         * scala.runtime.Nothing$ instead. They will be initialized with
         * `null`, and any attempt to access them will throw a
         * `ClassCastException` (generated in the unboxing code).
         */
        toIRType(sym.tpe) match {
          case jstpe.NothingType =>
            val f = js.Select(qual, fieldIdent)(
                encodeClassType(RuntimeNothingClass))
            (f, true)
          case ftpe =>
            val f = js.Select(qual, fieldIdent)(ftpe)
            (f, false)
        }
      }
    }

    /** Generate access to a static field. */
    private def genStaticField(sym: Symbol)(implicit pos: Position): js.Tree = {
      /* Static fields are not accessed directly at the IR level, because there
       * is no lazily-executed static initializer to make sure they are
       * initialized. Instead, reading a static field must always go through a
       * static getter with the same name as the field, 0 argument, and with
       * the field's type as result type. The static getter is responsible for
       * proper initialization, if required.
       */
      import scalaPrimitives._
      import jsPrimitives._
      if (isPrimitive(sym)) {
        getPrimitive(sym) match {
          case UNITVAL => js.Undefined()
        }
      } else {
        val className = encodeClassName(sym.owner)
        val method = encodeStaticFieldGetterSym(sym)
        js.ApplyStatic(js.ApplyFlags.empty, className, method, Nil)(toIRType(sym.tpe))
      }
    }

    private def genLinkTimeTree(cond: Tree)(
        implicit pos: Position): js.LinkTimeTree = {
      import js.LinkTimeOp._
      val dummy = js.LinkTimeTree.Property("dummy", toIRType(cond.tpe))
      cond match {
        case Literal(Constant(b: Boolean)) =>
          js.LinkTimeTree.BooleanConst(b)

        case Literal(Constant(i: Int)) =>
          js.LinkTimeTree.IntConst(i)

        case Literal(_) =>
          reporter.error(cond.pos,
              s"Invalid literal $cond inside linkTimeIf. " +
              "Only boolean and int values can be used in linkTimeIf.")
          dummy

        case Ident(name) =>
          reporter.error(cond.pos,
              s"Invalid identifier $name inside linkTimeIf. " +
              "Only @linkTimeProperty annotated values can be used in linkTimeIf.")
          dummy

        // !x
        case Apply(Select(t, nme.UNARY_!), Nil) if cond.symbol == definitions.Boolean_not =>
          val lt = genLinkTimeTree(t)
          js.LinkTimeTree.BinaryOp(Boolean_==, lt, js.LinkTimeTree.BooleanConst(false))

        // if(foo()) (...)
        case Apply(prop, Nil) =>
          getLinkTimeProperty(prop).getOrElse {
            reporter.error(prop.pos,
                s"Invalid identifier inside linkTimeIf. " +
                "Only @linkTimeProperty annotated values can be used in linkTimeIf.")
            dummy
          }

        // if(lhs <comp> rhs) (...)
        case Apply(Select(cond1, comp), List(cond2)) =>
          val tpe = toIRType(cond.tpe)
          val c1 = genLinkTimeTree(cond1)
          val c2 = genLinkTimeTree(cond2)
          val dummyOp = -1
          val op: Code =
            if (c1.tpe == jstpe.IntType) {
              comp match {
                case nme.EQ => Int_==
                case nme.NE => Int_!=
                case nme.GT => Int_>
                case nme.GE => Int_>=
                case nme.LT => Int_<
                case nme.LE => Int_<=
                case _      =>
                  reporter.error(cond.pos,
                      s"Invalid operation '$comp' inside linkTimeIf. " +
                      "Only '==', '!=', '>', '>=', '<', '<=' " +
                      "operations are allowed for integer values in linkTimeIf.")
                  dummyOp
              }
            } else if (c1.tpe == jstpe.BooleanType) {
              comp match {
                case nme.EQ   => Boolean_==
                case nme.NE   => Boolean_!=
                case nme.ZAND => Boolean_&&
                case nme.ZOR  => Boolean_||
                case _      =>
                  reporter.error(cond.pos,
                      s"Invalid operation '$comp' inside linkTimeIf. " +
                      "Only '==', '!=', '&&', and '||' operations are allowed for boolean values in linkTimeIf.")
                  dummyOp
              }
            } else {
              dummyOp
            }
          if (op == dummyOp) dummy
          else js.LinkTimeTree.BinaryOp(op, c1, c2)

        case t =>
          reporter.error(t.pos,
              s"Only @linkTimeProperty annotated values, int and boolean constants, " +
              "and binary operations are allowd in linkTimeIf.")
          dummy
      }
    }
  }

  private def getLinkTimeProperty(tree: Tree): Option[js.LinkTimeTree.Property] = {
    tree.symbol.getAnnotation(LinkTimePropertyAnnotation)
      .flatMap(_.args.headOption)
      .flatMap {
        case Literal(Constant(v: String)) =>
          Some(js.LinkTimeTree.Property(v, toIRType(tree.symbol.tpe.resultType))(tree.pos))
        case _ => None
      }
    }

  private lazy val hasNewCollections =
    !scala.util.Properties.versionNumberString.startsWith("2.12.")

  /** Tests whether the given type represents a JavaScript type,
   *  i.e., whether it extends scala.scalajs.js.Any.
   */
  def isJSType(tpe: Type): Boolean =
    isJSType(tpe.typeSymbol)

  /** Tests whether the given type symbol represents a JavaScript type,
   *  i.e., whether it extends scala.scalajs.js.Any.
   */
  def isJSType(sym: Symbol): Boolean =
    sym.hasAnnotation(JSTypeAnnot)

  /** Tests whether the given class is a non-native JS class. */
  def isNonNativeJSClass(sym: Symbol): Boolean =
    !sym.isTrait && isJSType(sym) && !sym.hasAnnotation(JSNativeAnnotation)

  def isNestedJSClass(sym: Symbol): Boolean =
    sym.isLifted && !isStaticModule(sym.originalOwner) && isJSType(sym)

  /** Tests whether the given class is a JS native class. */
  private def isJSNativeClass(sym: Symbol): Boolean =
    sym.hasAnnotation(JSNativeAnnotation)

  /** Tests whether the given member is exposed, i.e., whether it was
   *  originally a public or protected member of a non-native JS class.
   */
  private def isExposed(sym: Symbol): Boolean = {
    !sym.isBridge && {
      if (sym.isLazy)
        sym.isAccessor && sym.accessed.hasAnnotation(ExposedJSMemberAnnot)
      else
        sym.hasAnnotation(ExposedJSMemberAnnot)
    }
  }

  /** Test whether `sym` is the symbol of a JS function definition */
  private def isJSFunctionDef(sym: Symbol): Boolean = {
    /* A JS function may only reach the backend if it originally was a lambda.
     * This is difficult to check in the backend, so we use the fact that a
     * non-lambda, concrete, non-native JS type, cannot implement a method named
     * `apply`.
     *
     * Therefore, a class is a JS lambda iff it is anonymous, its direct
     * super class is `js.Function`, and it contains an implementation of an
     * `apply` method.
     *
     * Note that being anonymous implies being concrete and non-native, so we
     * do not have to test that.
     */
    sym.isAnonymousClass &&
    sym.superClass == JSFunctionClass &&
    sym.info.decl(nme.apply).filter(JSCallingConvention.isCall(_)).exists
  }

  private def hasDefaultCtorArgsAndJSModule(classSym: Symbol): Boolean = {
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

  private object DefaultParamInfo {
    /** Is the symbol applicable to `DefaultParamInfo`?
     *
     *  This is true iff it is a default accessor and it is not an value class
     *  `$extension` method. The latter condition is for #4583.
     *
     *  Excluding all `$extension` methods is fine because `DefaultParamInfo`
     *  is used for JS default accessors, i.e., default accessors of
     *  `@js.native def`s or of `def`s in JS types. Those can never appear in
     *  an `AnyVal` class (as a class, it cannot contain `@js.native def`s, and
     *  as `AnyVal` it cannot also extend `js.Any`).
     */
    def isApplicable(sym: Symbol): Boolean =
      sym.hasFlag(Flags.DEFAULTPARAM) && !sym.name.endsWith("$extension")
  }

  /** Info about a default param accessor.
   *
   *  `DefaultParamInfo.isApplicable(sym)` must be true for this class to make
   *  sense.
   */
  private class DefaultParamInfo(sym: Symbol) {
    private val methodName = nme.defaultGetterToMethod(sym.name)

    def isForConstructor: Boolean = methodName == nme.CONSTRUCTOR

    /** When `isForConstructor` is true, returns the owner of the attached
     *  constructor.
     */
    def constructorOwner: Symbol = patchedLinkedClassOfClass(sym.owner)

    /** When `isForConstructor` is false, returns the method attached to the
     *  specified default accessor.
     */
    def attachedMethod: Symbol = {
      // If there are overloads, we need to find the one that has default params.
      val overloads = sym.owner.info.decl(methodName)
      if (!overloads.isOverloaded) {
        overloads
      } else {
        /* We should use `suchThat` here instead of `filter`+`head`. Normally,
         * it can never happen that two overloads of a method both have default
         * params. However, there is a loophole until Scala 2.12, with the
         * `-Xsource:2.10` flag, which disables a check and allows that to
         * happen in some circumstances. This is still tested as part of the
         * partest test `pos/t8157-2.10.scala`. The use of `filter` instead of
         * `suchThat` allows those situations not to crash, although that is
         * mostly for (intense) backward compatibility purposes.
         *
         * This loophole can be use to construct a case of miscompilation where
         * one of the overloads would be `@js.native` but the other not. We
         * don't really care, though, as it needs some deep hackery to produce
         * it.
         */
        overloads
          .filter(_.paramss.exists(_.exists(_.hasFlag(Flags.DEFAULTPARAM))))
          .alternatives
          .head
      }
    }
  }

  private def isStringType(tpe: Type): Boolean =
    tpe.typeSymbol == StringClass

  protected lazy val isHijackedClass: Set[Symbol] = {
    /* This list is a duplicate of ir.Definitions.HijackedClasses, but
     * with global.Symbol's instead of IR encoded names as Strings.
     * We also add java.lang.Void, which BoxedUnit "erases" to, and
     * HackedStringClass if it is defined.
     */
    val s = Set[Symbol](
        JavaLangVoidClass, BoxedUnitClass, BoxedBooleanClass,
        BoxedCharacterClass, BoxedByteClass, BoxedShortClass, BoxedIntClass,
        BoxedLongClass, BoxedFloatClass, BoxedDoubleClass, StringClass
    )
    if (HackedStringClass == NoSymbol) s
    else s + HackedStringClass
  }

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
    sym.isModuleClass && !sym.isLifted

  def isAnonymousJSClass(sym: Symbol): Boolean = {
    /* sym.isAnonymousClass simply checks if
     * `name containsName tpnme.ANON_CLASS_NAME`
     * so after flatten (which we are) it identifies classes nested inside
     * anonymous classes as anonymous (notably local classes, see #4278).
     *
     * Instead we recognize names generated for anonymous classes:
     * tpnme.ANON_CLASS_NAME followed by $<n> where `n` is an integer.
     */
    isJSType(sym) && {
      val name = sym.name
      val i = name.lastIndexOf('$')

      i > 0 &&
      name.endsWith(tpnme.ANON_CLASS_NAME, i) &&
      (i + 1 until name.length).forall(j => name.charAt(j).isDigit)
    }
  }

  sealed abstract class MaybeGlobalScope

  object MaybeGlobalScope {
    case class NotGlobalScope(tree: js.Tree) extends MaybeGlobalScope

    case class GlobalScope(pos: Position) extends MaybeGlobalScope
  }

  /** Marker object for undefined parameters in JavaScript semantic calls.
   *
   *  To be used inside a `js.Transient` node.
   */
  case object UndefinedParam extends js.Transient.Value {
    val tpe: jstpe.Type = jstpe.UndefType

    def traverse(traverser: ir.Traversers.Traverser): Unit = ()

    def transform(transformer: ir.Transformers.Transformer, isStat: Boolean)(
        implicit pos: ir.Position): js.Tree = {
      js.Transient(this)
    }

    def printIR(out: ir.Printers.IRTreePrinter): Unit =
      out.print("<undefined-param>")
  }
}

private object GenJSCode {
  private val JSObjectClassName = ClassName("scala.scalajs.js.Object")
  private val JavaScriptExceptionClassName = ClassName("scala.scalajs.js.JavaScriptException")

  private val newSimpleMethodName = SimpleMethodName("new")

  private val ObjectArgConstructorName =
    MethodName.constructor(List(jstpe.ClassRef(ir.Names.ObjectClass)))

  private val lengthMethodName =
    MethodName("length", Nil, jstpe.IntRef)
  private val charAtMethodName =
    MethodName("charAt", List(jstpe.IntRef), jstpe.CharRef)

  private val thisOriginalName = OriginalName("this")

  private object BlockOrAlone {
    def unapply(tree: js.Tree): Some[(List[js.Tree], js.Tree)] = tree match {
      case js.Block(trees) => Some((trees.init, trees.last))
      case _               => Some((Nil, tree))
    }
  }

  private object FirstInBlockOrAlone {
    def unapply(tree: js.Tree): Some[(js.Tree, List[js.Tree])] = tree match {
      case js.Block(trees) => Some((trees.head, trees.tail))
      case _               => Some((tree, Nil))
    }
  }
}
