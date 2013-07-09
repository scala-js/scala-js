/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package js

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import scalajs.JSGlobal

/** Generate JavaScript code and output it to disk
 *
 *  @author Sébastien Doeraene
 */
abstract class GenJSCode extends SubComponent
                            with TypeKinds
                            with JSEncoding
                            with JSBridges
                            with JSDesugaring
                            with GenJSFiles {
  val global: JSGlobal

  import global._
  import definitions._
  import jsDefinitions._

  import treeInfo.hasSynthCaseSymbol

  import platform.isMaybeBoxed

  val phaseName = "jscode"

  override def newPhase(p: Phase) = new JSCodePhase(p)

  class JSCodePhase(prev: Phase) extends StdPhase(prev) {

    override def name = phaseName
    override def description = "Generate JavaScript code from ASTs"
    override def erasedTypes = true

    // Some state --------------------------------------------------------------

    var currentCUnit: CompilationUnit = _
    var currentClassSym: Symbol = _
    var currentMethodSym: Symbol = _
    var isModuleInitialized: Boolean = false // see genApply for super calls
    var methodHasTailJump: Boolean = false
    var methodTailJumpThisSym: Symbol = _
    var methodTailJumpLabelSym: Symbol = _
    var methodTailJumpFormalArgs: List[Symbol] = _

    // Top-level apply ---------------------------------------------------------

    override def run() {
      scalaPrimitives.init()
      jsPrimitives.init()
      super.run()
    }

    /** Generate JS code for a compilation unit
     *  This method iterates over all the class and interface definitions
     *  found in the compilation unit.
     *
     *  For every interface, it calls `genInterface()`.
     *  For every class, it calls `genClass()`. If it is a module class, it
     *  also calls `genModuleAccessor()`.
     *
     *  Classes representing raw JS types and primitive types, as well as the
     *  scala.Array class, are not actually emitted.
     *
     *  Emitted class and interface definitions are grouped into bundles
     *  according to their so-called representative, which is basically their
     *  enclosing top-level class/trait/object. Companions are also grouped
     *  together.
     *
     *  Each bundle is then wrapped in a closure:
     *
     *     (function($) {
     *       ...
     *     })($ScalaJSEnvironment);
     *
     *  which is desugared with `JSDesugaring`, and then sent to disc
     *  with `GenJSFiles`.
     */
    override def apply(cunit: CompilationUnit) {
      try {
        currentCUnit = cunit

        val generatedBundles = mutable.Map.empty[Symbol, ListBuffer[js.Tree]]

        def representativeTopLevelClass(classSymbol: Symbol): Symbol = {
          val topLevel = beforePhase(currentRun.flattenPhase) {
            classSymbol.enclosingTopLevelClass
          }
          if (topLevel.isImplClass)
            topLevel.owner.info.decl(tpnme.interfaceName(topLevel.name))
          else if (topLevel.isModuleClass && (topLevel.linkedClassOfClass != NoSymbol))
            topLevel.linkedClassOfClass
          else
            topLevel
        }

        def gen(tree: Tree) {
          tree match {
            case EmptyTree => ()
            case PackageDef(_, stats) => stats foreach gen
            case cd: ClassDef =>
              implicit val pos = tree.pos
              val sym = cd.symbol

              /* Always get the bundle, so that it is created even if it would
               * be empty. This is needed for pickles to be emitted.
               */
              val representative = representativeTopLevelClass(sym)
              val bundle = generatedBundles.getOrElseUpdate(
                  representative, new ListBuffer)

              /* Do not actually emit code for raw JS types, primitive types
               * nor scala.Array.
               */
              val isPrimitive =
                isRawJSType(sym.tpe) || isPrimitiveValueClass(sym) ||
                (sym == ArrayClass)

              if (!isPrimitive) {
                if (sym.isInterface) {
                  bundle += genInterface(cd)
                } else {
                  bundle += genClass(cd)
                  if ((sym.isModuleClass && !sym.isLifted) || sym.isImplClass)
                    bundle += genModuleAccessor(sym)
                }
              }
          }
        }

        gen(cunit.body)

        for ((representative, classes) <- generatedBundles) {
          implicit val pos = representative.pos
          val bundleTree =
            if (classes.isEmpty) js.EmptyTree
            else {
              val tree = js.Apply(
                  js.Function(List(environment), js.Block(classes.toList)),
                  List(js.Ident(ScalaJSEnvironmentFullName)))
              desugarJavaScript(tree)
            }

          genJSFiles(cunit, representative, bundleTree)
        }
      } finally {
        currentCUnit = null
        currentClassSym = null
        currentMethodSym = null
      }
    }

    // Generate a class --------------------------------------------------------

    /** Gen JS code for a class definition (maybe a module class)
     *  It emits an ES6 class declaration with all the fields (ValDefs) and
     *  methods (DefDefs) in the class as JS fields and methods.
     *  The constructors are emitted as regular JS methods with name '<init>'.
     *
     *  In addition, bridges for JS-friendly (non-mangled) method names are
     *  generated with `JSBridges`.
     *
     *  The class definition itself is wrapped inside a closure which is
     *  only registered to the Scala.js environment using `registerClass`. It
     *  is not automatically created, only on demand.
     */
    def genClass(cd: ClassDef): js.Tree = {
      implicit val jspos = cd.pos
      val ClassDef(mods, name, _, impl) = cd
      currentClassSym = cd.symbol
      val originalClassName = Some(currentClassSym.fullNameAsName('.').decoded)

      val superClass =
        if (currentClassSym.superClass == NoSymbol) ObjectClass
        else currentClassSym.superClass

      val generatedMembers = new ListBuffer[js.Tree]

      if (!currentClassSym.isInterface)
        generatedMembers += genConstructor(cd)

      def gen(tree: Tree) {
        tree match {
          case EmptyTree => ()
          case Template(_, _, body) => body foreach gen

          case ValDef(mods, name, tpt, rhs) =>
            () // fields are added in the constructor (genConstructor(cd))

          case dd: DefDef =>
            generatedMembers ++= genMethod(dd)

          case _ => abort("Illegal tree in gen: " + tree)
        }
      }

      gen(impl)

      // Generate the bridges, then steal the constructor bridges (1 at most)
      val bridges0 = genBridgesForClass(currentClassSym)
      val (constructorBridges0, bridges) = bridges0.partition {
        case js.MethodDef(js.Ident("init\ufe33", _), _, _) => true
        case _ => false
      }
      assert(constructorBridges0.size <= 1)

      val constructorBridge = {
        if (!currentClassSym.isImplClass) constructorBridges0.headOption
        else {
          // Make up
          Some(js.MethodDef(js.Ident("irrelevant"), Nil, js.Skip()))
        }
      }

      val typeVar = js.Ident("Class", originalClassName)
      val classDefinition = js.ClassDef(typeVar,
          encodeClassSym(superClass), generatedMembers.toList ++ bridges)

      /* function JSClass(<args of the constructor bridge>) {
       *   Class.call(this);
       *   <body of the constructor bridge>
       * }
       * JSClass.prototype = Class.prototype;
       */
      val jsConstructorVar = js.Ident("JSClass", originalClassName)
      val createJSConstructorStat = constructorBridge match {
        case Some(js.MethodDef(_, args, body)) =>
          js.Block(List(
              js.FunDef(jsConstructorVar, args, js.Block(List(
                  js.ApplyMethod(typeVar, js.Ident("call"), List(js.This())),
                  body))),
              js.Assign(
                  js.DotSelect(jsConstructorVar, js.Ident("prototype")),
                  js.DotSelect(typeVar, js.Ident("prototype")))
              ))

        case _ =>
          js.Assign(jsConstructorVar, js.Undefined())
      }

      val createClassStat = {
        val nameArg = encodeFullNameLit(currentClassSym)
        val typeArg = typeVar
        val jsConstructorArg = jsConstructorVar
        val parentArg = encodeFullNameLit(superClass)
        val ancestorsArg = js.ObjectConstr(
            for (ancestor <- currentClassSym :: currentClassSym.ancestors)
              yield (encodeFullNameLit(ancestor), js.BooleanLiteral(true)))

        js.ApplyMethod(environment, js.PropertyName("createClass"),
            List(nameArg, typeArg, jsConstructorArg, parentArg, ancestorsArg))
      }

      val createClassFun = js.Function(List(environment),
          js.Block(List(classDefinition, createJSConstructorStat),
              createClassStat))

      val registerClassStat = {
        val nameArg = encodeFullNameLit(currentClassSym)
        js.ApplyMethod(environment, js.PropertyName("registerClass"),
            List(nameArg, createClassFun))
      }

      currentClassSym = null

      registerClassStat
    }

    // Generate an interface ---------------------------------------------------

    /** Gen JS code for an interface definition
     *  This is very simple, as interfaces have virtually no existence at
     *  runtime. They exist solely for reflection purposes.
     */
    def genInterface(cd: ClassDef): js.Tree = {
      implicit val pos = cd.pos
      val sym = cd.symbol

      val createInterfaceStat = {
        val nameArg = encodeFullNameLit(sym)
        val ancestorsArg = js.ObjectConstr(
            for (ancestor <- sym :: sym.ancestors)
              yield (encodeFullNameLit(ancestor), js.BooleanLiteral(true)))

        js.ApplyMethod(environment, js.PropertyName("createInterface"),
            List(nameArg, ancestorsArg))
      }

      createInterfaceStat
    }

    // Generate the constructor of a class -------------------------------------

    /** Gen JS code for the constructor of a class
     *  The constructor calls the super constructor, then creates all the
     *  fields of the object by assigning them to the zero of their type.
     */
    def genConstructor(cd: ClassDef): js.MethodDef = {
      // Non-method term members are fields, except for module members.
      val createFieldsStats = {
        for {
          f <- currentClassSym.info.decls
          if !f.isMethod && f.isTerm && !f.isModule
        } yield {
          implicit val pos = f.pos
          val fieldName = encodeFieldSym(f)
          js.Assign(js.Select(js.This(), fieldName), genZeroOf(f.tpe))
        }
      }.toList

      {
        implicit val pos = cd.pos
        val superCall =
          js.ApplyMethod(js.Super(), js.PropertyName("constructor"), Nil)
        js.MethodDef(js.PropertyName("constructor"), Nil,
            js.Block(superCall :: createFieldsStats))
      }
    }

    // Generate a method -------------------------------------------------------

    /** Gen JS code for a method definition in a class
     *  Methods are compiled as ES6 methods in the emitted JS6 class.
     *  On the JS side, method names are mangled to encode the full signature
     *  of the Scala method, as described in `JSEncoding`, to support
     *  overloading.
     *
     *  Abstract methods are not emitted at all. An alternative might to
     *  generate a method throwing a java.lang.AbstractMethodError.
     *
     *  Methods marked with @native are not emitted. Instead, their body is
     *  lookup up in the global repository for natives available in the
     *  Scala.js environment.
     *
     *  Constructors are emitted by generating their body as a statement, then
     *  return `this`.
     *
     *  Other (normal) methods are emitted with `genMethodBody()`.
     */
    def genMethod(dd: DefDef): Option[js.Tree] = {
      implicit val jspos = dd.pos
      val DefDef(mods, name, _, vparamss, _, rhs) = dd
      currentMethodSym = dd.symbol

      isModuleInitialized = false
      methodHasTailJump = false
      methodTailJumpThisSym = NoSymbol
      methodTailJumpLabelSym = NoSymbol
      methodTailJumpFormalArgs = Nil

      assert(vparamss.isEmpty || vparamss.tail.isEmpty,
          "Malformed parameter list: " + vparamss)
      val params = if (vparamss.isEmpty) Nil else vparamss.head map (_.symbol)

      val jsParams =
        for (param <- params)
          yield encodeLocalSym(param)(param.pos)

      val isNative = currentMethodSym.hasAnnotation(NativeAttr)
      val isAbstractMethod =
        (currentMethodSym.isDeferred || currentMethodSym.owner.isInterface)

      val methodPropIdent = encodeMethodSym(currentMethodSym)

      val result = {
        if (isPrimitive(currentMethodSym)) {
          // Do not output code for primitive methods, it won't be called
          None
        } else if (isNative) {
          val nativeID = encodeFullName(currentClassSym) +
            " :: " + methodPropIdent.name
          Some(js.CustomDef(methodPropIdent,
              js.Select(js.DotSelect(environment, js.Ident("natives")),
                  js.PropertyName(nativeID))))
        } else if (isAbstractMethod) {
          None
        } else {
          val returnType = toTypeKind(currentMethodSym.tpe.resultType)
          val body = {
            if (currentMethodSym.isConstructor)
              js.Block(List(genStat(rhs)), js.Return(js.This()))
            else
              genMethodBody(rhs, params, toTypeKind(currentMethodSym.tpe.resultType))
          }
          Some(js.MethodDef(methodPropIdent, jsParams, body))
        }
      }

      currentMethodSym = null

      result
    }

    // Generate a module accessor ----------------------------------------------

    /** Gen JS code for a module accessor
     *
     *  Modules are not created upon startup. Instead, they are registered to
     *  the Scala.js environment with `registerModule`, giving their full name
     *  and class.
     *
     *  The Scala.js environment takes care of setting up the necessary lazy
     *  accessors and the actual creation of the module when needed.
     *
     *  Since modules have exactly one public, parameterless constructor, the
     *  Scala.js environment can create them using the JS bridge for the
     *  constructor.
     */
    def genModuleAccessor(sym: Symbol): js.Tree = {
      implicit val pos = sym.pos

      /* For whatever reason, a module nested in another module will be
       * lifted as a top-level module, with its module class, but
       * sym.companionModule will be NoSymbol.
       * This makes it awkward to get its full name without $ using
       * standard methods of JSEncoding.
       * Instead, we drop manually the trailing $ of the class full name.
       */
      val className = encodeFullName(sym, '.')
      val moduleName = dropTrailingDollar(className)

      val moduleNameArg = js.StringLiteral(moduleName, Some(className))
      val classNameArg = js.StringLiteral(className, Some(className))

      js.ApplyMethod(environment, js.Ident("registerModule"),
          List(moduleNameArg, classNameArg))
    }

    // Code generation ---------------------------------------------------------

    /** Generate the body of a (non-constructor) method
     *
     *  Most normal methods are emitted straightforwardly. If the result
     *  type is Unit, then the body is emitted as a statement. Otherwise, it is
     *  emitted as an expression and wrapped in a `js.Return()` statement.
     *
     *  The additional complexity of this method handles the transformation of
     *  recursive tail calls. The `tailcalls` phase unhelpfully transforms
     *  them as one big LabelDef surrounding the body of the method, and
     *  label-Apply's for recursive tail calls.
     *  Here, we transform the outer LabelDef into a labelled `while (true)`
     *  loop. Label-Apply's to the LabelDef are turned into a `continue` of
     *  that loop. The body of the loop is a `js.Return()` of the body of the
     *  LabelDef (even if the return type is Unit), which will break out of
     *  the loop as necessary.
     */
    def genMethodBody(tree: Tree, paramsSyms: List[Symbol],
        resultTypeKind: TypeKind): js.Tree = {
      implicit val pos = tree.pos

      tree match {
        case Block(
            List(thisDef @ ValDef(_, nme.THIS, _, initialThis)),
            ld @ LabelDef(labelName, _, rhs)) =>
          // This method has tail jumps
          methodHasTailJump = true
          methodTailJumpLabelSym = ld.symbol
          initialThis match {
            case This(_) =>
              methodTailJumpThisSym = thisDef.symbol
              methodTailJumpFormalArgs = thisDef.symbol :: paramsSyms
            case Ident(_) =>
              methodTailJumpThisSym = NoSymbol
              methodTailJumpFormalArgs = paramsSyms
          }

          val theLoop =
            js.While(js.BooleanLiteral(true), js.Return(genExpr(rhs)),
                Some(js.Ident("tailCallLoop")))

          if (methodTailJumpThisSym == NoSymbol) {
            theLoop
          } else {
            js.Block(List(
                js.VarDef(encodeLocalSym(methodTailJumpThisSym), js.This())),
                theLoop)
          }

        case _ =>
          val bodyIsStat = resultTypeKind == UNDEFINED
          if (bodyIsStat) genStat(tree)
          else js.Return(genExpr(tree))
      }
    }

    /** Gen JS code for a tree in statement position (from JS's perspective)
     *
     *  Here we handle Assign trees directly. All other types of nodes are
     *  redirected `genExpr()`.
     */
    def genStat(tree: Tree): js.Tree = {
      implicit val pos = tree.pos

      tree match {
        /** qualifier.field = rhs */
        case Assign(lhs @ Select(qualifier, _), rhs) =>
          val sym = lhs.symbol

          val member =
            if (sym.isStaticMember) {
              genStaticMember(sym)
            } else {
              js.Select(genExpr(qualifier), encodeFieldSym(sym))
            }

          js.Assign(member, genExpr(rhs))

        /** lhs = rhs */
        case Assign(lhs, rhs) =>
          val sym = lhs.symbol
          js.Assign(encodeLocalSym(sym), genExpr(rhs))

        case _ =>
          exprToStat(genExpr(tree))
      }
    }

    /** Turn a JavaScript expression into a statement */
    def exprToStat(tree: js.Tree): js.Tree = {
      // Any JavaScript expression is also a statement
      tree
    }

    /** Gen JS code for a tree in expression position (from JS's perspective)
     *
     *  This is the main transformation method. Each node of the Scala AST
     *  is transformed into an equivalent portion of the JS AST.
     */
    def genExpr(tree: Tree): js.Tree = {
      implicit val pos = tree.pos

      /** Predicate satisfied by LabelDefs produced by the pattern matcher */
      def isCaseLabelDef(tree: Tree) =
        tree.isInstanceOf[LabelDef] && hasSynthCaseSymbol(tree)

      tree match {
        /** LabelDefs (for while and do..while loops) */
        case lblDf: LabelDef =>
          genLabelDef(lblDf)

        /** val nme.THIS = this
         *  Must have been eliminated by the tail call transform performed
         *  by `genMethodBody()`.
         */
        case ValDef(_, nme.THIS, _, _) =>
          abort("ValDef(_, nme.THIS, _, _) found at: " + tree.pos)

        /** Local val or var declaration */
        case ValDef(_, name, _, rhs) =>
          val sym = tree.symbol
          val lhsTree =
            if (rhs == EmptyTree) genZeroOf(sym.tpe)
            else genExpr(rhs)
          statToExpr(js.VarDef(encodeLocalSym(sym), lhsTree))

        case If(cond, thenp, elsep) =>
          js.If(genExpr(cond), genExpr(thenp), genExpr(elsep))

        case Return(expr) =>
          js.Return(genExpr(expr))

        case t: Try =>
          genTry(t)

        case Throw(expr) =>
          js.Throw(genExpr(expr))

        case app: Apply =>
          genApply(app)

        case app: ApplyDynamic =>
          genApplyDynamic(app)

        /** this
         *  Normally encoded straightforwardly as a JS this.
         *  But must be replaced by the tail-jump-this local variable if there
         *  is one.
         */
        case This(qual) =>
          val symIsModuleClass = tree.symbol.isModuleClass
          assert(tree.symbol == currentClassSym || symIsModuleClass,
              "Trying to access the this of another class: " +
              "tree.symbol = " + tree.symbol +
              ", class symbol = " + currentClassSym +
              " compilation unit:" + currentCUnit)
          if (symIsModuleClass && tree.symbol != currentClassSym) {
            genLoadModule(tree.symbol)
          } else if (methodTailJumpThisSym != NoSymbol) {
            encodeLocalSym(methodTailJumpThisSym)
          } else {
            js.This()
          }

        case Select(Ident(nme.EMPTY_PACKAGE_NAME), module) =>
          assert(tree.symbol.isModule,
              "Selection of non-module from empty package: " + tree +
              " sym: " + tree.symbol + " at: " + (tree.pos))
          genLoadModule(tree.symbol)

        case Select(qualifier, selector) =>
          val sym = tree.symbol

          if (sym.isModule) {
            if (settings.debug.value)
              log("LOAD_MODULE from Select(qualifier, selector): " + sym)
            assert(!tree.symbol.isPackageClass, "Cannot use package as value: " + tree)
            genLoadModule(sym)
          } else if (sym.isStaticMember) {
            genStaticMember(sym)
          } else {
            js.Select(genExpr(qualifier), encodeFieldSym(sym))
          }

        case Ident(name) =>
          val sym = tree.symbol
          if (!sym.isPackage) {
            if (sym.isModule) {
              assert(!sym.isPackageClass, "Cannot use package as value: " + tree)
              genLoadModule(sym)
            } else {
              encodeLocalSym(sym)
            }
          } else {
            sys.error("Cannot use package as value: " + tree)
          }

        case Literal(value) =>
          value.tag match {
            case UnitTag =>
              js.Undefined()
            case BooleanTag =>
              js.BooleanLiteral(value.booleanValue)
            case ByteTag | ShortTag | CharTag | IntTag | LongTag =>
              js.IntLiteral(value.longValue)
            case FloatTag | DoubleTag =>
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

        /** Block that appeared as the result of a translated match
         *  Such blocks are recognized by having at least one element that is
         *  a so-called case-label-def.
         *  The method `genTranslatedMatch()` takes care of compiling the
         *  actual match.
         */
        case Block(stats, expr) if (expr +: stats) exists isCaseLabelDef =>
          /* The assumption is once we encounter a case, the remainder of the
           * block will consist of cases.
           * The prologue may be empty, usually it is the valdef that stores
           * the scrut.
           */
          val (prologue, cases) = stats span (s => !isCaseLabelDef(s))
          assert((expr +: cases) forall isCaseLabelDef,
              "Assumption on the form of translated matches broken: " + tree)

          val translatedMatch =
            genTranslatedMatch(cases map (_.asInstanceOf[LabelDef]),
                expr.asInstanceOf[LabelDef])

          if (prologue.isEmpty) translatedMatch
          else js.Block(prologue map genStat, translatedMatch)

        /** Normal block */
        case Block(stats, expr) =>
          val statements = stats map genStat
          val expression = genExpr(expr)
          js.Block(statements, expression)

        case Typed(Super(_, _), _) =>
          genExpr(This(currentClassSym))

        case Typed(expr, _) =>
          genExpr(expr)

        case Assign(_, _) =>
          statToExpr(genStat(tree))

        /** Array constructor */
        case av: ArrayValue =>
          genArrayValue(av)

        /** A Match reaching the backend is supposed to be optimized as a switch */
        case mtch: Match =>
          genMatch(mtch)

        case EmptyTree =>
          // TODO Hum, I do not think this is OK
          js.Undefined()

        case _ =>
          abort("Unexpected tree in genExpr: " +
              tree + "/" + tree.getClass + " at: " + tree.pos)
      }
    } // end of GenJSCode.genExpr()

    /** Turn a JavaScript statement into an expression */
    def statToExpr(tree: js.Tree): js.Tree = {
      implicit val jspos = tree.pos

      tree match {
        case _ : js.Apply =>
          tree
        case js.Block(stats, stat) =>
          js.Block(stats, statToExpr(stat))
        case _ =>
          js.Block(List(tree), js.Undefined())
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
                Block(List(body), Apply(target @ Ident(lname2), Nil)),
                Literal(_))) if (target.symbol == sym) =>
          statToExpr(js.While(genExpr(cond), genStat(body)))

        // while (cond) { body }; result
        case LabelDef(lname, Nil,
            Block(List(
                If(cond,
                    Block(List(body), Apply(target @ Ident(lname2), Nil)),
                    Literal(_))),
                result)) if (target.symbol == sym) =>
          js.Block(List(js.While(genExpr(cond), genStat(body))),
              genExpr(result))

        // while (true) { body }
        case LabelDef(lname, Nil,
            Block(List(body),
                Apply(target @ Ident(lname2), Nil))) if (target.symbol == sym) =>
          statToExpr(js.While(js.BooleanLiteral(true), genStat(body)))

        // do { body } while (cond)
        case LabelDef(lname, Nil,
            Block(List(body),
                If(cond,
                    Apply(target @ Ident(lname2), Nil),
                    Literal(_)))) if (target.symbol == sym) =>
          statToExpr(js.DoWhile(genStat(body), genExpr(cond)))

        // do { body } while (cond); result
        case LabelDef(lname, Nil,
            Block(List(
                body,
                If(cond,
                    Apply(target @ Ident(lname2), Nil),
                    Literal(_))),
                result)) if (target.symbol == sym) =>
          js.Block(List(js.DoWhile(genStat(body), genExpr(cond))),
              genExpr(result))

        case _ =>
          abort("Found unknown label def at "+tree.pos+": "+tree)
      }
    }

    /** Gen JS code for a try..catch or try..finally block
     *
     *  try..finally blocks are compiled straightforwardly to try..finally
     *  blocks of JS.
     *
     *  try..catch blocks are a little more subtle, as JS does not have
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
     *
     *  TODO JavaScript-generated exceptions are not handled properly here.
     *  We should probably have a method jsExceptionToScalaException(e) in
     *  the Scala.js environment and call that as the first instruction of the
     *  catch.
     */
    def genTry(tree: Try): js.Tree = {
      implicit val jspos = tree.pos
      val Try(block, catches, finalizer) = tree

      val blockAST = genExpr(block)
      val exceptVar = js.Ident("$jsexc$")

      val handlerAST = {
        if (catches.isEmpty) {
          js.EmptyTree
        } else {
          val elseHandler: js.Tree = js.Throw(exceptVar)
          catches.foldRight(elseHandler) { (caseDef, elsep) =>
            implicit val jspos = caseDef.pos
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
              case None => genExpr(body)
              case Some(bv) =>
                js.Block(List(js.VarDef(bv, exceptVar)), genExpr(body))
            })

            // Generate the test
            if (tpe == ThrowableClass.tpe) {
              bodyWithBoundVar
            } else {
              val cond = genIsInstanceOf(ThrowableClass.tpe, tpe, exceptVar)
              js.If(cond, bodyWithBoundVar, elsep)
            }
          }
        }
      }

      val finalizerAST = genStat(finalizer) match {
        case js.Skip() => js.EmptyTree
        case ast => ast
      }

      js.Try(blockAST, exceptVar, handlerAST, finalizerAST)
    }

    /** Gen JS code for an Apply node (method call)
     *
     *  There's a whole bunch of varieties of Apply nodes: regular method
     *  calls, super calls, constructor calls, isInstanceOf/asInstanceOf,
     *  primitives, JS calls, etc. They are further dispatched in here.
     */
    def genApply(tree: Tree): js.Tree = {
      implicit val jspos = tree.pos

      tree match {
        /** isInstanceOf and asInstanceOf
         *  The two only methods that keep their type argument until the
         *  backend.
         */
        case Apply(TypeApply(fun, targs), _) =>
          val sym = fun.symbol
          val cast = sym match {
            case Object_isInstanceOf => false
            case Object_asInstanceOf => true
            case _ =>
              abort("Unexpected type application " + fun +
                  "[sym: " + sym.fullName + "]" + " in: " + tree)
          }

          val Select(obj, _) = fun
          val from = obj.tpe
          val to = targs.head.tpe
          val l = toTypeKind(from)
          val r = toTypeKind(to)
          val source = genExpr(obj)

          if (l.isValueType && r.isValueType) {
            if (cast)
              genConversion(l, r, source)
            else
              js.BooleanLiteral(l == r)
          }
          else if (l.isValueType) {
            val stat = exprToStat(source)
            val result = if (cast) {
              val ctor = ClassCastExceptionClass.info.member(
                  nme.CONSTRUCTOR).suchThat(_.tpe.params.isEmpty)
              js.Throw(genNew(ClassCastExceptionClass, ctor, Nil))
            } else {
              js.BooleanLiteral(false)
            }
            js.Block(List(stat), result)
          }
          else if (r.isValueType && cast) {
            // Erasure should have added an unboxing operation to prevent that.
            assert(false, tree)
            source
          }
          else if (r.isValueType)
            genIsInstanceOf(from, boxedClass(to.typeSymbol).tpe, source)
          else if (cast)
            genAsInstanceOf(from, to, source)
          else
            genIsInstanceOf(from, to, source)

        /** Super call of the form Class.super[mix].fun(args)
         *  This does not include calls defined in mixin traits, as these are
         *  already desugared by the 'mixin' phase. Only calls to super
         *  classes remain.
         *  Since a class has exactly one direct superclass, and calling a
         *  method two classes above the current one is invalid, I believe
         *  the `mix` item is irrelevant.
         */
        case Apply(fun @ Select(sup @ Super(_, mix), _), args) =>
          if (settings.debug.value)
            log("Call to super: " + tree)

          /* We produce a desugared JavaScript super call immediately,
           * because we might have to use the special `methodTailJumpThisSym`
           * instead of the js.This() that would be output by the JavaScript
           * desugaring.
           */
          val superCall = {
            val superClass = encodeClassSym(
                if (sup.symbol.superClass == NoSymbol) ObjectClass
                else sup.symbol.superClass)(sup.pos)
            val superProto = js.DotSelect(superClass, js.Ident("prototype")(sup.pos))(sup.pos)
            val callee = js.Select(superProto, encodeMethodSym(fun.symbol)(fun.pos))(fun.pos)
            val thisArg =
              if (methodTailJumpThisSym == NoSymbol) js.This()(sup.pos)
              else encodeLocalSym(methodTailJumpThisSym)(sup.pos)
            val arguments = thisArg :: (args map genExpr)
            js.ApplyMethod(callee, js.Ident("call"), arguments)
          }

          def isStaticModule(sym: Symbol): Boolean =
            (sym.isModuleClass && !sym.isImplClass && !sym.isLifted &&
                sym.companionModule != NoSymbol)

          // We initialize the module instance just after the super constructor
          // call.
          if (isStaticModule(currentClassSym) && !isModuleInitialized &&
              currentMethodSym.isClassConstructor) {
            isModuleInitialized = true
            val module = currentClassSym.companionModule
            val initModule = js.Assign(encodeModuleSymInternal(module), js.This())

            js.Block(List(superCall, initModule), js.This())
          } else
            superCall

        /** Constructor call (new)
         *  Further refined into:
         *  * new String(...)
         *  * new of a primitive JS type
         *  * new Array
         *  * regular new
         */
        case app @ Apply(fun @ Select(New(tpt), nme.CONSTRUCTOR), args) =>
          val ctor = fun.symbol
          if (settings.debug.value)
            assert(ctor.isClassConstructor,
                   "'new' call to non-constructor: " + ctor.name)

          if (isStringType(tpt.tpe)) {
            genNewString(app)
          } else if (isRawJSType(tpt.tpe)) {
            genPrimitiveJSNew(app)
          } else {
            val arguments = args map genExpr

            val generatedType = toTypeKind(tpt.tpe)
            if (settings.debug.value)
              assert(generatedType.isReferenceType || generatedType.isArrayType,
                   "Non reference type cannot be instantiated: " + generatedType)

            (generatedType: @unchecked) match {
              case arr @ ARRAY(elem) =>
                genNewArray(tpt.tpe, arr.dimensions, arguments)

              case rt @ REFERENCE(cls) =>
                genNew(cls, ctor, arguments)
            }
          }

        /** unbox(ApplyDynamic(...))
         *  Normally ApplyDynamic would generate a boxing operation of its
         *  result, because that is what earlier phases of the compiler
         *  expect. But then that result is often unboxed immediately.
         *  This case catches this, and short-circuit the generation of the
         *  ApplyDynamic by explicitly asking it *not* to box its result.
         */
        case Apply(fun @ _, List(dynapply:ApplyDynamic))
        if (isUnbox(fun.symbol) &&
            isBoxedForApplyDynamic(dynapply.symbol.tpe.resultType)) =>
          genApplyDynamic(dynapply, nobox = true)

        /** All other Applys, which cannot be refined by pattern matching
         *  They are further refined by properties of the method symbol.
         */
        case app @ Apply(fun, args) =>
          val sym = fun.symbol

          /** Jump to a label
           *  Most label-applys are catched upstream (while and do..while
           *  loops, jumps to next case of a pattern match), but some are
           *  still handled here:
           *  * Recursive tail call
           *  * Jump to the end of a pattern match
           */
          if (sym.isLabel) {
            /** Recursive tail call
             *  Basically this compiled into
             *  continue tailCallLoop;
             *  but arguments need to be updated beforehand.
             *
             *  Since the rhs for the new value of an argument can depend on
             *  the value of another argument (and since deciding if it is
             *  indeed the case is impossible in general), new values are
             *  computed in temporary variables first, then copied to the
             *  actual variables representing the argument.
             *
             *  Trivial assignments (arg1 = arg1) are eliminated.
             *
             *  If, after elimination of trivial assignments, only one
             *  assignment remains, then we do not use a temporary variable
             *  for this one.
             */
            if (sym == methodTailJumpLabelSym) {
              // Prepare triplets of (formalArg, tempVar, actualArg)
              // Do not include trivial assignments (when actualArg == formalArg)
              val formalArgs = methodTailJumpFormalArgs
              val actualArgs = args map genExpr
              val triplets = {
                for {
                  (formalArgSym, actualArg) <- formalArgs zip actualArgs
                  formalArg = encodeLocalSym(formalArgSym)
                  if actualArg != formalArg
                } yield {
                  (formalArg, js.Ident("temp$" + formalArg.name, None), actualArg)
                }
              }

              // The actual jump (continue tailCallLoop;)
              val tailJump = js.Continue(Some(js.Ident("tailCallLoop")))

              triplets match {
                case Nil => tailJump

                case (formalArg, _, actualArg) :: Nil =>
                  js.Block(List(js.Assign(formalArg, actualArg)), tailJump)

                case _ =>
                  val tempAssignments =
                    for ((_, tempArg, actualArg) <- triplets)
                      yield js.Assign(tempArg, actualArg)
                  val trueAssignments =
                    for ((formalArg, tempArg, _) <- triplets)
                      yield js.Assign(formalArg, tempArg)
                  js.Block(tempAssignments ::: trueAssignments, tailJump)
              }
            } else // continues after the comment
            /** Jump the to the end-label of a pattern match
             *  Such labels have exactly one argument, which is the result of
             *  the pattern match (of type Unit if the match is in statement
             *  position).
             *
             *  If non-unit, the result of the pattern match is stored in the
             *  dedicated synthetic variable. Otherwise it is emitted as a
             *  statement (it could have side-effects).
             *
             *  Then we jump at the end of the match by `break`ing the labelled
             *  loop surrounding the match.
             */
            if (sym.name.toString() startsWith "matchEnd") {
              val isResultUnit = toTypeKind(sym.info.resultType) == UNDEFINED
              val labelIdent = encodeLabelSym(sym)
              val jumpStat = js.Break(Some(labelIdent))
              val List(matchResult) = args

              if (isResultUnit) {
                js.Block(List(genStat(matchResult)), jumpStat)
              } else {
                val resultVar = js.Ident("result$"+labelIdent.name, None)
                js.Block(List(js.Assign(resultVar, genExpr(matchResult)), jumpStat))
              }
            } else {
              /* No other label apply should ever happen. If it does, then we
               * have missed a pattern of LabelDef/LabelApply and some new
               * translation must be found for it.
               */
              abort("Found unknown label apply at "+tree.pos+": "+tree)
            }
          } else // continues after the comment
          /** Primitive method whose code is generated by the codegen */
          if (isPrimitive(sym)) {
            // primitive operation
            genPrimitiveOp(app)
          } else if (isBox(sym)) {
            /** Box a primitive value */
            val arg = args.head
            makeBox(genExpr(arg), arg.tpe)
          } else if (isUnbox(sym)) {
            /** Unbox a primitive value */
            val arg = args.head
            makeUnbox(genExpr(arg), tree.tpe)
          } else {
            /** Actual method call
             *  But even these are further refined into:
             *  * Methods of java.lang.Object (because things typed as such
             *    at compile-time are sometimes raw JS values at runtime).
             *  * Calls to primitive JS methods (Scala.js -> JS bridge)
             *  * Regular method call
             */
            if (settings.debug.value)
              log("Gen CALL_METHOD with sym: " + sym + " isStaticSymbol: " + sym.isStaticMember);

            val Select(receiver, _) = fun

            if (fun.symbol == Object_toString) {
              js.ApplyMethod(genExpr(receiver), js.Ident("toString"), Nil)
            } else if (ObjectMemberMethodToHelperMethodName contains fun.symbol) {
              val helper = ObjectMemberMethodToHelperMethodName(fun.symbol)
              val arguments = (receiver :: args) map genExpr
              genBuiltinApply(helper, arguments:_*)
            } else if (isRawJSType(receiver.tpe)) {
              genPrimitiveJSCall(app)
            } else {
              val instance = genExpr(receiver)
              val arguments = args map genExpr

              js.ApplyMethod(instance, encodeMethodSym(fun.symbol), arguments)
            }
          }
      }
    }

    // TODO Make these primitives?
    lazy val ObjectMemberMethodToHelperMethodName = Map[Symbol, String](
      Object_getClass  -> "objectGetClass",
      Object_clone     -> "objectClone",
      Object_finalize  -> "objectFinalize",
      Object_notify    -> "objectNotify",
      Object_notifyAll -> "objectNotifyAll",
      Object_equals    -> "objectEquals",
      Object_hashCode  -> "objectHashCode"
    )

    /** Gen JS code for a conversion between primitive value types */
    def genConversion(from: TypeKind, to: TypeKind, value: js.Tree)(
        implicit pos: Position): js.Tree = {
      def int0 = js.IntLiteral(0)
      def int1 = js.IntLiteral(1)
      def float0 = js.DoubleLiteral(0.0)
      def float1 = js.DoubleLiteral(1.0)

      (from, to) match {
        case (_:INT, BOOL) => js.BinaryOp("!=", value, int0)
        case (_:FLOAT, BOOL) => js.BinaryOp("!=", value, float0)

        case (BOOL, _:INT) => js.If(value, int1, int0)
        case (BOOL, _:FLOAT) => js.If(value, float1, float0)

        // TODO Isn't float-to-int missing?

        case _ => value
      }
    }

    /** Gen JS code for an isInstanceOf test (for reference types only) */
    def genIsInstanceOf(from: Type, to: Type, value: js.Tree)(
        implicit pos: Position = value.pos): js.Tree = {
      if (isStringType(to)) {
        js.BinaryOp("===", js.UnaryOp("typeof", value),
            js.StringLiteral("string"))
      } else if (isRawJSType(to)) {
        // isInstanceOf is not supported for raw JavaScript types
        abort("isInstanceOf["+to+"]")
      } else {
        genBuiltinApply("isInstance", value, encodeFullNameLit(to))
      }
    }

    /** Gen JS code for an asInstanceOf cast (for reference types only) */
    def genAsInstanceOf(from: Type, to: Type, value: js.Tree)(
        implicit pos: Position = value.pos): js.Tree = {
      if (isStringType(to)) {
        genBuiltinApply("asInstanceString", value)
      } else if (isRawJSType(to)) {
        // asInstanceOf on JavaScript is completely erased
        value
      } else {
        genBuiltinApply("asInstance", value, encodeFullNameLit(to))
      }
    }

    /** Gen JS code for a call to a Scala class constructor
     *  Because the actual JS constructor of classes is the JS bridge for the
     *  constructors, we bypass it using Object.create.
     *  We call the <init> method representing the constructor on the resulting
     *  instance. Since this method returns `this`, we simply chain the calls.
     */
    def genNew(clazz: Symbol, ctor: Symbol, arguments: List[js.Tree])(
        implicit pos: Position): js.Tree = {
      val typeVar = encodeClassSym(clazz)
      val instance = js.New(typeVar, Nil)
      js.Apply(js.Select(instance, encodeMethodSym(ctor)), arguments)
    }

    /** Gen JS code for creating a new Array: new Array[T](length)
     *  For multidimensional arrays (dimensions > 1), the arguments can
     *  specify up to `dimensions` lengths for the first dimensions of the
     *  array.
     */
    def genNewArray(arrayType: Type, dimensions: Int,
        arguments: List[js.Tree])(implicit pos: Position): js.Tree = {
      val argsLength = arguments.length

      if (argsLength > dimensions)
        abort("too many arguments for array constructor: found " + argsLength +
          " but array has only " + dimensions + " dimension(s)")

      val arrayClassData = encodeClassDataOfType(arrayType)

      genBuiltinApply("newArrayObject", arrayClassData,
          js.ArrayConstr(arguments))
    }

    /** Gen JS code for an array literal
     *  We generate a JS array construction that we wrap in a native array
     *  wrapper.
     */
    def genArrayValue(tree: Tree): js.Tree = {
      implicit val pos = tree.pos
      val ArrayValue(tpt @ TypeTree(), elems) = tree

      val arrayClassData = encodeClassDataOfType(tree.tpe)
      val nativeArray = js.ArrayConstr(elems map genExpr)

      genBuiltinApply("makeNativeArrayWrapper",
          arrayClassData, nativeArray)
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
    def genMatch(tree: Tree): js.Tree = {
      implicit val pos = tree.pos
      val Match(selector, cases) = tree

      val expr = genExpr(selector)

      val List(defaultBody0) = for {
        CaseDef(Ident(nme.WILDCARD), EmptyTree, body) <- cases
      } yield body

      val (defaultBody, defaultLabelSym) = defaultBody0 match {
        case LabelDef(_, Nil, rhs) if hasSynthCaseSymbol(defaultBody0) =>
          (rhs, defaultBody0.symbol)
        case _ =>
          (defaultBody0, NoSymbol)
      }

      var clauses: List[(List[js.Tree], js.Tree)] = Nil
      var elseClause: js.Tree = js.EmptyTree

      for (caze @ CaseDef(pat, guard, body) <- cases) {
        assert(guard == EmptyTree)

        def genBody() = body match {
          // Yes, this will duplicate the default body in the output
          case If(cond, thenp, app @ Apply(_, Nil)) if app.symbol == defaultLabelSym =>
            js.If(genExpr(cond), genExpr(thenp), genExpr(defaultBody))(body.pos)
          case If(cond, thenp, Block(List(app @ Apply(_, Nil)), _)) if app.symbol == defaultLabelSym =>
            js.If(genExpr(cond), genExpr(thenp), genExpr(defaultBody))(body.pos)

          case _ =>
            genExpr(body)
        }

        pat match {
          case lit: Literal =>
            clauses = (List(genExpr(lit)), genBody()) :: clauses
          case Ident(nme.WILDCARD) =>
            elseClause = genExpr(defaultBody)
          case Alternative(alts) =>
            val genAlts = {
              alts map {
                case lit: Literal => genExpr(lit)
                case _ =>
                  abort("Invalid case in alternative in switch-like pattern match: " +
                      tree + " at: " + tree.pos)
              }
            }
            clauses = (genAlts, genBody()) :: clauses
          case _ =>
            abort("Invalid case statement in switch-like pattern match: " +
                tree + " at: " + (tree.pos))
        }
      }

      js.Match(expr, clauses.reverse, elseClause)
    }

    /** Gen JS code for a translated match
     *
     *  This implementation relies heavily on the patterns of trees emitted
     *  by the current pattern match phase (as of Scala 2.10).
     *
     *  For matches in expression position (i.e., whose types is not Unit), a
     *  synthetic variable is generated that will contain the result of the
     *  match. Think: a tiny transformation phase that transforms
     *    arbiter match {
     *      case Case1 => expr
     *      ...
     *    } // of type T
     *  into
     *    {
     *      var matchResult: T = _
     *      arbiter match {
     *        case Case1 => matchResult = expr
     *        ...
     *      } // of type Unit
     *      matchResult
     *    }
     *  We do this because it is hard to give meaning to jumps within an
     *  expression context when translating them in JS (even our extended
     *  JS which supports complex constructs in expression position).
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
     *        If(_, <case-body>, <jump>)
     *      | Block(_, <case-body>)
     *      | _
     *    These restrictions, together with the fact that we are in statement
     *    position (thanks to the above transformation), mean that they can be
     *    simply replaced by `skip`.
     *
     *  To implement jumps to `matchEnd`, we enclose all the cases in one big
     *  labeled do..while(false) loop. Jumps are then compiled as `break`s out
     *  of that loop.
     */
    def genTranslatedMatch(cases: List[LabelDef],
        matchEnd: LabelDef)(implicit pos: Position): js.Tree = {

      val isResultUnit = toTypeKind(matchEnd.tpe) == UNDEFINED
      val resultVar =
        js.Ident("result$"+encodeLabelSym(matchEnd.symbol).name, None)

      val nextCaseSyms = (cases.tail map (_.symbol)) :+ NoSymbol

      val translatedCases = for {
        (LabelDef(_, Nil, rhs), nextCaseSym) <- cases zip nextCaseSyms
      } yield {
        def genCaseBody(tree: Tree): js.Tree = {
          implicit val pos = tree.pos
          tree match {
            case If(cond, thenp, app @ Apply(_, Nil)) if app.symbol == nextCaseSym =>
              js.If(genExpr(cond), genCaseBody(thenp), js.Skip())

            case Block(stats, expr) =>
              js.Block(stats map genStat, genCaseBody(expr))

            case _ =>
              if (isResultUnit) genStat(tree)
              else js.Assign(resultVar, genExpr(tree))
          }
        }

        genCaseBody(rhs)
      }

      val matchLoop = js.DoWhile(js.Block(translatedCases),
          js.BooleanLiteral(false), Some(encodeLabelSym(matchEnd.symbol)))

      if (isResultUnit) {
        statToExpr(matchLoop)
      } else {
        js.Block(List(js.VarDef(resultVar, js.EmptyTree), matchLoop), resultVar)
      }
    }

    /** Test whether the given method is a primitive from Scala.js point of view */
    private def isPrimitive(sym: Symbol) = {
      // TODO Is (sym ne String_+) still useful here?
      (scalaPrimitives.isPrimitive(sym) && (sym ne String_+))
    }

    /** Gen JS code for a primitive method call */
    private def genPrimitiveOp(tree: Apply): js.Tree = {
      import scalaPrimitives._

      implicit val jspos = tree.pos

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
        genSynchronized(tree)
      else if (isCoercion(code))
        genCoercion(tree, receiver, code)
      else if (jsPrimitives.isJavaScriptPrimitive(code))
        genJSPrimitive(tree, receiver, args, code)
      else
        abort("Primitive operation not handled yet: " + sym.fullName + "(" +
            fun.symbol.simpleName + ") " + " at: " + (tree.pos))
    }

    /** Gen JS code for a simple operation (arithmetic, logical, or comparison) */
    private def genSimpleOp(tree: Apply, args: List[Tree], code: Int): js.Tree = {
      import scalaPrimitives._

      implicit val jspos = tree.pos

      val sources = args map genExpr

      sources match {
        // Unary operation
        case List(source) =>
          (code match {
            case POS =>
              js.UnaryOp("+", source)
            case NEG =>
              js.UnaryOp("-", source)
            case NOT =>
              js.UnaryOp("~", source)
            case ZNOT =>
              js.UnaryOp("!", source)
            case _ =>
              abort("Unknown unary operation code: " + code)
          })

        // Binary operation
        case List(lsrc, rsrc) =>
          lazy val leftKind = toTypeKind(args.head.tpe)

          def genEquality(eqeq: Boolean, not: Boolean) = {
            if (eqeq && leftKind.isReferenceType &&
                !isRawJSType(args(0).tpe) && !isRawJSType(args(1).tpe)) {
              val body = genEqEqPrimitive(args(0), args(1), lsrc, rsrc)
              if (not) js.UnaryOp("!", body) else body
            } else
              js.BinaryOp(if (not) "!==" else "===", lsrc, rsrc)
          }

          (code match {
            case ADD => js.BinaryOp("+", lsrc, rsrc)
            case SUB => js.BinaryOp("-", lsrc, rsrc)
            case MUL => js.BinaryOp("*", lsrc, rsrc)
            case DIV =>
              val actualDiv = js.BinaryOp("/", lsrc, rsrc)
              (leftKind: @unchecked) match {
                case LongKind => genBuiltinApply("truncateToLong", actualDiv)
                case _:INT => js.BinaryOp("|", actualDiv, js.IntLiteral(0))
                case _:FLOAT => actualDiv
              }
            case MOD => js.BinaryOp("%", lsrc, rsrc)
            case OR => js.BinaryOp("|", lsrc, rsrc)
            case XOR => js.BinaryOp("^", lsrc, rsrc)
            case AND => js.BinaryOp("&", lsrc, rsrc)
            case LSL => js.BinaryOp("<<", lsrc, rsrc)
            case LSR => js.BinaryOp(">>>", lsrc, rsrc)
            case ASR => js.BinaryOp(">>", lsrc, rsrc)
            case LT => js.BinaryOp("<", lsrc, rsrc)
            case LE => js.BinaryOp("<=", lsrc, rsrc)
            case GT => js.BinaryOp(">", lsrc, rsrc)
            case GE => js.BinaryOp(">=", lsrc, rsrc)
            case EQ => genEquality(eqeq = true, not = false)
            case NE => genEquality(eqeq = true, not = true)
            case ID => genEquality(eqeq = false, not = false)
            case NI => genEquality(eqeq = false, not = true)
            case ZOR => js.BinaryOp("||", lsrc, rsrc)
            case ZAND => js.BinaryOp("&&", lsrc, rsrc)
            case _ =>
              abort("Unknown binary operation code: " + code)
          })

        case _ =>
          abort("Too many arguments for primitive function: " + tree)
      }
    }

    /** Gen JS code for a call to Any.== */
    def genEqEqPrimitive(l: Tree, r: Tree, lsrc: js.Tree, rsrc: js.Tree)(
        implicit pos: Position): js.Tree = {
      /** True if the equality comparison is between values that require the use of the rich equality
        * comparator (scala.runtime.Comparator.equals). This is the case when either side of the
        * comparison might have a run-time type subtype of java.lang.Number or java.lang.Character.
        * When it is statically known that both sides are equal and subtypes of Number of Character,
        * not using the rich equality is possible (their own equals method will do ok.)*/
      def mustUseAnyComparator: Boolean = {
        def areSameFinals = l.tpe.isFinalType && r.tpe.isFinalType && (l.tpe =:= r.tpe)
        !areSameFinals && isMaybeBoxed(l.tpe.typeSymbol) && isMaybeBoxed(r.tpe.typeSymbol)
      }

      val function = if (mustUseAnyComparator) "anyEqEq" else "anyRefEqEq"
      genBuiltinApply(function, lsrc, rsrc)
    }

    /** Gen JS code for string concatenation
     *  We explicitly call the JS toString() on any non-String argument to
     *  avoid the weird things happening when adding "things" in JS.
     */
    private def genStringConcat(tree: Apply, receiver: Tree, args: List[Tree]): js.Tree = {
      implicit val pos = tree.pos

      val List(lhs, rhs) = for {
        op <- receiver :: args
      } yield {
        if (isStringType(op.tpe)) genExpr(op)
        else js.ApplyMethod(genExpr(op), js.Ident("toString"), Nil)
      }

      js.BinaryOp("+", lhs, rhs)
    }

    /** Gen JS code for a call to Any.## */
    private def genScalaHash(tree: Apply, receiver: Tree): js.Tree = {
      implicit val jspos = tree.pos

      val instance = genLoadModule(ScalaRunTimeModule)
      val arguments = List(genExpr(receiver))
      val sym = getMember(ScalaRunTimeModule, stringToTermName("hash"))

      js.ApplyMethod(instance, encodeMethodSym(sym), arguments)
    }

    /** Gen JS code for an array operation (get, set or length) */
    private def genArrayOp(tree: Tree, code: Int): js.Tree = {
      import scalaPrimitives._

      implicit val pos = tree.pos

      val Apply(Select(arrayObj, _), args) = tree
      val arrayValue = js.DotSelect(genExpr(arrayObj), js.Ident("underlying"))
      val arguments = args map genExpr

      if (scalaPrimitives.isArrayGet(code)) {
        // get an item of the array
        if (settings.debug.value)
          assert(args.length == 1,
              s"Array get requires 1 argument, found ${args.length} in $tree")

        js.BracketSelect(arrayValue, arguments(0))
      } else if (scalaPrimitives.isArraySet(code)) {
        // set an item of the array
        if (settings.debug.value)
          assert(args.length == 2,
              s"Array set requires 2 arguments, found ${args.length} in $tree")

        statToExpr {
          js.Assign(js.BracketSelect(arrayValue, arguments(0)), arguments(1))
        }
      } else {
        // length of the array
        js.DotSelect(arrayValue, js.Ident("length"))
      }
    }

    /** Gen JS code for a call to AnyRef.synchronized */
    private def genSynchronized(tree: Apply): js.Tree = {
      /* JavaScript is single-threaded. I believe we can drop the
       * synchronization altogether.
       */
      genExpr(tree.args.head)
    }

    /** Gen JS code for a coercion */
    private def genCoercion(tree: Apply, receiver: Tree, code: Int): js.Tree = {
      import scalaPrimitives._

      implicit val jspos = tree.pos

      val source = genExpr(receiver)

      (code: @scala.annotation.switch) match {
        case B2F | B2D | S2F | S2D | C2F | C2D | I2F | I2D | L2F | L2D =>
          source

        case F2B | F2S | F2C | F2I | D2B | D2S | D2C | D2I =>
          js.BinaryOp("|", source, js.IntLiteral(0))

        case F2L | D2L =>
          genBuiltinApply("truncateToLong", source)

        case _ => source
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
     *
     *  Otherwise, this is just a regular method call, because JS is dynamic
     *  anyway.
     */
    private def genApplyDynamic(tree: ApplyDynamic,
        nobox: Boolean = false): js.Tree = {

      implicit val pos = tree.pos

      val sym = tree.symbol
      val ApplyDynamic(receiver, args) = tree

      val instance = genExpr(receiver)

      val arguments = args zip sym.tpe.params map { case (arg, param) =>
        if (isBoxedForApplyDynamic(param.tpe)) {
          arg match {
            case Apply(_, List(result)) if isBox(arg.symbol) => genExpr(result)
            case _ => makeUnbox(genExpr(arg), param.tpe)
          }
        } else {
          genExpr(arg)
        }
      }

      val apply = js.ApplyMethod(instance, encodeMethodSym(sym), arguments)

      if (nobox || !isBoxedForApplyDynamic(sym.tpe.resultType))
        apply
      else
        makeBox(apply, sym.tpe.resultType)
    }

    /** Test whether the given type is artificially boxed for ApplyDynamic */
    private def isBoxedForApplyDynamic(tpe: Type) =
      tpe.typeSymbol.isPrimitiveValueClass

    /** Gen a boxing operation (tpe is the primitive type) */
    private def makeBox(expr: js.Tree, tpe: Type)(
        implicit pos: Position): js.Tree =
      makeBoxUnbox(expr, tpe, "b")

    /** Gen an unboxing operation (tpe is the primitive type) */
    private def makeUnbox(expr: js.Tree, tpe: Type)(
        implicit pos: Position): js.Tree =
      makeBoxUnbox(expr, tpe, "u")

    /** Common implementation for `makeBox()` and `makeUnbox()` */
    private def makeBoxUnbox(expr: js.Tree, tpe: Type, functionPrefix: String)(
        implicit pos: Position): js.Tree = {

      val boxFunName = toTypeKind(tpe) match {
        case kind: ValueTypeKind => functionPrefix + kind.primitiveCharCode
        case _ =>
          abort(s"makeBoxUnbox requires a primitive type, found $tpe at $pos")
      }
      genBuiltinApply(boxFunName, expr)
    }

    /** Gen JS code for a Scala.js-specific primitive method */
    private def genJSPrimitive(tree: Apply, receiver0: Tree,
        args: List[Tree], code: Int): js.Tree = {
      import jsPrimitives._

      implicit val pos = tree.pos

      def receiver = genExpr(receiver0)
      val genArgs = genPrimitiveJSArgs(tree.symbol, args)

      if (code == DYNAPPLY) {
        // js.Dynamic.applyDynamic(methodName)(actualArgs:_*)
        val methodName :: actualArgs = genArgs
        js.DynamicApplyMethod(receiver, methodName, actualArgs)
      } else if (code == ARR_CREATE) {
        // js.Array.create(elements:_*)
        js.ArrayConstr(genArgs)
      } else (genArgs match {
        case Nil =>
          code match {
            case GETGLOBAL => envField("g")
          }

        case List(arg) =>
          code match {
            case V2JS => js.Undefined()
            case Z2JS => arg
            case N2JS => arg
            case S2JS => arg

            /** Convert a scala.FunctionN f to a js.FunctionN
             *  Basically it binds the appropriate `apply` method of f to f.
             *  (function($this) {
             *    return function(args...) {
             *      return $this["the right apply(...)"](args...);
             *    }
             *  })(f);
             *
             *  TODO Use the JS function Function.prototype.bind()?
             */
            case F2JS =>
              val inputTpe = args.head.tpe
              val applyMeth = getMemberMethod(inputTpe.typeSymbol,
                  newTermName("apply"))
              val arity = applyMeth.tpe.params.size
              val theFunction = js.Ident("$this")
              val arguments = (1 to arity).toList map (x => js.Ident("arg"+x))
              js.captureWithin(theFunction, arg) {
                js.Function(arguments, {
                  js.Return(js.ApplyMethod(theFunction,
                      encodeMethodSym(applyMeth), arguments))
                })
              }

            case JS2Z => arg
            case JS2N => arg
            case JS2S => arg

            case ANY2DYN => arg

            case DYNSELECT =>
              // js.Dynamic.selectDynamic(arg)
              js.DynamicSelect(receiver, arg)
            case DICT_SELECT =>
              // js.Dictionary.apply(arg)
              js.BracketSelect(receiver, arg)
          }

        case List(arg1, arg2) =>
          code match {
            case DYNUPDATE =>
              // js.Dynamic.updateDynamic(arg1)(arg2)
              statToExpr(js.Assign(js.DynamicSelect(receiver, arg1), arg2))
            case DICT_UPDATE =>
              // js.Dictionary.update(arg1, arg2)
              statToExpr(js.Assign(js.BracketSelect(receiver, arg1), arg2))
          }
      })
    }

    /** Gen JS code for a primitive JS call (to a method of a subclass of js.Any)
     *  This is the typed Scala.js to JS bridge feature. Basically it boils
     *  down to calling the method without name mangling. But other aspects
     *  come into play:
     *  * Operator methods are translated to JS operators (not method calls)
     *  * apply is translated as a function call, i.e. o() instead of o.apply()
     *  * Some methods of java.lang.String are given an alternative name
     *    (TODO: consider them as primitives instead?)
     *  * Scala varargs are turned into JS varargs (see genPrimitiveJSArgs())
     *  * Getters and parameterless methods are translated as Selects
     *  * Setters are translated to Assigns of Selects
     */
    private def genPrimitiveJSCall(tree: Apply): js.Tree = {
      implicit val pos = tree.pos

      val sym = tree.symbol
      val Apply(fun @ Select(receiver0, _), args0) = tree

      val funName = sym.nameString
      val receiver = genExpr(receiver0)
      val args = genPrimitiveJSArgs(sym, args0)
      val argc = args.length

      val isString = isStringType(receiver0.tpe)

      funName match {
        case "unary_+" | "unary_-" | "unary_~" | "unary_!" =>
          assert(argc == 0)
          js.UnaryOp(funName.substring(funName.length-1), receiver)

        case "+" | "-" | "*" | "/" | "%" | "<<" | ">>" | ">>>" |
             "&" | "|" | "^" | "&&" | "||" =>
          assert(argc == 1)
          js.BinaryOp(funName, receiver, args.head)

        case "apply" =>
          js.Apply(receiver, args)

        case "charAt" | "codePointAt" if isString =>
          js.ApplyMethod(receiver, js.Ident("charCodeAt"), args)

        case "length" if isString =>
          js.DotSelect(receiver, js.Ident("length"))

        case _ =>
          def wasNullaryMethod(sym: Symbol) = {
            beforePhase(currentRun.uncurryPhase) {
              sym.tpe.isInstanceOf[NullaryMethodType]
            }
          }

          if (argc == 0 && (sym.isGetter || wasNullaryMethod(sym))) {
            js.Select(receiver, js.PropertyName(funName))
          } else if (argc == 1 && sym.isSetter) {
            statToExpr(js.Assign(
                js.Select(receiver,
                    js.PropertyName(funName.substring(0, funName.length-2))),
                args.head))
          } else {
            js.ApplyMethod(receiver, js.PropertyName(funName), args)
          }
      }
    }

    /** Gen JS code for new java.lang.String(...)
     *  TODO Currently only new String() and new String(String) are implemented
     */
    private def genNewString(tree: Apply): js.Tree = {
      implicit val pos = tree.pos
      val Apply(fun @ Select(_, _), args0) = tree

      val ctor = fun.symbol
      val args = genPrimitiveJSArgs(ctor, args0)

      (args0 map (_.tpe)) match {
        case Nil => js.StringLiteral("")
        case List(tpe) if isStringType(tpe) => args.head
        case _ =>
          // TODO
          js.Throw(js.StringLiteral("new String() not implemented"))
      }
    }

    /** Gen JS code for a new of a JS class (subclass of js.Any) */
    private def genPrimitiveJSNew(tree: Apply): js.Tree = {
      implicit val pos = tree.pos

      val Apply(fun @ Select(New(tpt), _), args0) = tree
      val cls = tpt.tpe.typeSymbol
      val ctor = fun.symbol

      val args = genPrimitiveJSArgs(ctor, args0)

      if (cls == JSObjectClass && args.isEmpty) js.ObjectConstr(Nil)
      else js.New(genPrimitiveJSClass(cls), args)
    }

    /** Gen JS code representing a JS class (subclass of js.Any) */
    private def genPrimitiveJSClass(sym: Symbol)(
        implicit pos: Position): js.Tree = {
      /* TODO Improve this, so that the JS name is not bound to the Scala name
       * (idea: annotation on the class? optional annot?) */
      val className = js.Ident(sym.nameString)
      genSelectInGlobalScope(className)
    }

    /** Gen JS code representing a JS module (var of the global scope) */
    private def genPrimitiveJSModule(sym: Symbol)(
        implicit pos: Position): js.Tree = {
      /* TODO Improve this, so that the JS name is not bound to the Scala name
       * (idea: annotation on the class? optional annot?) */
      val moduleName = js.Ident(sym.nameString)
      genSelectInGlobalScope(moduleName)
    }

    /** Gen JS code selecting a field of the global scope */
    private def genSelectInGlobalScope(property: js.PropertyName)(
        implicit pos: Position): js.Tree = {
      js.Select(envField("g"), property)
    }

    /** Gen actual actual arguments to a primitive JS call
     *  This handles repeated arguments (varargs) by turning them into
     *  JS varargs, i.e., by expanding them into normal arguments.
     */
    private def genPrimitiveJSArgs(sym: Symbol, args: List[Tree]): List[js.Tree] = {
      val wereRepeated = afterPhase(currentRun.typerPhase) {
        for {
          params <- sym.tpe.paramss
          param <- params
        } yield isScalaRepeatedParamType(param.tpe)
      }

      args zip wereRepeated flatMap {
        case (arg, wasRepeated) =>
          if (wasRepeated) genPrimitiveJSRepeatedParam(arg)
          else List(genExpr(arg))
      }
    }

    /** Expand the elements of an actual repeated argument */
    private def genPrimitiveJSRepeatedParam(arg: Tree): List[js.Tree] = {
      arg match {
        case Apply(wrapRefArray_?, List(
            Apply(TypeApply(asInstanceOf_? @ Select(
                ArrayValue(tpt, elems), _), _), _)))
        if (wrapRefArray_?.symbol == Predef_wrapRefArray) &&
            (asInstanceOf_?.symbol == Object_asInstanceOf) =>
          elems map genExpr

        case Select(_, _) if arg.symbol == NilModule =>
          Nil

        case _ =>
          // TODO This can be supported using Function.prototype.apply
          abort("Passing a seq:_* to a JavaScript method is not supported: "+arg)
      }
    }

    /** Generate a literal "zero" for the requested type */
    def genZeroOf(tpe: Type)(implicit pos: Position): js.Tree = toTypeKind(tpe) match {
      case UNDEFINED => js.Undefined()
      case BOOL => js.BooleanLiteral(false)
      case INT(_) => js.IntLiteral(0)
      case FLOAT(_) => js.DoubleLiteral(0.0)
      case REFERENCE(_) => js.Null()
      case ARRAY(_) => js.Null()
    }

    /** Generate loading of a module value */
    private def genLoadModule(sym0: Symbol)(implicit pos: Position): js.Tree = {
      /* Sometimes we receive a module class here.
       * We must select its companion module if it exists.
       */
      val sym =
        if (sym0.isModuleClass && sym0.companionModule != NoSymbol) sym0.companionModule
        else sym0

      val isGlobalScope =
        isScalaJSDefined &&
        sym.isModuleOrModuleClass && // TODO Why? Is this ever false?
        beforePhase(currentRun.erasurePhase) {
          sym.tpe.typeSymbol isSubClass JSGlobalScopeClass
        }

      if (isGlobalScope) envField("g")
      else if (isRawJSType(sym.tpe)) genPrimitiveJSModule(sym)
      else encodeModuleSym(sym)
    }

    /** Generate access to a static member */
    private def genStaticMember(sym: Symbol)(implicit pos: Position) = {
      /* Actually, there is no static member in Scala.js. If we come here, that
       * is because we found the symbol in a Java-emitted .class in the
       * classpath. But the corresponding implementation in Scala.js will
       * actually be a val in the companion module.
       * So we cheat here. This is a workaround for not having separate
       * compilation yet.
       */
      val instance = genLoadModule(sym.owner)
      val method = encodeStaticMemberSym(sym)
      js.ApplyMethod(instance, method, Nil)
    }

    /** Generate a Class[_] value (e.g. coming from classOf[T]) */
    private def genClassConstant(tpe: Type)(implicit pos: Position): js.Tree = {
      encodeClassOfType(tpe)
    }

    /** Generate a call to a runtime builtin helper */
    def genBuiltinApply(funName: String, args: js.Tree*)(implicit pos: Position) = {
      js.ApplyMethod(environment, js.Ident(funName), args.toList)
    }
  }

  /** Test whether the given type represents a raw JavaScript type
   *
   *  I.e., test whether the type extends scala.js.Any
   */
  def isRawJSType(tpe: Type): Boolean = {
    isStringType(tpe) ||
    (isScalaJSDefined && beforePhase(currentRun.erasurePhase) {
      tpe.typeSymbol isSubClass JSAnyClass
    })
  }

  private def isStringType(tpe: Type): Boolean =
    tpe.typeSymbol == StringClass
}
