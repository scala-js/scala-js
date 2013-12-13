/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import scala.tools.nsc._

/** Generate JavaScript code and output it to disk
 *
 *  @author Sébastien Doeraene
 */
abstract class GenJSCode extends plugins.PluginComponent
                            with TypeKinds
                            with JSEncoding
                            with JSBridges
                            with JSDesugaring
                            with GenJSFiles {
  val jsAddons: JSGlobalAddons {
    val global: GenJSCode.this.global.type
  }

  import global._
  import jsAddons._
  import rootMirror._
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

    // Fresh local name generator ----------------------------------------------

    val usedLocalNames = mutable.Set.empty[String]
    val localSymbolNames = mutable.Map.empty[Symbol, String]
    private val isKeywordOrReserved =
      js.isKeyword ++ Seq("arguments", ScalaJSEnvironmentName)

    def freshName(base: String = "x"): String = {
      var suffix = 1
      var longName = base
      while (usedLocalNames(longName) || isKeywordOrReserved(longName)) {
        suffix += 1
        longName = base+"$"+suffix
      }
      usedLocalNames += longName
      longName
    }

    def freshName(sym: Symbol): String = {
      localSymbolNames.getOrElseUpdate(sym, {
        freshName(sym.name.toString)
      })
    }

    // Top-level apply ---------------------------------------------------------

    override def run() {
      scalaPrimitives.init()
      jsPrimitives.init()
      super.run()
    }

    /** Generate JS code for a compilation unit
     *  This method iterates over all the class and interface definitions
     *  found in the compilation unit and emits their code (.js) and type
     *  definitions (.jstype).
     *
     *  Classes representing primitive types, as well as the scala.Array
     *  class, are not actually emitted.
     *
     *  Other ClassDefs are emitted according to their nature:
     *  * Interface               -> `genInterface()`
     *  * Implementation class    -> `genImplClass()`
     *  * Raw JS type (<: js.Any) -> `genRawJSClassData()`
     *  * Normal class            -> `genClass()`
     *                               + `genModuleAccessor()` if module class
     *
     *  The resulting tree is desugared with `JSDesugaring`, and then sent to
     *  disc with `GenJSFiles`.
     *
     *  Type definitions (i.e., pickles) for top-level representatives are also
     *  emitted.
     */
    override def apply(cunit: CompilationUnit) {
      try {
        currentCUnit = cunit

        val generatedClasses = ListBuffer.empty[(Symbol, js.Tree)]

        def gen(tree: Tree) {
          tree match {
            case EmptyTree => ()
            case PackageDef(_, stats) => stats foreach gen
            case cd: ClassDef =>
              implicit val pos = tree.pos
              val sym = cd.symbol

              /* Do not actually emit code for primitive types nor scala.Array.
               */
              val isPrimitive =
                isPrimitiveValueClass(sym) || (sym == ArrayClass)

              if (!isPrimitive) {
                val tree = if (sym.isInterface) {
                  genInterface(cd)
                } else if (sym.isImplClass) {
                  genImplClass(cd)
                } else if (isRawJSType(sym.tpe)) {
                  genRawJSClassData(cd)
                } else {
                  val classDef = genClass(cd)
                  if (isStaticModule(sym))
                    js.Block(classDef, genModuleAccessor(sym))
                  else
                    classDef
                }
                generatedClasses += sym -> tree
              }
          }
        }

        gen(cunit.body)

        for ((sym, tree) <- generatedClasses) {
          val desugared = desugarJavaScript(tree)
          genJSFile(cunit, sym, desugared)
        }
      } finally {
        currentCUnit = null
        currentClassSym = null
        currentMethodSym = null
      }
    }

    // Generate a class --------------------------------------------------------

    /** Gen JS code for a class definition (maybe a module class)
     *  It emits:
     *  * An ES6 class declaration with:
     *    - A constructor creating all the fields (ValDefs)
     *    - Methods (DefDefs), including the Scala constructor
     *    - JS-friendly bridges for all public methods (with non-mangled names)
     *  * An inheritable constructor, used to create the prototype of subclasses
     *  * A JS-friendly constructor bridge, if there is a public constructor
     *  * Functions for instance tests
     *  * The class data record
     */
    def genClass(cd: ClassDef): js.Tree = {
      import js.TreeDSL._

      implicit val pos = cd.pos
      val ClassDef(mods, name, _, impl) = cd
      val sym = cd.symbol
      currentClassSym = sym

      assert(!sym.isInterface && !sym.isImplClass,
          "genClass() must be called only for normal classes: "+sym)
      assert(sym.superClass != NoSymbol, sym)

      val classIdent = encodeClassFullNameIdent(sym)
      val classVar = envField("c") DOT classIdent

      // Generate members (constructor + methods)

      val generatedMembers = new ListBuffer[js.Tree]

      generatedMembers += genConstructor(cd)

      def gen(tree: Tree) {
        tree match {
          case EmptyTree => ()
          case Template(_, _, body) => body foreach gen

          case ValDef(mods, name, tpt, rhs) =>
            () // fields are added in the constructor (genConstructor(cd))

          case dd: DefDef =>
            generatedMembers ++= genMethod(dd)

          case _ => abort("Illegal tree in gen of genClass(): " + tree)
        }
      }

      gen(impl)

      // Generate the bridges, then steal the constructor bridges (1 at most)
      val bridges0 = genBridgesForClass(sym)
      val (constructorBridges0, bridges) = bridges0.partition {
        case js.MethodDef(js.Ident("init_", _), _, _) => true
        case _ => false
      }
      assert(constructorBridges0.size <= 1)
      val constructorBridge = constructorBridges0.headOption

      // The actual class definition
      val classDefinition = js.ClassDef(classVar,
          envField("inheritable") DOT encodeClassFullNameIdent(sym.superClass),
          generatedMembers.toList ++ bridges)

      /* Inheritable constructor
       *
       * ScalaJS.inheritable.classIdent = function() {};
       * ScalaJS.inheritable.classIdent.prototype = ScalaJS.c.classIdent.prototype;
       */
      val createInheritableConstructor = {
        val inheritableConstructorVar = envField("inheritable") DOT classIdent
        js.Block(
            js.DocComment("@constructor"),
            inheritableConstructorVar := js.Function(Nil, js.Skip()),
            inheritableConstructorVar DOT "prototype" := classVar DOT "prototype")
      }

      /* JS-friendly constructor
       *
       * ScalaJS.classes.classIdent = function(<args of the constructor bridge>) {
       *   ScalaJS.c.classIdent.call(this);
       *   <body of the constructor bridge>
       * }
       * ScalaJS.classes.prototype = Class.prototype;
       */
      val createJSConstructorStat = constructorBridge match {
        case Some(js.MethodDef(_, args, body)) =>
          val jsConstructorVar = envField("classes") DOT classIdent
          js.Block(
              js.DocComment("@constructor"),
              jsConstructorVar := js.Function(args, js.Block(
                  js.ApplyMethod(classVar, js.Ident("call"), List(js.This())),
                  body)),
              jsConstructorVar DOT "prototype" := classVar DOT "prototype")

        case _ =>
          js.Skip()
      }

      // Instance tests

      val instanceTestMethods = genInstanceTestMethods(cd)

      // Data

      val createDataStat = {
        val classDataVar = envField("data") DOT classIdent

        js.Block(
            classDataVar := genDataRecord(cd),
            classVar DOT "prototype" DOT "$classData" := classDataVar)
      }

      // Bring it all together

      val everything = js.Block(
          classDefinition,
          createInheritableConstructor,
          createJSConstructorStat,
          instanceTestMethods,
          createDataStat)

      currentClassSym = null

      everything
    }

    // Generate the class data of a raw JS class -------------------------------

    /** Gen JS code creating the class data of a raw JS class
     */
    def genRawJSClassData(cd: ClassDef): js.Tree = {
      import js.TreeDSL._

      implicit val pos = cd.pos
      val ClassDef(mods, name, _, impl) = cd
      val sym = cd.symbol

      val classIdent = encodeClassFullNameIdent(sym)

      val classDataVar = envField("data") DOT classIdent
      classDataVar := genDataRecord(cd)
    }

    // Generate an interface ---------------------------------------------------

    /** Gen JS code for an interface definition
     *  This is very simple, as interfaces have virtually no existence at
     *  runtime. They exist solely for reflection purposes.
     */
    def genInterface(cd: ClassDef): js.Tree = {
      import js.TreeDSL._

      implicit val pos = cd.pos
      val sym = cd.symbol

      val classIdent = encodeClassFullNameIdent(sym)

      val instanceTestMethods = genInstanceTestMethods(cd)

      val createDataStat = {
        envField("data") DOT classIdent := genDataRecord(cd)
      }

      js.Block(instanceTestMethods, createDataStat)
    }

    // Generate an implementation class of a trait -----------------------------

    /** Gen JS code for an implementation class (of a trait)
     */
    def genImplClass(cd: ClassDef): js.Tree = {
      import js.TreeDSL._

      implicit val pos = cd.pos
      val ClassDef(mods, name, _, impl) = cd
      val sym = cd.symbol
      currentClassSym = sym

      val generatedMethods = new ListBuffer[js.MethodDef]

      def gen(tree: Tree) {
        tree match {
          case EmptyTree => ()
          case Template(_, _, body) => body foreach gen

          case dd: DefDef =>
            generatedMethods ++= genMethod(dd)

          case _ => abort("Illegal tree in gen of genImplClass(): " + tree)
        }
      }

      gen(impl)

      val implModuleFields =
        for (js.MethodDef(name, params, body) <- generatedMethods.result())
          yield name -> js.Function(params, body)

      val fieldCreations =
        for ((name, value) <- implModuleFields) yield
          js.Assign(js.Select(envField("impls"), name), value)

      val implDefinition = js.Block(fieldCreations)

      currentClassSym = null

      implDefinition
    }

    // Commons for genClass, genRawJSClassData and genInterface ----------------

    def genInstanceTestMethods(cd: ClassDef): js.Tree = {
      import js.TreeDSL._

      implicit val pos = cd.pos
      val sym = cd.symbol

      val displayName = sym.fullName
      val classIdent = encodeClassFullNameIdent(sym)

      val isAncestorOfString =
        StringClass.ancestors contains sym

      val createIsStat = {
        val obj = js.Ident("obj")
        envField("is") DOT classIdent := js.Function(List(obj), js.Return {
          var test = (obj && (obj DOT "$classData") &&
              (obj DOT "$classData" DOT "ancestors" DOT classIdent))

          if (isAncestorOfString)
            test = test || (
                js.UnaryOp("typeof", obj) === js.StringLiteral("string"))

          !(!test)
        })
      }

      val createAsStat = {
        val obj = js.Ident("obj")
        envField("as") DOT classIdent := js.Function(List(obj), js.Block {
          IF (js.ApplyMethod(envField("is"), classIdent, List(obj)) ||
              (obj === js.Null())) {
            js.Return(obj)
          } ELSE {
            genCallHelper("throwClassCastException", obj,
                js.StringLiteral(displayName))
          }
        })
      }

      val createIsArrayOfStat = {
        val obj = js.Ident("obj")
        val depth = js.Ident("depth")
        envField("isArrayOf") DOT classIdent := js.Function(List(obj, depth), js.Block {
          js.Return(!(!(obj && (obj DOT "$classData") &&
              ((obj DOT "$classData" DOT "arrayDepth") === depth) &&
              (obj DOT "$classData" DOT "arrayBase" DOT "ancestors" DOT classIdent))))
        })
      }

      val createAsArrayOfStat = {
        val obj = js.Ident("obj")
        val depth = js.Ident("depth")
        envField("asArrayOf") DOT classIdent := js.Function(List(obj, depth), js.Block {
          IF (js.ApplyMethod(envField("isArrayOf"), classIdent, List(obj, depth)) ||
              (obj === js.Null())) {
            js.Return(obj)
          } ELSE {
            genCallHelper("throwArrayCastException", obj,
                js.StringLiteral("L"+displayName+";"), depth)
          }
        })
      }

      js.Block(createIsStat, createAsStat,
          createIsArrayOfStat, createAsArrayOfStat)
    }

    def genDataRecord(cd: ClassDef): js.Tree = {
      import js.TreeDSL._

      implicit val pos = cd.pos
      val sym = cd.symbol

      val isInterface = sym.isInterface
      val isAncestorOfString = StringClass.ancestors contains sym

      val parentData = {
        if (isInterface) js.Undefined()
        else envField("data") DOT encodeClassFullNameIdent(
            if (sym.superClass == NoSymbol) ObjectClass else sym.superClass)
      }

      val ancestorsRecord = js.ObjectConstr(
          for (ancestor <- sym :: sym.ancestors)
            yield (encodeClassFullNameIdent(ancestor), js.BooleanLiteral(true)))

      val classIdent = encodeClassFullNameIdent(sym)

      js.New(envField("ClassTypeData"), List(
          js.ObjectConstr(List(classIdent -> js.IntLiteral(0))),
          js.BooleanLiteral(isInterface),
          js.StringLiteral(sym.fullName),
          parentData,
          ancestorsRecord
      ) ++ (
          // Ancestors of string have a non-standard isInstanceOf test
          if (isAncestorOfString) List(envField("is") DOT classIdent)
          else Nil
      ))
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
          js.Assign(js.DotSelect(js.This(), fieldName), genZeroOf(f.tpe))
        }
      }.toList

      {
        implicit val pos = cd.pos
        val superClass =
          if (currentClassSym.superClass == NoSymbol) ObjectClass
          else currentClassSym.superClass
        val superCall =
          js.ApplyMethod(encodeClassSym(superClass),
              js.Ident("call"), List(js.This()))
        js.MethodDef(js.Ident("constructor"), Nil,
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
     *  Some methods are not emitted at all:
     *  * Primitives, since they are never actually called
     *  * Abstract methods (alternative: throw a java.lang.AbstractMethodError)
     *  * Trivial constructors, which only call their super constructor, with
     *    the same signature, and the same arguments. The JVM needs these
     *    constructors, but not JavaScript. Since there are lots of them, we
     *    take the trouble of recognizing and removing them.
     *
     *  Constructors are emitted by generating their body as a statement, then
     *  return `this`.
     *
     *  Other (normal) methods are emitted with `genMethodBody()`.
     */
    def genMethod(dd: DefDef): Option[js.MethodDef] = {
      implicit val pos = dd.pos
      val DefDef(mods, name, _, vparamss, _, rhs) = dd
      val sym = dd.symbol
      currentMethodSym = sym

      isModuleInitialized = false
      methodHasTailJump = false
      methodTailJumpThisSym = NoSymbol
      methodTailJumpLabelSym = NoSymbol
      methodTailJumpFormalArgs = Nil
      usedLocalNames.clear()
      localSymbolNames.clear()

      assert(vparamss.isEmpty || vparamss.tail.isEmpty,
          "Malformed parameter list: " + vparamss)
      val params = if (vparamss.isEmpty) Nil else vparamss.head map (_.symbol)

      assert(!sym.owner.isInterface,
          "genMethod() must not be called for methods in interfaces: "+sym)

      val result = {
        if (scalaPrimitives.isPrimitive(sym)
            || sym.isDeferred // abstract method
            || isTrivialConstructor(sym, params, rhs)) {
          None
        } else {
          val jsParams =
            for (param <- params)
              yield encodeLocalSym(param, freshName)(param.pos)

          val body = {
            if (sym.isClassConstructor)
              js.Block(genStat(rhs), js.Return(js.This()))
            else
              genMethodBody(rhs, params, toTypeKind(sym.tpe.resultType))
          }

          Some(js.MethodDef(encodeMethodSym(sym), jsParams, body))
        }
      }

      currentMethodSym = null

      result
    }

    private def isTrivialConstructor(sym: Symbol, params: List[Symbol],
        rhs: Tree): Boolean = {
      if (!sym.isClassConstructor) {
        false
      } else {
        rhs match {
          // Shape of a constructor that only calls super
          case Block(List(Apply(fun @ Select(_:Super, _), args)), Literal(_)) =>
            val callee = fun.symbol
            implicit val dummyPos = NoPosition

            // Does the callee have the same signature as sym
            if (encodeMethodSym(sym) == encodeMethodSym(callee)) {
              // Test whether args are trivial forwarders
              assert(args.size == params.size, "Argument count mismatch")
              params.zip(args) forall { case (param, arg) =>
                arg.symbol == param
              }
            } else {
              false
            }

          case _ => false
        }
      }
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
     *  constructor name is always the same and need not be given.
     */
    def genModuleAccessor(sym: Symbol): js.Tree = {
      import js.TreeDSL._

      require(sym.isModuleClass,
          "genModuleAccessor called with non-moduleClass symbol: " + sym)

      implicit val pos = sym.pos

      val moduleIdent = encodeModuleFullNameIdent(sym)
      val moduleInstance = encodeModuleSymInstance(sym)

      val createModuleInstanceField = {
        moduleInstance := js.Undefined()
      }

      val createAccessor = {
        envField("modules") DOT moduleIdent := js.Function(Nil, js.Block(
            IF (!(moduleInstance)) {
              moduleInstance := js.ApplyMethod(
                  js.New(encodeClassSym(sym), Nil),
                  js.Ident("init___"),
                  Nil)
            },
            js.Return(moduleInstance)
        ))
      }

      js.Block(createModuleInstanceField, createAccessor)
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
            js.Block(
                js.VarDef(encodeLocalSym(methodTailJumpThisSym, freshName), js.This()),
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
              js.DotSelect(genExpr(qualifier), encodeFieldSym(sym))
            }

          js.Assign(member, genExpr(rhs))

        /** lhs = rhs */
        case Assign(lhs, rhs) =>
          val sym = lhs.symbol
          js.Assign(encodeLocalSym(sym, freshName), genExpr(rhs))

        case _ =>
          exprToStat(genExpr(tree))
      }
    }

    /** Turn a JavaScript statement into an expression of type Unit */
    def statToExpr(tree: js.Tree): js.Tree = {
      implicit val pos = tree.pos
      js.Block(tree, js.Undefined())
    }

    /** Turn a JavaScript expression of type Unit into a statement */
    def exprToStat(tree: js.Tree): js.Tree = {
      /* Any JavaScript expression is also a statement, but at least we get rid
       * of the stupid js.Block(..., js.Undefined()) that we create ourselves
       * in statToExpr().
       */
      implicit val pos = tree.pos
      tree match {
        case js.Block(stats :+ js.Undefined()) => js.Block(stats)
        case js.Undefined() => js.Skip()
        case _ => tree
      }
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
          statToExpr(js.VarDef(encodeLocalSym(sym, freshName), lhsTree))

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
            encodeLocalSym(methodTailJumpThisSym, freshName)
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
            js.DotSelect(genExpr(qualifier), encodeFieldSym(sym))
          }

        case Ident(name) =>
          val sym = tree.symbol
          if (!sym.isPackage) {
            if (sym.isModule) {
              assert(!sym.isPackageClass, "Cannot use package as value: " + tree)
              genLoadModule(sym)
            } else {
              encodeLocalSym(sym, freshName)
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

          js.Block((prologue map genStat) :+ translatedMatch)

        /** Normal block */
        case Block(stats, expr) =>
          val statements = stats map genStat
          val expression = genExpr(expr)
          js.Block(statements :+ expression)

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
          statToExpr(js.While(genExpr(cond), js.Block(bodyStats map genStat)))

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
          statToExpr(js.While(js.BooleanLiteral(true),
              js.Block(bodyStats map genStat)))

        // do { body } while (cond)
        case LabelDef(lname, Nil,
            Block(bodyStats,
                If(cond,
                    Apply(target @ Ident(lname2), Nil),
                    Literal(_)))) if (target.symbol == sym) =>
          statToExpr(js.DoWhile(js.Block(bodyStats map genStat), genExpr(cond)))

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
     */
    def genTry(tree: Try): js.Tree = {
      implicit val jspos = tree.pos
      val Try(block, catches, finalizer) = tree

      val blockAST = genExpr(block)
      val exceptVar = js.Ident("$jsexc$")

      def isAncestorOfJavaScriptException(tpe: Type) = {
        // A bit awkward because JSException might not be in the classpath
        val sym = tpe.typeSymbol
        (sym == JavaScriptExceptionClass ||
            RuntimeExceptionClass.ancestors.contains(sym))
      }

      val handlerAST = {
        if (catches.isEmpty) {
          js.EmptyTree
        } else {
          var mightCatchJavaScriptException = false

          val elseHandler: js.Tree = js.Throw(exceptVar)
          val handler0 = catches.foldRight(elseHandler) { (caseDef, elsep) =>
            implicit val jspos = caseDef.pos
            val CaseDef(pat, _, body) = caseDef

            // Extract exception type and variable
            val (tpe, boundVar) = (pat match {
              case Typed(Ident(nme.WILDCARD), tpt) =>
                (tpt.tpe, None)
              case Ident(nme.WILDCARD) =>
                (ThrowableClass.tpe, None)
              case Bind(_, _) =>
                (pat.symbol.tpe, Some(encodeLocalSym(pat.symbol, freshName)))
            })

            // Generate the body that must be executed if the exception matches
            val bodyWithBoundVar = (boundVar match {
              case None => genExpr(body)
              case Some(bv) =>
                js.Block(js.VarDef(bv, exceptVar), genExpr(body))
            })

            // Generate the test
            if (tpe == ThrowableClass.tpe) {
              mightCatchJavaScriptException = true
              bodyWithBoundVar
            } else {
              mightCatchJavaScriptException ||=
                isAncestorOfJavaScriptException(tpe)
              val cond = genIsInstanceOf(ThrowableClass.tpe, tpe, exceptVar)
              js.If(cond, bodyWithBoundVar, elsep)
            }
          }

          if (mightCatchJavaScriptException) {
            js.Block(
                js.Assign(exceptVar,
                    genCallHelper("wrapJavaScriptException", exceptVar)),
                handler0)
          } else {
            handler0
          }
        }
      }

      val finalizerAST = genStat(finalizer) match {
        case js.Skip() => js.EmptyTree
        case ast => ast
      }

      if (handlerAST == js.EmptyTree && finalizerAST == js.EmptyTree) blockAST
      else js.Try(blockAST, exceptVar, handlerAST, finalizerAST)
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
            val result = if (cast) {
              val ctor = ClassCastExceptionClass.info.member(
                  nme.CONSTRUCTOR).suchThat(_.tpe.params.isEmpty)
              js.Throw(genNew(ClassCastExceptionClass, ctor, Nil))
            } else {
              js.BooleanLiteral(false)
            }
            js.Block(source, result) // eval and discard source
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
            val callee = js.DotSelect(superProto, encodeMethodSym(fun.symbol)(fun.pos))(fun.pos)
            val thisArg =
              if (methodTailJumpThisSym == NoSymbol) js.This()(sup.pos)
              else encodeLocalSym(methodTailJumpThisSym, freshName)(sup.pos)
            val arguments = thisArg :: (args map genExpr)
            js.ApplyMethod(callee, js.Ident("call"), arguments)
          }

          // We initialize the module instance just after the super constructor
          // call.
          if (isStaticModule(currentClassSym) && !isModuleInitialized &&
              currentMethodSym.isClassConstructor) {
            isModuleInitialized = true
            val initModule = js.Assign(
                encodeModuleSymInstance(currentClassSym), js.This())
            js.Block(superCall, initModule, js.This())
          } else {
            superCall
          }

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
                  formalArg = encodeLocalSym(formalArgSym, freshName)
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
                  js.Block(js.Assign(formalArg, actualArg), tailJump)

                case _ =>
                  val tempAssignments =
                    for ((_, tempArg, actualArg) <- triplets)
                      yield js.VarDef(tempArg, actualArg)
                  val trueAssignments =
                    for ((formalArg, tempArg, _) <- triplets)
                      yield js.Assign(formalArg, tempArg)
                  js.Block(tempAssignments ++ trueAssignments :+ tailJump)
              }
            } else // continues after the comment
            /** Jump the to the end-label of a pattern match
             *  Such labels have exactly one argument, which is the result of
             *  the pattern match (of type Unit if the match is in statement
             *  position). We simply `return` the argument as the result of the
             *  labeled block surrounding the match.
             */
            if (sym.name.toString() startsWith "matchEnd") {
              val labelIdent = encodeLabelSym(sym, freshName)
              js.Return(genExpr(args.head), Some(labelIdent))
            } else {
              /* No other label apply should ever happen. If it does, then we
               * have missed a pattern of LabelDef/LabelApply and some new
               * translation must be found for it.
               */
              abort("Found unknown label apply at "+tree.pos+": "+tree)
            }
          } else // continues after the comment
          /** Primitive method whose code is generated by the codegen */
          if (scalaPrimitives.isPrimitive(sym)) {
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
             *  * Methods of ancestors of java.lang.String (because they could
             *    be a primitive string at runtime).
             *  * Calls to primitive JS methods (Scala.js -> JS bridge)
             *  * Regular method call
             */
            if (settings.debug.value)
              log("Gen CALL_METHOD with sym: " + sym + " isStaticSymbol: " + sym.isStaticMember);

            val Select(receiver, _) = fun

            if (ToStringMaybeOnString contains fun.symbol) {
              js.ApplyMethod(genExpr(receiver), js.Ident("toString"), Nil)
            } else if (MethodWithHelperInEnv contains fun.symbol) {
              val helper = MethodWithHelperInEnv(fun.symbol)
              val arguments = (receiver :: args) map genExpr
              genCallHelper(helper, arguments:_*)
            } else if (isRawJSType(receiver.tpe) || isStringType(receiver.tpe)) {
              genPrimitiveJSCall(app)
            } else {
              val instance = genExpr(receiver)
              val arguments = args map genExpr

              js.ApplyMethod(instance, encodeMethodSym(fun.symbol), arguments)
            }
          }
      }
    }

    private lazy val ToStringMaybeOnString = Set[Symbol](
      Object_toString,
      getMemberMethod(CharSequenceClass, nme.toString_),
      getMemberMethod(StringClass, nme.toString_)
    )

    // TODO Make these primitives?
    private lazy val MethodWithHelperInEnv = Map[Symbol, String](
      Object_getClass  -> "objectGetClass",
      Object_clone     -> "objectClone",
      Object_finalize  -> "objectFinalize",
      Object_notify    -> "objectNotify",
      Object_notifyAll -> "objectNotifyAll",
      Object_equals    -> "objectEquals",
      Object_hashCode  -> "objectHashCode",

      getMemberMethod(CharSequenceClass, newTermName("length")) -> "charSequenceLength",
      getMemberMethod(CharSequenceClass, newTermName("charAt")) -> "charSequenceCharAt",
      getMemberMethod(CharSequenceClass, newTermName("subSequence")) -> "charSequenceSubSequence",

      getMemberMethod(ComparableClass, newTermName("compareTo")) -> "comparableCompareTo",

      getMemberMethod(StringClass, nme.equals_) -> "objectEquals",
      getMemberMethod(StringClass, nme.hashCode_) -> "objectHashCode",
      getMemberMethod(StringClass, newTermName("compareTo")) -> "comparableCompareTo"
    )

    private lazy val CharSequenceClass = requiredClass[java.lang.CharSequence]

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
      if (isRawJSType(to)) {
        def genTypeOfTest(typeString: String) = {
          js.BinaryOp("===", js.UnaryOp("typeof", value),
              js.StringLiteral(typeString))
        }
        to.typeSymbol match {
          case JSNumberClass    => genTypeOfTest("number")
          case JSStringClass    => genTypeOfTest("string")
          case JSBooleanClass   => genTypeOfTest("boolean")
          case JSUndefinedClass => genTypeOfTest("undefined")
          case sym if sym.isTrait =>
            currentCUnit.error(pos,
                s"isInstanceOf[${sym.fullName}] not supported because it is a raw JS trait")
            js.BooleanLiteral(true)
          case sym =>
            js.BinaryOp("instanceof", value, genGlobalJSObject(sym))
        }
      } else {
        encodeIsInstanceOf(value, to)
      }
    }

    /** Gen JS code for an asInstanceOf cast (for reference types only) */
    def genAsInstanceOf(from: Type, to: Type, value: js.Tree)(
        implicit pos: Position = value.pos): js.Tree = {
      if (isRawJSType(to)) {
        // asInstanceOf on JavaScript is completely erased
        value
      } else {
        encodeAsInstanceOf(value, to)
      }
    }

    /** Gen JS code for a call to a Scala class constructor
     *  This first calls the only JS constructor for the class, which creates
     *  the fields of the instance, initialized to the zero of their respective
     *  types.
     *  Then we call the <init> method containing the code of the particular
     *  overload of the Scala constructors. Since this method returns `this`,
     *  we simply chain the calls.
     */
    def genNew(clazz: Symbol, ctor: Symbol, arguments: List[js.Tree])(
        implicit pos: Position): js.Tree = {
      val typeVar = encodeClassSym(clazz)
      val instance = js.New(typeVar, Nil)
      js.Apply(js.DotSelect(instance, encodeMethodSym(ctor)), arguments)
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

      genCallHelper("newArrayObject", arrayClassData,
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

      genCallHelper("makeNativeArrayWrapper",
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
     *  Jumps are then compiled as `break`s out of that block if the result has
     *  type Unit, or `return`s out of the block otherwise.
     */
    def genTranslatedMatch(cases: List[LabelDef],
        matchEnd: LabelDef)(implicit pos: Position): js.Tree = {

      val nextCaseSyms = (cases.tail map (_.symbol)) :+ NoSymbol

      val translatedCases = for {
        (LabelDef(_, Nil, rhs), nextCaseSym) <- cases zip nextCaseSyms
      } yield {
        def genCaseBody(tree: Tree): js.Tree = {
          implicit val pos = tree.pos
          tree match {
            case If(cond, thenp, elsep) =>
              js.If(genExpr(cond), genCaseBody(thenp), genCaseBody(elsep))

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

      js.Labeled(encodeLabelSym(matchEnd.symbol, freshName),
          js.Block(translatedCases))
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
          lazy val resultKind = toTypeKind(tree.tpe)

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
              (resultKind: @unchecked) match {
                case LongKind => genCallHelper("truncateToLong", actualDiv)
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
      genCallHelper(function, lsrc, rsrc)
    }

    /** Gen JS code for string concatenation
     *  We explicitly call the JS toString() on any non-String argument to
     *  avoid the weird things happening when adding "things" in JS.
     *  Because any argument can potentially be `null` or `undefined`, we
     *  cannot really call toString() directly. The helper
     *  `anyToStringForConcat` handles these cases properly.
     */
    private def genStringConcat(tree: Apply, receiver: Tree,
        args: List[Tree]): js.Tree = {
      implicit val pos = tree.pos

      val List(lhs, rhs) = for {
        op <- receiver :: args
      } yield {
        val genOp = genExpr(op)
        genOp match {
          case js.StringLiteral(_, _) => genOp
          case _ => genCallHelper("anyToStringForConcat", genOp)
        }
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
          genCallHelper("truncateToLong", source)

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

      val boxHelperName = toTypeKind(tpe) match {
        case kind: ValueTypeKind => functionPrefix + kind.primitiveCharCode
        case _ =>
          abort(s"makeBoxUnbox requires a primitive type, found $tpe at $pos")
      }
      genCallHelper(boxHelperName, expr)
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
      val genArgArray = genPrimitiveJSArgs(tree.symbol, args)

      lazy val js.ArrayConstr(genArgs) = genArgArray

      /* The implementations of java.lang.Class, java.lang.reflect.Array
       * and java.util.Arrays use fields and methods of the Scala.js global
       * environment through js.Dynamic calls.
       * These must be emitted as dot-selects when possible.
       */
      def shouldUseDynamicSelect: Boolean =
        currentClassSym == ClassClass || currentClassSym == ReflectArrayModuleClass ||
        currentClassSym == UtilArraysModuleClass

      def maybeDynamicSelect(receiver: js.Tree, item: js.Tree): js.Tree = {
        if (shouldUseDynamicSelect) {
          // Now that's a cute hack ...
          js.DynamicSelect(receiver, item) match {
            case js.DotSelect(
                js.DotSelect(js.Ident(ScalaJSEnvironmentName, _), js.Ident("g", _)),
                globalVar) =>
              globalVar
            case x => x
          }
        } else {
          js.BracketSelect(receiver, item)
        }
      }

      def extractFirstArg() = {
        genArgArray match {
          case js.ArrayConstr(firstArg :: otherArgs) =>
            (firstArg, js.ArrayConstr(otherArgs))
          case js.ApplyMethod(
              js.ArrayConstr(firstArg :: firstPart), concat, otherParts) =>
            (firstArg, js.ApplyMethod(
                js.ArrayConstr(firstPart), concat, otherParts))
        }
      }

      if (code == DYNNEW) {
        // js.Dynamic.newInstance(clazz)(actualArgs:_*)
        val (jsClass, actualArgArray) = extractFirstArg()
        actualArgArray match {
          case js.ArrayConstr(actualArgs) =>
            js.New(jsClass, actualArgs)
          case _ =>
            genCallHelper("newInstanceWithVarargs",
                jsClass, actualArgArray)
        }
      } else if (code == DYNAPPLY) {
        // js.Dynamic.applyDynamic(methodName)(actualArgs:_*)
        val (methodName, actualArgArray) = extractFirstArg()
        actualArgArray match {
          case js.ArrayConstr(actualArgs) =>
            js.Apply(maybeDynamicSelect(receiver, methodName), actualArgs)
          case _ =>
            genCallHelper("applyMethodWithVarargs",
                receiver, methodName, actualArgArray)
        }
      } else if (code == ARR_CREATE) {
        // js.Array.create(elements:_*)
        genArgArray
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

            case DYNSELECT =>
              // js.Dynamic.selectDynamic(arg)
              maybeDynamicSelect(receiver, arg)

            case DICT_PROPS =>
              // js.Dictionary.propertiesOf(arg)
              genCallHelper("propertiesOf", arg)
          }

        case List(arg1, arg2) =>
          code match {
            case DYNUPDATE =>
              // js.Dynamic.updateDynamic(arg1)(arg2)
              statToExpr(js.Assign(maybeDynamicSelect(receiver, arg1), arg2))
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

      val funName = sym.originalName.decoded
      val receiver = genExpr(receiver0)
      val argArray = genPrimitiveJSArgs(sym, args0)

      // valid only for methods that don't have any varargs
      lazy val js.ArrayConstr(args) = argArray
      lazy val argc = args.length

      def hasExplicitJSEncoding = {
        isScalaJSDefined && (
            sym.hasAnnotation(JSNameAnnotation) ||
            sym.hasAnnotation(JSBracketAccessAnnotation))
      }

      val isString = isStringType(receiver0.tpe)

      def paramType(index: Int) = sym.tpe.params(index).tpe

      def charToString(arg: js.Tree) = {
        val jsString = js.BracketSelect(envField("g"), js.StringLiteral("String"))
        js.ApplyMethod(jsString, js.StringLiteral("fromCharCode"), List(arg))
      }

      def charSeqToString(arg: js.Tree) =
        js.ApplyMethod(arg, js.Ident("toString"), Nil)

      def stringLength(arg: js.Tree) =
        js.BracketSelect(arg, js.StringLiteral("length"))

      funName match {
        case "unary_+" | "unary_-" | "unary_~" | "unary_!" =>
          assert(argc == 0)
          js.UnaryOp(funName.substring(funName.length-1), receiver)

        case "+" | "-" | "*" | "/" | "%" | "<<" | ">>" | ">>>" |
             "&" | "|" | "^" | "&&" | "||" =>
          assert(argc == 1)
          js.BinaryOp(funName, receiver, args.head)

        case "apply" if !hasExplicitJSEncoding =>
          /* Protect the receiver so that if the receiver is, e.g.,
           * path.f
           * we emit
           * ScalaJS.protect(path.f)(args...)
           * instead of
           * path.f(args...)
           * where
           * ScalaJS.protect = function(x) { return x; }
           * If we emit the latter, then `this` will be bound to `path` in
           * `f`, which is sometimes extremely harmful (e.g., for builtin
           * methods of `window`).
           */
          def protectedReceiver = receiver match {
            case js.DotSelect(_, _) | js.BracketSelect(_, _) =>
              genCallHelper("protect", receiver)
            case _ =>
              receiver
          }
          argArray match {
            case js.ArrayConstr(args) => js.Apply(protectedReceiver, args)
            case _ => js.ApplyMethod(receiver, js.StringLiteral("apply"),
                List(js.Null(), argArray))
          }

        case "charAt" | "codePointAt" if isString =>
          js.ApplyMethod(receiver, js.StringLiteral("charCodeAt"), args)
        case "length" if isString =>
          js.BracketSelect(receiver, js.StringLiteral("length"))
        case "isEmpty" if isString =>
          js.UnaryOp("!", js.BracketSelect(receiver, js.StringLiteral("length")))
        case "indexOf" | "lastIndexOf" if isString && !isStringType(paramType(0)) =>
          js.ApplyMethod(receiver, js.StringLiteral(funName),
              charToString(args.head) :: args.tail)
        case "contains" if isString =>
          val index = js.ApplyMethod(receiver, js.StringLiteral("indexOf"), args)
          js.BinaryOp(">=", index, js.IntLiteral(0))
        case "startsWith" if isString =>
          genCallHelper("stringStartsWith", receiver, args.head)
        case "endsWith" if isString =>
          genCallHelper("stringEndsWith", receiver, args.head)
        case "subSequence" if isString =>
          js.ApplyMethod(receiver, js.StringLiteral("substring"), args)
        case "intern" if isString =>
          receiver
        case "compareTo" if isString =>
          genCallHelper("comparableCompareTo", receiver, args.head)

        case "replace" if isString =>
          val argsAsStrings =
            if (paramType(0).typeSymbol == CharSequenceClass)
              args map charSeqToString
            else
              args map charToString
          js.ApplyMethod(
              js.ApplyMethod(receiver,
                  js.StringLiteral("split"), List(argsAsStrings(0))),
              js.StringLiteral("join"), List(argsAsStrings(1)))

        case "matches" if isString =>
          // Made-up of Pattern.matches(args.head, receiver)
          val PatternModuleClass =
            requiredClass[java.util.regex.Pattern].companionModule.moduleClass
          val PatternModule = genLoadModule(PatternModuleClass)
          val matchesMethod =
            getMemberMethod(PatternModuleClass, newTermName("matches"))
          js.ApplyMethod(PatternModule, encodeMethodSym(matchesMethod), List(
              args.head, receiver))

        case "split" if isString =>
          // Made-up of Pattern.compile(args.head).split(receiver, args.tail)
          val PatternClass = requiredClass[java.util.regex.Pattern]
          val PatternModuleClass = PatternClass.companionModule.moduleClass
          val PatternModule = genLoadModule(PatternModuleClass)
          val compileMethod = getMemberMethod(PatternModuleClass,
              newTermName("compile")).suchThat(_.tpe.params.size == 1)
          val pattern = js.ApplyMethod(PatternModule,
              encodeMethodSym(compileMethod), List(args.head))
          val splitMethod = getMemberMethod(PatternClass,
              newTermName("split")).suchThat(_.tpe.params.size == args.size)
          js.ApplyMethod(pattern, encodeMethodSym(splitMethod),
              receiver :: args.tail)

        case _ =>
          def isJSGetter = {
            sym.tpe.params.isEmpty && beforePhase(currentRun.uncurryPhase) {
              sym.tpe.isInstanceOf[NullaryMethodType]
            }
          }

          def isJSSetter = {
            funName.endsWith("_=") && beforePhase(currentRun.uncurryPhase) {
              sym.tpe.paramss match {
                case List(List(arg)) => !isScalaRepeatedParamType(arg.tpe)
                case _ => false
              }
            }
          }

          def isJSBracketAccess = {
            isScalaJSDefined && sym.hasAnnotation(JSBracketAccessAnnotation)
          }

          if (isJSGetter) {
            assert(argc == 0)
            js.BracketSelect(receiver, js.StringLiteral(funName))
          } else if (isJSSetter) {
            assert(argc == 1)
            statToExpr(js.Assign(
                js.BracketSelect(receiver,
                    js.StringLiteral(funName.substring(0, funName.length-2))),
                args.head))
          } else if (isJSBracketAccess) {
            assert(argArray.isInstanceOf[js.ArrayConstr] && (argc == 1 || argc == 2),
                s"@JSBracketAccess methods should have 1 or 2 non-varargs arguments")
            args match {
              case List(keyArg) =>
                js.BracketSelect(receiver, keyArg)
              case List(keyArg, valueArg) =>
                statToExpr(js.Assign(
                    js.BracketSelect(receiver, keyArg),
                    valueArg))
            }
          } else {
            val jsFunName = jsNameOf(sym)
            argArray match {
              case js.ArrayConstr(args) =>
                js.ApplyMethod(receiver, js.StringLiteral(jsFunName), args)
              case _ =>
                genCallHelper("applyMethodWithVarargs",
                    receiver, js.StringLiteral(jsFunName), argArray)
            }
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
      val js.ArrayConstr(args) = genPrimitiveJSArgs(ctor, args0)

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

      genPrimitiveJSArgs(ctor, args0) match {
        case js.ArrayConstr(args) =>
          if (cls == JSObjectClass && args.isEmpty) js.ObjectConstr(Nil)
          else js.New(genPrimitiveJSClass(cls), args)
        case argArray =>
          genCallHelper("newInstanceWithVarargs",
              genPrimitiveJSClass(cls), argArray)
      }
    }

    /** Gen JS code representing a JS class (subclass of js.Any) */
    private def genPrimitiveJSClass(sym: Symbol)(
        implicit pos: Position): js.Tree = {
      genGlobalJSObject(sym)
    }

    /** Gen JS code representing a JS module (var of the global scope) */
    private def genPrimitiveJSModule(sym: Symbol)(
        implicit pos: Position): js.Tree = {
      genGlobalJSObject(sym)
    }

    /** Gen JS code representing a JS object (class or module) in global scope
     */
    private def genGlobalJSObject(sym: Symbol)(
        implicit pos: Position): js.Tree = {
      jsNameOf(sym).split('.').foldLeft(envField("g")) { (memo, chunk) =>
        js.BracketSelect(memo, js.StringLiteral(chunk, Some(chunk)))
      }
    }

    /** Gen actual actual arguments to a primitive JS call
     *  This handles repeated arguments (varargs) by turning them into
     *  JS varargs, i.e., by expanding them into normal arguments.
     *
     *  Returns an only tree which is a JS array of the arguments. In most
     *  cases, it will be a js.ArrayConstr with the expanded arguments. It will
     *  not if a Seq is passed to a varargs argument with the syntax seq:_*.
     */
    private def genPrimitiveJSArgs(sym: Symbol, args: List[Tree])(
        implicit pos: Position): js.Tree = {
      val wereRepeated = afterPhase(currentRun.typerPhase) {
        for {
          params <- sym.tpe.paramss
          param <- params
        } yield isScalaRepeatedParamType(param.tpe)
      }

      var reversedParts: List[js.Tree] = Nil
      var reversedPartUnderConstruction: List[js.Tree] = Nil

      def closeReversedPartUnderConstruction() = {
        if (!reversedPartUnderConstruction.isEmpty) {
          val part = reversedPartUnderConstruction.reverse
          reversedParts ::= js.ArrayConstr(part)
          reversedPartUnderConstruction = Nil
        }
      }

      for ((arg, wasRepeated) <- args zip wereRepeated) {
        if (wasRepeated) {
          genPrimitiveJSRepeatedParam(arg) match {
            case js.ArrayConstr(jsArgs) =>
              reversedPartUnderConstruction =
                jsArgs reverse_::: reversedPartUnderConstruction
            case jsArgArray =>
              closeReversedPartUnderConstruction()
              reversedParts ::= jsArgArray
          }
        } else {
          reversedPartUnderConstruction ::= genExpr(arg)
        }
      }
      closeReversedPartUnderConstruction()

      reversedParts match {
        case Nil => js.ArrayConstr(Nil)
        case List(part) => part
        case _ =>
          val partHead :: partTail = reversedParts.reverse
          js.ApplyMethod(partHead, js.StringLiteral("concat"), partTail)
      }
    }

    /** Gen JS code for a repeated param of a primitive JS method
     *  In this case `arg` has type Seq[T] for some T, but the result should
     *  have type js.Array[T]. So this method takes care of the conversion.
     *  It is specialized for the shapes of tree generated by the desugaring
     *  of repeated params in Scala, so that these produce a js.ArrayConstr.
     */
    private def genPrimitiveJSRepeatedParam(arg: Tree): js.Tree = {
      implicit val pos = arg.pos

      // Given a method `def foo(args: T*)`
      arg match {
        // foo(arg1, arg2, ..., argN) where N > 0
        case MaybeAsInstanceOf(WrapArray(
            MaybeAsInstanceOf(ArrayValue(tpt, elems))))
            if elems.forall(e => !isPrimitiveValueType(e.tpe)) => // non-optimal fix to #39
          js.ArrayConstr(elems map genExpr)

        // foo()
        case Select(_, _) if arg.symbol == NilModule =>
          js.ArrayConstr(Nil)

        // foo(argSeq:_*)
        case _ =>
          /* Here we fall back to calling js.Any.fromTraversableOnce(seqExpr)
           * to perform the conversion.
           */
          js.ApplyMethod(
              genLoadModule(JSAnyModule),
              encodeMethodSym(JSAny_fromTraversableOnce),
              List(genExpr(arg)))
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

    /** Generate a literal "zero" for the requested type */
    def genZeroOf(tpe: Type)(implicit pos: Position): js.Tree = toTypeKind(tpe) match {
      case UNDEFINED => js.Undefined()
      case BOOL => js.BooleanLiteral(false)
      case INT(_) => js.IntLiteral(0)
      case FLOAT(_) => js.DoubleLiteral(0.0)
      case REFERENCE(_) => js.Null()
      case ARRAY(_) => js.Null()
    }

    /** Generate loading of a module value
     *  Can be given either the module symbol, or its module class symbol.
     */
    private def genLoadModule(sym0: Symbol)(implicit pos: Position): js.Tree = {
      require(sym0.isModuleOrModuleClass,
          "genLoadModule called with non-module symbol: " + sym0)
      val sym = if (sym0.isModule) sym0.moduleClass else sym0

      val isGlobalScope =
        isScalaJSDefined &&
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

    /** Generate a call to a helper function in the environment */
    def genCallHelper(helperName: String, args: js.Tree*)(implicit pos: Position) = {
      js.ApplyMethod(environment, js.Ident(helperName), args.toList)
    }
  }

  /** Test whether the given type represents a raw JavaScript type
   *
   *  I.e., test whether the type extends scala.js.Any
   */
  def isRawJSType(tpe: Type): Boolean =
    tpe.typeSymbol.annotations.find(_.tpe =:= RawJSTypeAnnot.tpe).isDefined

  private def isStringType(tpe: Type): Boolean =
    tpe.typeSymbol == StringClass

  /** Get JS name of Symbol if it was specified with JSName annotation */
  def jsNameOf(sym: Symbol): String = {
    if (isScalaJSDefined) {
      sym.getAnnotation(JSNameAnnotation).flatMap(_.stringArg(0)).getOrElse(
          sym.originalName.decoded)
    } else {
      sym.originalName.decoded
    }
  }

  private def isStaticModule(sym: Symbol): Boolean =
    sym.isModuleClass && !sym.isImplClass && !sym.isLifted
}
