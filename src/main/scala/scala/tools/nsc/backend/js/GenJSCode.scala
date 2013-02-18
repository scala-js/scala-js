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
abstract class GenJSCode extends SubComponent
                            with TypeKinds
                            with JSEncoding
                            with JSDesugaring
                            with GenJSFiles {
  val global: JSGlobal

  import global._

  import definitions.{
    ObjectClass, ClassCastExceptionClass, ThrowableClass,
    ScalaRunTimeModule,
    Object_isInstanceOf, Object_asInstanceOf, Object_toString, String_+,
    boxedClass, isBox, isUnbox, getMember
  }

  import platform.isMaybeBoxed

  val phaseName = "jscode"

  override def newPhase(p: Phase) = new JSCodePhase(p)

  class JSCodePhase(prev: Phase) extends StdPhase(prev) {

    override def name = phaseName
    override def description = "Generate JavaScript code from ASTs"
    override def erasedTypes = true

    // Accumulator for the generated classes -----------------------------------

    val generatedClasses = new ListBuffer[(Symbol, js.Tree)]

    // Some state --------------------------------------------------------------

    var currentCUnit: CompilationUnit = _
    var currentClassSym: Symbol = _
    var currentMethodSym: Symbol = _
    var isModuleInitialized: Boolean = false // see genApply for super calls

    // Top-level apply ---------------------------------------------------------

    override def apply(cunit: CompilationUnit) {
      try {
        currentCUnit = cunit

        def gen(tree: Tree) {
          tree match {
            case EmptyTree => ()
            case PackageDef(_, stats) => stats foreach gen
            case cd: ClassDef =>
              val generatedClass = genClass(cd)
              val sym = cd.symbol
              val wholeTree =
                if (sym.isModuleClass && sym.companionModule != NoSymbol) {
                  js.Block(List(generatedClass), genModuleAccessor(sym))(tree.pos)
                } else generatedClass
              generatedClasses += sym -> wholeTree
          }
        }

        gen(cunit.body)

        for ((classSymbol, tree) <- generatedClasses)
          genJSFile(cunit, classSymbol, tree)
      } finally {
        generatedClasses.clear()
        currentCUnit = null
        currentClassSym = null
        currentMethodSym = null
      }
    }

    // Generate a class --------------------------------------------------------

    def genClass(cd: ClassDef): js.Tree = {
      implicit val jspos = cd.pos
      val ClassDef(mods, name, _, impl) = cd
      currentClassSym = cd.symbol

      val parent = if (currentClassSym.isInterface)
        js.EmptyTree
      else if (currentClassSym.superClass == NoSymbol)
        encodeClassSym(ObjectClass)
      else
        encodeClassSym(currentClassSym.superClass)

      val generatedMethods = new ListBuffer[js.MethodDef]

      generatedMethods += genConstructor(cd)

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

      val typeVar = encodeClassSym(currentClassSym)

      currentClassSym = null

      val sugaredClass = js.ClassDef(typeVar, parent, generatedMethods.toList)
      desugarJavaScript(sugaredClass)
    }

    // Generate the constructor of a class -------------------------------------

    def genConstructor(cd: ClassDef): js.MethodDef = {
      /** Non-method term members are fields, except for module members. Module
       *  members can only happen on .NET (no flatten) for inner traits. There,
       *  a module symbol is generated (transformInfo in mixin) which is used
       *  as owner for the members of the implementation class (so that the
       *  backend emits them as static).
       *  No code is needed for this module symbol.
       */
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

    def genMethod(dd: DefDef): js.MethodDef = {
      implicit val jspos = dd.pos
      val DefDef(mods, name, _, vparamss, _, rhs) = dd
      currentMethodSym = dd.symbol

      isModuleInitialized = false

      assert(vparamss.isEmpty || vparamss.tail.isEmpty,
          "Malformed parameter list: " + vparamss)
      val params = if(vparamss.isEmpty) Nil else vparamss.head

      val jsParams =
        for (param <- params)
          yield encodeLocalSym(param.symbol)(param.pos)

      val isNative = currentMethodSym.hasAnnotation(definitions.NativeAttr)
      val isAbstractMethod =
        (currentMethodSym.isDeferred || currentMethodSym.owner.isInterface)

      val body = {
        if (!isNative && !isAbstractMethod) {
          val returnType = toTypeKind(currentMethodSym.tpe.resultType)
          if (currentMethodSym.isConstructor)
            js.Block(List(genStat(rhs)), js.Return(js.This()))
          else if (returnType == UNDEFINED) genStat(rhs)
          else js.Return(genExpr(rhs))(rhs.pos)
        } else {
          js.EmptyTree
        }
      }

      val methodPropIdent = encodeMethodSym(currentMethodSym)

      currentMethodSym = null

      js.MethodDef(methodPropIdent, jsParams, body)
    }

    // Generate a module accessor ----------------------------------------------

    def genModuleAccessor(sym: Symbol): js.Tree = {
      implicit val pos = sym.pos

      val internalVar = encodeModuleSymInternal(sym.companionModule)
      val varDef = js.VarDef(internalVar, js.Null())

      val funBody: js.Tree = {
        js.Block(List(
            js.If(js.BinaryOp("===", internalVar, js.Null()),
                js.Assign(internalVar, genNew(sym, sym.primaryConstructor, Nil)),
                js.Skip())),
            js.Return(internalVar))
      }

      val funDef = js.FunDef(encodeModuleSym(sym.companionModule), Nil, funBody)

      js.Block(List(varDef), funDef)
    }

    // Code generation ---------------------------------------------------------

    /** Generate code for a statement */
    def genStat(tree: Tree): js.Tree = {
      implicit val pos = tree.pos

      tree match {
        case Assign(lhs @ Select(qualifier, _), rhs) =>
          val sym = lhs.symbol

          val member =
            if (sym.isStaticMember) {
              genStaticMember(sym)
            } else {
              js.Select(genExpr(qualifier), encodeFieldSym(sym))
            }

          js.Assign(member, genExpr(rhs))

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

    /** Generate code for an expression */
    def genExpr(tree: Tree): js.Tree = {
      implicit val pos = tree.pos

      tree match {
        case lblDf: LabelDef =>
          genLabelDef(lblDf)

        case ValDef(_, nme.THIS, _, _) =>
          debuglog("skipping trivial assign to _$this: " + tree)
          js.Undefined()

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

        case New(tpt) =>
          abort("Unexpected New(" + tpt.summaryString + "/" + tpt + ") reached GenJSCode.\n" +
                "  Call was genExpr(" + tree + ")")

        case app: Apply =>
          genApply(app)

        case ApplyDynamic(qual, args) =>
          sys.error("No ApplyDynamic support yet.")

        case This(qual) =>
          val symIsModuleClass = tree.symbol.isModuleClass
          assert(tree.symbol == currentClassSym || symIsModuleClass,
              "Trying to access the this of another class: " +
              "tree.symbol = " + tree.symbol +
              ", class symbol = " + currentClassSym +
              " compilation unit:" + currentCUnit)
          if (symIsModuleClass && tree.symbol != currentClassSym) {
            genLoadModule(tree.symbol)
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
              this.genBuiltinApply("MakeNativeStrWrapper",
                  js.StringLiteral(value.stringValue))
            case NullTag =>
              js.Null()
            case ClazzTag =>
              genClassConstant(value.typeValue)
            case EnumTag =>
              genStaticMember(value.symbolValue)
          }

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

        case av : ArrayValue =>
          genArrayValue(av)

        case mtch : Match =>
          genMatch(mtch)

        case EmptyTree =>
          // TODO Hum, I do not think this is OK
          js.Undefined()

        case _ =>
          abort("Unexpected tree in genExpr: " +
              tree + "/" + tree.getClass + " at: " + tree.pos)
      }
    } // end of GenJSCode.genExpression()

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

    /** Generate a LabelDef
     *
     *  label l(x1, ..., xn) {
     *    body
     *  }
     *
     *  becomes
     *
     *  function l(x1, ..., xn) {
     *    body
     *  }
     *  l(x1, ..., xn)
     *
     *  in the hope that the continuation of all label-apply's are the same
     *  as the continuation of the label-def.
     */
    def genLabelDef(tree: LabelDef): js.Tree = {
      implicit val jspos = tree.pos
      val LabelDef(name, params, rhs) = tree

      if (settings.debug.value)
        log("Encountered a label def: " + tree)

      val sym = tree.symbol
      val procVar = encodeLabelSym(sym)

      val body = genExpr(rhs)
      val arguments = params map { ident => encodeLocalSym(ident.symbol) }

      val defineProc = js.FunDef(procVar, arguments, body)
      val call = js.Apply(procVar, arguments)

      js.Block(List(defineProc), call)
    }

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
              val cond = genBuiltinApply("IsInstance",
                  exceptVar, genClassConstant(tpe))
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

    def genApply(tree: Tree): js.Tree = {
      implicit val jspos = tree.pos

      tree match {
        // isInstanceOf or asInstanceOf
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
              js.Throw(genNew(ClassCastExceptionClass))
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
            genCast(from, boxedClass(to.typeSymbol).tpe, source, false)
          else
            genCast(from, to, source, cast)

        // 'super' call
        case Apply(fun @ Select(sup @ Super(_, mix), _), args) =>
          if (settings.debug.value)
            log("Call to super: " + tree)

          // TODO Do we need to take care of sup/mix?
          //val superClass = varForSymbol(sup.symbol.superClass)(sup.pos)

          val callee = js.Select(js.Super(), encodeMethodSym(fun.symbol))
          val arguments = args map genExpr
          val superCall = js.Apply(callee, arguments)

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

        // 'new' constructor call
        case Apply(fun @ Select(New(tpt), nme.CONSTRUCTOR), args) =>
          val ctor = fun.symbol
          if (settings.debug.value)
            assert(ctor.isClassConstructor,
                   "'new' call to non-constructor: " + ctor.name)

          val arguments = args map genExpr

          val generatedType = toTypeKind(tpt.tpe)
          if (settings.debug.value)
            assert(generatedType.isReferenceType || generatedType.isArrayType,
                 "Non reference type cannot be instantiated: " + generatedType)

          (generatedType: @unchecked) match {
            case arr @ ARRAY(elem) =>
              genNewArray(arr.elementKind, arr.dimensions, arguments)

            case rt @ REFERENCE(cls) =>
              genNew(cls, ctor, arguments)
          }

        case Apply(fun @ _, List(dynapply:ApplyDynamic))
        if (isUnbox(fun.symbol) &&
            hasPrimitiveReturnType(dynapply.symbol)) =>
          genApplyDynamic(dynapply, nobox = true)

        case app @ Apply(fun, args) =>
          val sym = fun.symbol

          if (sym.isLabel) {  // jump to a label
            if (settings.verbose.value)
              println("warning: jump found at "+tree.pos+", doing my best ...")

            val procVar = encodeLabelSym(sym)
            val arguments = args map genExpr
            js.Apply(procVar, arguments)
          } else if (isPrimitive(sym)) {
            // primitive operation
            genPrimitiveOp(app)
          } else {  // normal method call
            if (settings.debug.value)
              log("Gen CALL_METHOD with sym: " + sym + " isStaticSymbol: " + sym.isStaticMember);

            val Select(receiver, _) = fun
            val instance = genExpr(receiver)
            val arguments = args map genExpr

            js.Apply(js.Select(instance, encodeMethodSym(fun.symbol)), arguments)
          }
      }
    }

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

        case _ => value
      }
    }

    def genCast(from: Type, to: Type, value: js.Tree, cast: Boolean)(
        implicit pos: Position): js.Tree = {
      val classConstant = genClassConstant(to)
      if (cast) {
        genBuiltinApply("AsInstance", value, classConstant)
      } else {
        genBuiltinApply("IsInstance", value, classConstant)
      }
    }

    def genNew(clazz: Symbol, ctor: Symbol, arguments: List[js.Tree])(
        implicit pos: Position): js.Tree = {
      val typeVar = encodeClassSym(clazz)
      val classVar = encodeClassOfSym(clazz)
      val instance = js.New(typeVar, Nil)
      js.Apply(js.Select(instance, encodeMethodSym(ctor)), arguments)
    }

    def genNew(clazz: Symbol)(implicit pos: Position): js.Tree = {
      val ctor = ???
      genNew(clazz, ctor, Nil)
    }

    def genNewArray(elementKind: TypeKind, dimensions: Int,
        arguments: List[js.Tree])(implicit pos: Position): js.Tree = {
      val argsLength = arguments.length

      if (argsLength > dimensions)
        abort("too many arguments for array constructor: found " + argsLength +
          " but array has only " + dimensions + " dimension(s)")

      val componentClass = elementKind.toType

      genBuiltinApply("NewArrayObject", genClassConstant(componentClass),
          js.IntLiteral(dimensions), js.ArrayConstr(arguments))
    }

    def genArrayValue(tree: Tree): js.Tree = ???

    def genMatch(tree: Tree): js.Tree = ???

    private def isPrimitive(sym: Symbol) = {
      (scalaPrimitives.isPrimitive(sym) && (sym ne definitions.String_+))
    }

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
      else
        abort("Primitive operation not handled yet: " + sym.fullName + "(" +
            fun.symbol.simpleName + ") " + " at: " + (tree.pos))
    }

    private def genSimpleOp(tree: Apply, args: List[Tree], code: Int): js.Tree = {
      import scalaPrimitives._

      implicit val jspos = tree.pos

      val sources = args map genExpr

      sources match {
        // Unary operation
        case List(source) =>
          (code match {
            case POS =>
              source // nothing to do
            case NEG =>
              js.UnaryOp("~", source)
            case NOT =>
              genBuiltinApply("BinNot", source)
            case ZNOT =>
              genBuiltinApply("Not", source)
            case _ =>
              abort("Unknown unary operation code: " + code)
          })

        // Binary operation
        case List(lsrc, rsrc) =>
          lazy val leftKind = toTypeKind(args.head.tpe)

          def genEquality(eqeq: Boolean, not: Boolean) = {
            if (eqeq && leftKind.isReferenceType) {
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
              (leftKind: @unchecked) match {
                case _:INT => genBuiltinApply("IntDiv", lsrc, rsrc)
                case _:FLOAT => js.BinaryOp("/", lsrc, rsrc)
              }
            case MOD => js.BinaryOp("%", lsrc, rsrc)
            case OR => genBuiltinApply("BinOr", lsrc, rsrc)
            case XOR => genBuiltinApply("BinXor", lsrc, rsrc)
            case AND => genBuiltinApply("BinAnd", lsrc, rsrc)
            case LSL => genBuiltinApply("LSL", lsrc, rsrc)
            case LSR => genBuiltinApply("LSR", lsrc, rsrc)
            case ASR => genBuiltinApply("ASR", lsrc, rsrc)
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

      val function = if (mustUseAnyComparator) "AnyEqEq" else "AnyRefEqEq"
      genBuiltinApply(function, lsrc, rsrc)
    }

    private def genStringConcat(tree: Apply, receiver: Tree, args: List[Tree]): js.Tree = {
      implicit val jspos = tree.pos

      val List(arg) = args

      val instance = genExpr(receiver)
      val boxed = makeBox(instance, receiver.tpe)

      val stringInstance = js.ApplyMethod(boxed, encodeMethodSym(Object_toString), Nil)

      js.ApplyMethod(stringInstance, encodeMethodSym(String_+), List(genExpr(args.head)))
    }

    private def makeBox(expr: js.Tree, tpe: Type): js.Tree =
      makeBoxUnbox(expr, tpe, "box")

    private def makeUnbox(expr: js.Tree, tpe: Type): js.Tree =
      makeBoxUnbox(expr, tpe, "unbox")

    private def makeBoxUnbox(expr: js.Tree, tpe: Type, function: TermName): js.Tree = {
      implicit val pos = expr.pos

      val module = tpe.typeSymbol.companionModule
      val boxSymbol = definitions.getMember(module, function)
      js.ApplyMethod(genLoadModule(module),
          encodeMethodSym(boxSymbol), List(expr))
    }

    private def genScalaHash(tree: Apply, receiver: Tree): js.Tree = {
      implicit val jspos = tree.pos

      val instance = genLoadModule(ScalaRunTimeModule)
      val arguments = List(genExpr(receiver))
      val sym = getMember(ScalaRunTimeModule, stringToTermName("hash"))

      js.ApplyMethod(instance, encodeMethodSym(sym), arguments)
    }

    private def genArrayOp(tree: Tree, code: Int): js.Tree = {
      import scalaPrimitives._

      implicit val jspos = tree.pos

      val Apply(Select(arrayObj, _), args) = tree
      val arrayValue = genExpr(arrayObj)
      val arguments = args map genExpr

      if (scalaPrimitives.isArrayGet(code)) {
        // get an item of the array
        if (settings.debug.value)
          assert(args.length == 1,
                 "Too many arguments for array get operation: " + tree)

        js.BracketSelect(arrayValue, arguments(0))
      }
      else if (scalaPrimitives.isArraySet(code)) {
        // set an item of the array
        if (settings.debug.value)
          assert(args.length == 2,
                 "Too many arguments for array set operation: " + tree)

        statToExpr(js.Assign(js.BracketSelect(arrayValue, arguments(0)), arguments(1)))
      }
      else {
        // length of the array
        js.Select(arrayValue, js.Ident("length"))
      }
    }

    private def genSynchronized(tree: Apply): js.Tree = {
      implicit val jspos = tree.pos

      val Apply(Select(receiver, _), args) = tree
      val receiverExpr = genExpr(receiver)
      val body = genExpr(args.head)

      val proc = js.Function(Nil, body)

      js.ApplyMethod(receiverExpr, js.Ident("synchronized"), List(proc))
    }

    private def genCoercion(tree: Apply, receiver: Tree, code: Int): js.Tree = {
      import scalaPrimitives._

      implicit val jspos = tree.pos

      val source = genExpr(receiver)

      (code: @scala.annotation.switch) match {
        case B2F | B2D | S2F | S2D | C2F | C2D | I2F | I2D | L2F | L2D =>
          genBuiltinApply("IntToFloat", source)

        case F2B | F2S | F2C | F2I | F2L | D2B | D2S | D2C | D2I | D2L =>
          genBuiltinApply("FloatToInt", source)

        case _ => source
      }
    }

    private def genApplyDynamic(tree: Tree, nobox: Boolean = false): js.Tree = {
      implicit val jspos = tree.pos

      val sym = tree.symbol
      val ApplyDynamic(receiver, args) = tree

      val instance = genExpr(receiver)
      val arguments = genApplyDynamicArgs(sym, args)
      val apply = js.Apply(js.Select(instance, encodeMethodSym(sym)), arguments)

      val returnType = returnTypeOf(sym)
      if (nobox || !isPrimitiveKind(toTypeKind(returnType)))
        apply
      else
        makeBox(apply, returnType)
    }

    private def genApplyDynamicArgs(sym: Symbol, args: List[Tree]): List[js.Tree] = {
      val types = sym.tpe match {
        case MethodType(params, _) => params map (_.tpe)
        case NullaryMethodType(_) => Nil
      }

      args zip types map { case (arg, tpe) =>
        if (isPrimitiveKind(toTypeKind(tpe)))
          unboxDynamicParam(arg, tpe)
        else
          genExpr(arg)
      }
    }

    private def isPrimitiveKind(kind: TypeKind) = kind match {
      case BOOL | _:INT | _:FLOAT => true
      case _ => false
    }

    private def returnTypeOf(sym: Symbol) = sym.tpe match {
      case MethodType(_, tpe) => tpe
      case NullaryMethodType(tpe) => tpe
    }

    private def hasPrimitiveReturnType(sym: Symbol) =
      isPrimitiveKind(toTypeKind(returnTypeOf(sym)))

    private def unboxDynamicParam(tree: Tree, tpe: Type): js.Tree = {
      (tree: @unchecked) match {
        case Apply(_, List(result)) if (isBox(tree.symbol)) => genExpr(result)
        case _ => makeUnbox(genExpr(tree), tpe)
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
    private def genLoadModule(sym: Symbol)(implicit pos: Position) = {
      val symbol = if (sym.isModuleClass) sym.companionModule else sym
      js.Apply(encodeModuleSym(symbol), Nil)
    }

    /** Generate access to a static member */
    private def genStaticMember(sym: Symbol)(implicit pos: Position) = {
      js.Select(genLoadModule(sym.owner), encodeFieldSym(sym))
    }

    /** Generate a Class[_] value (e.g. coming from classOf[T]) */
    private def genClassConstant(tpe: Type)(implicit pos: Position): js.Tree = {
      toTypeKind(tpe) match {
        case array : ARRAY =>
          val elementClass = encodeClassOfSym(array.elementKind.toType.typeSymbol)
          genBuiltinApply("MultiArrayClassOf", elementClass,
              js.IntLiteral(array.dimensions))

        case _ => encodeClassOfSym(tpe.typeSymbol)
      }
    }

    /** Generate a call to a runtime builtin helper */
    def genBuiltinApply(funName: String, args: js.Tree*)(implicit pos: Position) = {
      js.Apply(js.Ident(funName), args.toList)
    }
  }
}
