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
                            with JSEncoding {
  val global: JSGlobal

  import global._

  val phaseName = "jscode"

  override def newPhase(p: Phase) = new JSCodePhase(p)

  class JSCodePhase(prev: Phase) extends StdPhase(prev) {

    import scala.tools.jsm.{ Names => JSNames }
    import JSNames.{ IdentifierName => JSIdentName, PropertyName => JSPropName }
    import scala.tools.jsm.ast.{ Trees => js }

    import scala.util.parsing.input.{ Position => JSPosition }

    override def name = phaseName
    override def description = "Generate JavaScript code from ASTs"
    override def erasedTypes = true

    // Accumulator for the generated classes -----------------------------------

    val generatedClasses = new ListBuffer[js.ClassDef]

    // Some state --------------------------------------------------------------

    var currentCUnit: CompilationUnit = _
    var currentClassSym: Symbol = _
    var currentMethodSym: Symbol = _

    // Some bridges with JS code -----------------------------------------------

    implicit class PositionJSHelper(pos: Position) {
      def toJSPos: JSPosition = {
        new JSPosition {
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
      try {
        currentCUnit = cunit

        def gen(tree: Tree) {
          tree match {
            case EmptyTree            => ()
            case PackageDef(_, stats) => stats foreach gen
            case cd: ClassDef         => genClass(cd)
          }
        }

        gen(cunit.body)
      } finally {
        generatedClasses.clear()
        currentCUnit = null
        currentClassSym = null
        currentMethodSym = null
      }
    }

    // Generate a class --------------------------------------------------------

    def genClass(cd: ClassDef): js.ClassDef = {
      implicit val jspos = cd.pos.toJSPos
      val ClassDef(mods, name, _, impl) = cd
      currentClassSym = cd.symbol

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

      currentClassSym = null

      val result = js.ClassDef(identName(name), Nil, generatedMethods.toList)
      new scala.tools.jsm.ast.Printers.TreePrinter(
          new java.io.PrintWriter(Console.out, true)).printTree(result)
      result
    }

    // Generate a method -------------------------------------------------------

    def genMethod(dd: DefDef): js.MethodDef = {
      implicit val jspos = dd.pos.toJSPos
      val DefDef(mods, name, _, vparamss, _, rhs) = dd
      currentMethodSym = dd.symbol

      assert(vparamss.isEmpty || vparamss.tail.isEmpty,
          "Malformed parameter list: " + vparamss)
      val params = if(vparamss.isEmpty) Nil else vparamss.head

      val jsParams =
        for (param <- params)
          yield identName(param.name)

      val isNative = currentMethodSym.hasAnnotation(definitions.NativeAttr)
      val isAbstractMethod =
        (currentMethodSym.isDeferred || currentMethodSym.owner.isInterface)

      val body = {
        if (!isNative && !isAbstractMethod) {
          val returnType = toTypeKind(currentMethodSym.tpe.resultType)
          if (returnType == UNDEFINED) genStat(rhs)
          else genExpr(rhs)
        } else {
          js.EmptyTree
        }
      }

      currentMethodSym = null

      js.MethodDef(propName(name), jsParams, body)
    }

    /** Generate code for a statement */
    def genStat(tree: Tree): js.Tree = {
      implicit val pos = tree.pos.toJSPos

      genExpr(tree) match {
        case _ : js.Literal =>
          js.Skip()
        case js.Block(List(stat), _ : js.Literal) =>
          stat
        case other =>
          other
      }
    }

    /** Generate code for an expression */
    def genExpr(tree: Tree): js.Tree = {
      implicit val pos = tree.pos.toJSPos

      def statOnlyToExpr(stat: js.Tree) = js.Block(List(stat), js.Undefined())

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
          statOnlyToExpr(js.VarDef(identName(name), lhsTree))

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
            val symVar = propForSymbol(sym)
            js.Select(genExpr(qualifier), symVar)
          }

        case Ident(name) =>
          val sym = tree.symbol
          if (!sym.isPackage) {
            if (sym.isModule) {
              assert(!sym.isPackageClass, "Cannot use package as value: " + tree)
              genLoadModule(sym)
            } else {
              varForSymbol(sym)
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

        case Block(stats, expr) =>
          val statements = stats map genStat
          val expression = genExpr(expr)
          js.Block(statements, expression)

        case Typed(Super(_, _), _) =>
          genExpr(This(currentClassSym))

        case Typed(expr, _) =>
          genExpr(expr)

        case ass : Assign =>
          genAssign(ass)

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
      implicit val jspos = tree.pos.toJSPos
      val LabelDef(name, params, rhs) = tree

      if (settings.debug.value)
        log("Encountered a label def: " + tree)

      val sym = tree.symbol
      val procVar = varForSymbol(sym)

      val body = genExpr(rhs)
      val arguments = params map { ident => varForSymbol(ident.symbol) }

      val defineProc = js.FunDef(procVar.name, arguments map (_.name), body)
      val call = js.Apply(procVar, arguments)

      js.Block(List(defineProc), call)
    }

    def genTry(tree: Try): js.Tree = {
      implicit val jspos = tree.pos.toJSPos
      val Try(block, catches, finalizer) = tree

      val blockAST = genExpr(block)
      val exceptVar = js.Ident(new JSIdentName("$jsexc$"))

      val handlerAST = {
        if (catches.isEmpty) {
          js.EmptyTree
        } else {
          val elseHandler: js.Tree = js.Throw(exceptVar)
          catches.foldRight(elseHandler) { (caseDef, elsep) =>
            implicit val jspos = caseDef.pos.toJSPos
            val CaseDef(pat, _, body) = caseDef

            // Extract exception type and variable
            val (tpe, boundVar) = (pat match {
              case Typed(Ident(nme.WILDCARD), tpt) =>
                (tpt.tpe, None)
              case Ident(nme.WILDCARD) =>
                (definitions.ThrowableClass.tpe, None)
              case Bind(_, _) =>
                (pat.symbol.tpe, Some(varForSymbol(pat.symbol)))
            })

            // Generate the body that must be executed if the exception matches
            val bodyWithBoundVar = (boundVar match {
              case None => genExpr(body)
              case Some(bv) =>
                js.Block(List(js.VarDef(bv.name, exceptVar)), genExpr(body))
            })

            // Generate the test
            if (tpe == definitions.ThrowableClass.tpe) {
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

      js.Try(blockAST, exceptVar.name, handlerAST, finalizerAST)
    }

    def genApply(tree: Tree): js.Tree = ???

    def genAssign(tree: Tree): js.Tree = ???

    def genArrayValue(tree: Tree): js.Tree = ???

    def genMatch(tree: Tree): js.Tree = ???

    /** Generate a literal "zero" for the requested type */
    def genZeroOf(tpe: Type)(implicit pos: JSPosition): js.Tree = toTypeKind(tpe) match {
      case UNDEFINED => js.Undefined()
      case BOOL => js.BooleanLiteral(false)
      case INT(_) => js.IntLiteral(0)
      case FLOAT(_) => js.DoubleLiteral(0.0)
      case REFERENCE(_) => js.Null()
      case ARRAY(_) => js.Null()
    }

    /** Generate loading of a module value */
    private def genLoadModule(sym: Symbol)(implicit pos: JSPosition) = {
      val symbol = if (sym.isModuleClass) sym.companionModule else sym
      js.Apply(varForModule(symbol), Nil)
    }

    /** Generate access to a static member */
    private def genStaticMember(sym: Symbol)(implicit pos: JSPosition) = {
      val instance = genLoadModule(sym.owner)
      // TODO val accessor = methodEncodedName(sym.name.toString, Nil, sym.tpe)
      val accessor = propName(sym.name)

      js.Apply(js.Select(instance, js.PropIdent(accessor)), Nil)
    }

    /** Generate a Class[_] value (e.g. coming from classOf[T]) */
    private def genClassConstant(tpe: Type)(implicit pos: JSPosition): js.Tree = {
      toTypeKind(tpe) match {
        case array : ARRAY =>
          val elementClass = varForClass(array.elementKind.toType.typeSymbol)
          genBuiltinApply("MultiArrayClassOf", elementClass,
              js.IntLiteral(array.dimensions))

        case _ => varForClass(tpe.typeSymbol)
      }
    }

    /** Generate a call to a runtime builtin helper */
    def genBuiltinApply(funName: String, args: js.Tree*)(implicit pos: JSPosition) = {
      js.Apply(js.Ident(identName(funName)), args.toList)
    }
  }
}
