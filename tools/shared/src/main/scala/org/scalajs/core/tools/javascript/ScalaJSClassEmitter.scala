/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.javascript

import org.scalajs.core.ir._
import Position._
import Transformers._
import org.scalajs.core.ir.Trees._
import Types._

import org.scalajs.core.tools.sem._
import CheckedBehavior.Unchecked

import org.scalajs.core.tools.javascript.{Trees => js}

import org.scalajs.core.tools.optimizer.{LinkedClass, LinkingUnit}

/** Defines methods to emit Scala.js classes to JavaScript code.
 *  The results are completely desugared.
 */
final class ScalaJSClassEmitter(semantics: Semantics, outputMode: OutputMode,
    globalInfo: LinkingUnit.GlobalInfo) {

  import JSDesugaring._

  @deprecated("Use the constructor with an explicit output mode", "0.6.2")
  def this(semantics: Semantics, globalInfo: LinkingUnit.GlobalInfo) =
    this(semantics, OutputMode.ECMAScript51Global, globalInfo)

  @deprecated("Use the constructor with an explicit output mode and additional GlobalInfo", "0.6.1")
  def this(semantics: Semantics) =
    this(semantics, LinkingUnit.GlobalInfo.SafeApproximation)

  private implicit def implicitOutputMode: OutputMode = outputMode

  /** Desugar a Scala.js class into ECMAScript 5 constructs
   *
   *  @param tree The IR tree to emit to raw JavaScript
   *  @param ancestors Encoded names of the ancestors of the class (not only
   *                   parents), including the class itself.
   */
  def genClassDef(tree: LinkedClass): js.Tree = {
    implicit val pos = tree.pos
    val kind = tree.kind

    var reverseParts: List[js.Tree] = Nil

    reverseParts ::= genStaticMembers(tree)
    if (kind.isClass)
      reverseParts ::= genClass(tree)
    reverseParts ::= genInstanceTests(tree)
    reverseParts ::= genArrayInstanceTests(tree)
    reverseParts ::= genTypeData(tree)
    if (kind.isClass)
      reverseParts ::= genSetTypeData(tree)
    if (kind == ClassKind.ModuleClass)
      reverseParts ::= genModuleAccessor(tree)
    if (kind.isClass)
      reverseParts ::= genClassExports(tree)

    js.Block(reverseParts.reverse)
  }

  def genStaticMembers(tree: LinkedClass): js.Tree = {
    val className = tree.name.name
    val staticMemberDefs =
      tree.staticMethods.map(m => genMethod(className, m.tree))
    js.Block(staticMemberDefs)(tree.pos)
  }

  def genClass(tree: LinkedClass): js.Tree = {
    val className = tree.name.name
    val typeFunctionDef = genConstructor(tree)
    val memberDefs =
      tree.memberMethods.map(m => genMethod(className, m.tree))

    val exportedDefs = genExportedMembers(tree)

    js.Block(typeFunctionDef +: memberDefs :+ exportedDefs)(tree.pos)
  }

  /** Generates the JS constructor for a class. */
  def genConstructor(tree: LinkedClass): js.Tree = {
    assert(tree.kind.isClass)

    val classIdent = tree.name
    val className = classIdent.name
    val tpe = ClassType(className)

    assert(tree.superClass.isDefined || className == Definitions.ObjectClass,
        s"Class $className is missing a parent class")

    val ctorFun = {
      val superCtorCall = tree.superClass.fold[js.Tree] {
        js.Skip()(tree.pos)
      } { parentIdent =>
        implicit val pos = tree.pos
        js.Apply(
            js.DotSelect(encodeClassVar(parentIdent.name), js.Ident("call")),
            List(js.This()))
      }
      val fieldDefs = for {
        field @ FieldDef(name, ftpe, mutable) <- tree.fields
      } yield {
        implicit val pos = field.pos
        desugarJavaScript(
            Assign(Select(This()(tpe), name)(ftpe), zeroOf(ftpe)),
            semantics, outputMode)
      }
      js.Function(Nil,
          js.Block(superCtorCall :: fieldDefs)(tree.pos))(tree.pos)
    }

    {
      implicit val pos = tree.pos
      val typeVar = encodeClassVar(className)
      val docComment = js.DocComment("@constructor")
      val ctorDef = envFieldDef("c", className, ctorFun)

      val chainProto = tree.superClass.fold[js.Tree] {
        js.Skip()
      } { parentIdent =>
        js.Block(
          js.Assign(typeVar.prototype,
              js.New(envField("h", parentIdent.name, parentIdent.originalName), Nil)),
          genAddToPrototype(className, js.Ident("constructor"), typeVar)
        )
      }

      val inheritableCtorDef = {
        js.Block(
          js.DocComment("@constructor"),
          envFieldDef("h", className, js.Function(Nil, js.Skip())),
          js.Assign(envField("h", className).prototype, typeVar.prototype)
        )
      }

      js.Block(docComment, ctorDef, chainProto, inheritableCtorDef)
    }
  }

  /** Generates a method. */
  def genMethod(className: String, method: MethodDef): js.Tree = {
    implicit val pos = method.pos

    val paramEnv = Env.empty.withParams(method.args)
    val methodFun = js.Function(method.args.map(transformParamDef),
        desugarBody(method.body, method.resultType == NoType, paramEnv))

    if (method.static) {
      val Ident(methodName, origName) = method.name
      envFieldDef(
          "s", className + "__" + methodName, origName,
          methodFun)
    } else {
      genAddToPrototype(className, method.name, methodFun)
    }
  }

  /** Generates a property. */
  def genProperty(className: String, property: PropertyDef): js.Tree = {
    implicit val pos = property.pos
    val classType = ClassType(className)

    // defineProperty method
    val defProp =
      js.BracketSelect(js.VarRef(js.Ident("Object")),
          js.StringLiteral("defineProperty"))

    // class prototype
    val proto = encodeClassVar(className).prototype

    // property name
    val name = property.name match {
      case StringLiteral(value) =>
        js.StringLiteral(value)
      case id: Ident =>
        // We need to work around the closure compiler. Call propertyName to
        // get a string representation of the optimized name
        genCallHelper("propertyName",
            js.ObjectConstr(transformIdent(id) -> js.IntLiteral(0) :: Nil))
    }

    // Options passed to the defineProperty method
    val descriptor = js.ObjectConstr {
      // Basic config
      val base =
        js.StringLiteral("enumerable") -> js.BooleanLiteral(true) :: Nil

      // Optionally add getter
      val wget = {
        if (property.getterBody == EmptyTree) base
        else {
          val body = desugarBody(property.getterBody, isStat = false, Env.empty)
          js.StringLiteral("get") -> js.Function(Nil, body) :: base
        }
      }

      // Optionally add setter
      if (property.setterBody == EmptyTree) wget
      else {
        val env = Env.empty.withParams(property.setterArg :: Nil)
        val body = desugarBody(property.setterBody, isStat = true, env)
        js.StringLiteral("set") -> js.Function(
            transformParamDef(property.setterArg) :: Nil, body) :: wget
      }
    }

    js.Apply(defProp, proto :: name :: descriptor :: Nil)
  }

  /** Generate `classVar.prototype.name = value` */
  def genAddToPrototype(className: String, name: js.PropertyName,
      value: js.Tree)(implicit pos: Position): js.Tree = {
    val proto = encodeClassVar(className).prototype
    val select = name match {
      case name: js.Ident         => js.DotSelect(proto, name)
      case name: js.StringLiteral => js.BracketSelect(proto, name)
    }
    js.Assign(select, value)
  }

  /** Generate `classVar.prototype.name = value` */
  def genAddToPrototype(className: String, name: PropertyName,
      value: js.Tree)(implicit pos: Position): js.Tree = {
    val newName = name match {
      case ident: Ident         => transformIdent(ident)
      case StringLiteral(value) => js.StringLiteral(value)
    }
    genAddToPrototype(className, newName, value)
  }

  def genInstanceTests(tree: LinkedClass): js.Tree = {
    import Definitions._
    import TreeDSL._

    implicit val pos = tree.pos

    if (tree.kind.isClass || tree.kind == ClassKind.Interface ||
        tree.name.name == Definitions.StringClass) {
      val className = tree.name.name
      val displayName = decodeClassName(className)

      val isAncestorOfString =
        AncestorsOfStringClass.contains(className)
      val isAncestorOfHijackedNumberClass =
        AncestorsOfHijackedNumberClasses.contains(className)
      val isAncestorOfBoxedBooleanClass =
        AncestorsOfBoxedBooleanClass.contains(className)

      val objParam = js.ParamDef(Ident("obj"))
      val obj = objParam.ref

      val createIsStat = {
        envFieldDef("is", className,
          js.Function(List(objParam), js.Return(className match {
            case Definitions.ObjectClass =>
              js.BinaryOp(JSBinaryOp.!==, obj, js.Null())

            case Definitions.StringClass =>
              js.UnaryOp(JSUnaryOp.typeof, obj) === js.StringLiteral("string")

            case _ =>
              var test = (obj && (obj DOT "$classData") &&
                  (obj DOT "$classData" DOT "ancestors" DOT className))

              if (isAncestorOfString)
                test = test || (
                    js.UnaryOp(JSUnaryOp.typeof, obj) === js.StringLiteral("string"))
              if (isAncestorOfHijackedNumberClass)
                test = test || (
                    js.UnaryOp(JSUnaryOp.typeof, obj) === js.StringLiteral("number"))
              if (isAncestorOfBoxedBooleanClass)
                test = test || (
                    js.UnaryOp(JSUnaryOp.typeof, obj) === js.StringLiteral("boolean"))

              !(!test)
          })))
      }

      val createAsStat = if (semantics.asInstanceOfs == Unchecked) {
        js.Skip()
      } else {
        envFieldDef("as", className,
          js.Function(List(objParam), js.Return(className match {
            case Definitions.ObjectClass =>
              obj

            case _ =>
              js.If(js.Apply(envField("is", className), List(obj)) ||
                  (obj === js.Null()), {
                obj
              }, {
                genCallHelper("throwClassCastException",
                    obj, js.StringLiteral(displayName))
              })
        })))
      }

      js.Block(createIsStat, createAsStat)
    } else {
      js.Skip()
    }
  }

  def genArrayInstanceTests(tree: LinkedClass): js.Tree = {
    import Definitions._
    import TreeDSL._

    implicit val pos = tree.pos

    val className = tree.name.name
    val displayName = decodeClassName(className)

    val objParam = js.ParamDef(Ident("obj"))
    val obj = objParam.ref

    val depthParam = js.ParamDef(Ident("depth"))
    val depth = depthParam.ref

    val createIsArrayOfStat = {
      envFieldDef("isArrayOf", className,
        js.Function(List(objParam, depthParam), className match {
          case Definitions.ObjectClass =>
            val dataVarDef = js.VarDef(Ident("data"), {
              obj && (obj DOT "$classData")
            })
            val data = dataVarDef.ref
            js.Block(
              dataVarDef,
              js.If(!data, {
                js.Return(js.BooleanLiteral(false))
              }, {
                val arrayDepthVarDef = js.VarDef(Ident("arrayDepth"), {
                  (data DOT "arrayDepth") || js.IntLiteral(0)
                })
                val arrayDepth = arrayDepthVarDef.ref
                js.Block(
                  arrayDepthVarDef,
                  js.Return {
                    // Array[A] </: Array[Array[A]]
                    !js.BinaryOp(JSBinaryOp.<, arrayDepth, depth) && (
                      // Array[Array[A]] <: Array[Object]
                      js.BinaryOp(JSBinaryOp.>, arrayDepth, depth) ||
                      // Array[Int] </: Array[Object]
                      !js.BracketSelect(data DOT "arrayBase", js.StringLiteral("isPrimitive"))
                    )
                  })
              }))

          case _ =>
            js.Return(!(!(obj && (obj DOT "$classData") &&
                ((obj DOT "$classData" DOT "arrayDepth") === depth) &&
                (obj DOT "$classData" DOT "arrayBase" DOT "ancestors" DOT className))))
        }))
    }

    val createAsArrayOfStat = if (semantics.asInstanceOfs == Unchecked) {
      js.Skip()
    } else {
      envFieldDef("asArrayOf", className,
        js.Function(List(objParam, depthParam), js.Return {
          js.If(js.Apply(envField("isArrayOf", className), List(obj, depth)) ||
              (obj === js.Null()), {
            obj
          }, {
            genCallHelper("throwArrayCastException",
                obj, js.StringLiteral("L"+displayName+";"), depth)
          })
        }))
    }

    js.Block(createIsArrayOfStat, createAsArrayOfStat)
  }

  def genTypeData(tree: LinkedClass): js.Tree = {
    import Definitions._
    import TreeDSL._

    implicit val pos = tree.pos

    val classIdent = transformIdent(tree.name)
    val className = classIdent.name
    val kind = tree.kind

    val isObjectClass =
      className == ObjectClass
    val isHijackedBoxedClass =
      HijackedBoxedClasses.contains(className)
    val isAncestorOfHijackedClass =
      AncestorsOfHijackedClasses.contains(className)

    val parentData = if (globalInfo.isParentDataAccessed) {
      tree.superClass.fold[js.Tree] {
        if (isObjectClass) js.Null()
        else js.Undefined()
      } { parent =>
        envField("d", parent.name)
      }
    } else {
      js.Undefined()
    }

    val ancestorsRecord = js.ObjectConstr(
        tree.ancestors.map(ancestor => (js.Ident(ancestor), js.IntLiteral(1))))

    val typeData = js.New(envField("ClassTypeData"), (List(
        js.ObjectConstr(List(classIdent -> js.IntLiteral(0))),
        js.BooleanLiteral(kind == ClassKind.Interface),
        js.StringLiteral(semantics.runtimeClassName(tree)),
        ancestorsRecord,
        parentData
    ) ++ (
        // Optional parameter isInstance
        if (isObjectClass) {
          /* Object has special ScalaJS.is.O *and* ScalaJS.isArrayOf.O. */
          List(
            envField("is", className),
            envField("isArrayOf", className))
        } else if (isHijackedBoxedClass) {
          /* Hijacked boxed classes have a special isInstanceOf test. */
          val xParam = js.ParamDef(Ident("x"))
          List(js.Function(List(xParam), js.Return {
            genIsInstanceOf(xParam.ref, ClassType(className))
          }))
        } else if (isAncestorOfHijackedClass || className == StringClass) {
          /* java.lang.String and ancestors of hijacked classes have a normal
           * ScalaJS.is.pack_Class test but with a non-standard behavior. */
          List(envField("is", className))
        } else if (tree.kind == ClassKind.RawJSType) {
          /* Raw JS types have an instanceof operator-based isInstanceOf test
           * dictated by their jsName. If there is no jsName, the test cannot
           * be performed and must throw.
           */
          tree.jsName.fold[List[js.Tree]] {
            List(envField("noIsInstance"))
          } { jsName =>
            val jsCtor = jsName.split("\\.").foldLeft(envField("g")) {
              (prev, part) => js.BracketSelect(prev, js.StringLiteral(part))
            }
            List(js.Function(List(js.ParamDef(Ident("x"))), js.Return {
              js.BinaryOp(JSBinaryOp.instanceof, js.VarRef(Ident("x")), jsCtor)
            }))
          }
        } else {
          // For other classes, the isInstance function can be inferred.
          Nil
        }
    )).reverse.dropWhile(_.isInstanceOf[js.Undefined]).reverse)

    envFieldDef("d", className, typeData)
  }

  def genSetTypeData(tree: LinkedClass): js.Tree = {
    import TreeDSL._

    implicit val pos = tree.pos

    assert(tree.kind.isClass)

    encodeClassVar(tree.name.name).prototype DOT "$classData" :=
      envField("d", tree.name.name)
  }

  def genModuleAccessor(tree: LinkedClass): js.Tree = {
    import TreeDSL._

    implicit val pos = tree.pos

    val classIdent = transformIdent(tree.name)
    val className = classIdent.name
    val tpe = ClassType(className)

    require(tree.kind == ClassKind.ModuleClass,
        s"genModuleAccessor called with non-module class: $className")

    val createModuleInstanceField = {
      envFieldDef("n", className, js.Undefined())
    }

    val createAccessor = {
      val moduleInstanceVar = envField("n", className)

      val assignModule = {
        moduleInstanceVar :=
          js.Apply(js.New(encodeClassVar(className), Nil) DOT js.Ident("init___"), Nil)
      }

      val initBlock = semantics.moduleInit match {
        case CheckedBehavior.Unchecked =>
          js.If(!(moduleInstanceVar), assignModule, js.Skip())
        case CheckedBehavior.Compliant =>
          js.If(moduleInstanceVar === js.Undefined(),
            js.Block(
                moduleInstanceVar := js.Null(),
                assignModule
            ),
            js.Skip())
        case CheckedBehavior.Fatal =>
          js.If(moduleInstanceVar === js.Undefined(), {
            js.Block(
                moduleInstanceVar := js.Null(),
                assignModule
            )
          }, js.If(moduleInstanceVar === js.Null(), {
            // throw new UndefinedBehaviorError(
            //     "Initializer of $className called before completion of its" +
            //     "super constructor")
            val decodedName = Definitions.decodeClassName(className).stripSuffix("$")
            val msg = s"Initializer of $decodedName called before completion " +
              "of its super constructor"
            val obj = js.New(encodeClassVar("sjsr_UndefinedBehaviorError"), Nil)
            val ctor = obj DOT js.Ident("init___T")
            js.Throw(js.Apply(ctor, js.StringLiteral(msg) :: Nil))
          }, js.Skip()))
      }

      envFieldDef("m", className, js.Function(Nil, js.Block(
        initBlock, js.Return(moduleInstanceVar)
      )))
    }

    js.Block(createModuleInstanceField, createAccessor)
  }

  def genExportedMembers(tree: LinkedClass): js.Tree = {
    val exports = tree.exportedMembers map { member =>
      member.tree match {
        case m: MethodDef =>
          genMethod(tree.encodedName, m)
        case p: PropertyDef =>
          genProperty(tree.encodedName, p)
        case tree =>
          throw new AssertionError(
              "Illegal exportedMember " + tree.getClass.getName)
      }
    }

    js.Block(exports)(tree.pos)
  }

  def genClassExports(tree: LinkedClass): js.Tree = {
    val exports = tree.classExports collect {
      case e: ConstructorExportDef =>
        genConstructorExportDef(tree, e)
      case e: ModuleExportDef =>
        genModuleExportDef(tree, e)
    }

    js.Block(exports)(tree.pos)
  }

  def genConstructorExportDef(cd: LinkedClass,
      tree: ConstructorExportDef): js.Tree = {
    import TreeDSL._

    implicit val pos = tree.pos
    val classType = ClassType(cd.name.name)
    val ConstructorExportDef(fullName, args, body) = tree

    val baseCtor = envField("c", cd.name.name, cd.name.originalName)
    val (createNamespace, expCtorVar) = genCreateNamespaceInExports(fullName)

    js.Block(
      createNamespace,
      js.DocComment("@constructor"),
      expCtorVar := js.Function(args.map(transformParamDef), js.Block(
        js.Apply(js.DotSelect(baseCtor, js.Ident("call")), List(js.This())),
        desugarBody(body, isStat = true, Env.empty.withParams(args))
      )),
      expCtorVar DOT "prototype" := baseCtor DOT "prototype"
    )
  }

  def genModuleExportDef(cd: LinkedClass, tree: ModuleExportDef): js.Tree = {
    import TreeDSL._

    implicit val pos = tree.pos

    val baseAccessor =
      envField("m", cd.name.name)
    val (createNamespace, expAccessorVar) =
      genCreateNamespaceInExports(tree.fullName)

    js.Block(
      createNamespace,
      expAccessorVar := baseAccessor
    )
  }

  // Helpers

  /** Desugars a function body of the IR into ES5 JavaScript. */
  private def desugarBody(tree: Tree, isStat: Boolean, env: Env): js.Tree = {
    implicit val pos = tree.pos
    val withReturn =
      if (isStat) tree
      else Return(tree)
    desugarJavaScript(withReturn, semantics, outputMode, env) match {
      case js.Block(stats :+ js.Return(js.Undefined())) => js.Block(stats)
      case other                                        => other
    }
  }

  /** Gen JS code for assigning an rhs to a qualified name in the exports scope.
   *  For example, given the qualified name "foo.bar.Something", generates:
   *
   *  ScalaJS.e["foo"] = ScalaJS.e["foo"] || {};
   *  ScalaJS.e["foo"]["bar"] = ScalaJS.e["foo"]["bar"] || {};
   *
   *  Returns (statements, ScalaJS.e["foo"]["bar"]["Something"])
   */
  private def genCreateNamespaceInExports(qualName: String)(
      implicit pos: Position): (js.Tree, js.Tree) = {
    val parts = qualName.split("\\.")
    val statements = List.newBuilder[js.Tree]
    var namespace = envField("e")
    for (i <- 0 until parts.length-1) {
      namespace = js.BracketSelect(namespace, js.StringLiteral(parts(i)))
      statements +=
        js.Assign(namespace, js.BinaryOp(JSBinaryOp.||,
            namespace, js.ObjectConstr(Nil)))
    }
    val lhs = js.BracketSelect(namespace, js.StringLiteral(parts.last))
    (js.Block(statements.result()), lhs)
  }

}
