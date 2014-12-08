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

/** Defines methods to emit Scala.js classes to JavaScript code.
 *  The results are completely desugared.
 */
final class ScalaJSClassEmitter(semantics: Semantics) {

  import JSDesugaring._

  /** Desugar a Scala.js class into ECMAScript 5 constructs
   *
   *  @param tree The IR tree to emit to raw JavaScript
   *  @param ancestors Encoded names of the ancestors of the class (not only
   *                   parents), including the class itself.
   */
  def genClassDef(tree: ClassDef, ancestors: List[String]): js.Tree = {
    implicit val pos = tree.pos
    val kind = tree.kind

    var reverseParts: List[js.Tree] = Nil

    reverseParts ::= genStaticMembers(tree)
    if (kind.isClass)
      reverseParts ::= genClass(tree)
    if (kind.isClass || kind == ClassKind.Interface ||
        tree.name.name == Definitions.StringClass)
      reverseParts ::= genInstanceTests(tree)
    reverseParts ::= genArrayInstanceTests(tree)
    reverseParts ::= genTypeData(tree, ancestors)
    if (kind.isClass)
      reverseParts ::= genSetTypeData(tree)
    if (kind == ClassKind.ModuleClass)
      reverseParts ::= genModuleAccessor(tree)
    if (kind.isClass)
      reverseParts ::= genClassExports(tree)

    js.Block(reverseParts.reverse)
  }

  def genStaticMembers(tree: ClassDef): js.Tree = {
    val className = tree.name.name
    val staticMemberDefs = tree.defs collect {
      case m: MethodDef if m.static =>
        genMethod(className, m)
    }
    js.Block(staticMemberDefs)(tree.pos)
  }

  def genClass(tree: ClassDef): js.Tree = {
    val className = tree.name.name
    val typeFunctionDef = genConstructor(tree)
    val memberDefs = tree.defs collect {
      case m: MethodDef if !m.static =>
        genMethod(className, m)
      case p: PropertyDef =>
        genProperty(className, p)
    }

    js.Block(typeFunctionDef :: memberDefs)(tree.pos)
  }

  /** Generates the JS constructor for a class. */
  def genConstructor(tree: ClassDef): js.Tree = {
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
        field @ VarDef(name, vtpe, mutable, rhs) <- tree.defs
      } yield {
        implicit val pos = field.pos
        desugarJavaScript(
            Assign(Select(This()(tpe), name, mutable)(vtpe), rhs),
            semantics)
      }
      js.Function(Nil,
          js.Block(superCtorCall :: fieldDefs)(tree.pos))(tree.pos)
    }

    {
      implicit val pos = tree.pos
      val typeVar = encodeClassVar(className)
      val docComment = js.DocComment("@constructor")
      val ctorDef = js.Assign(typeVar, ctorFun)

      val chainProto = tree.superClass.fold[js.Tree] {
        js.Skip()
      } { parentIdent =>
        js.Block(
          js.Assign(typeVar.prototype,
              js.New(js.DotSelect(envField("h"), parentIdent), Nil)),
          genAddToPrototype(className, js.Ident("constructor"), typeVar)
        )
      }

      val inheritableCtorDef = {
        val inheritableCtorVar =
          js.DotSelect(envField("h"), classIdent)
        js.Block(
          js.DocComment("@constructor"),
          js.Assign(inheritableCtorVar, js.Function(Nil, js.Skip())),
          js.Assign(inheritableCtorVar.prototype, typeVar.prototype)
        )
      }

      js.Block(docComment, ctorDef, chainProto, inheritableCtorDef)
    }
  }

  /** Generates a method. */
  def genMethod(className: String, method: MethodDef): js.Tree = {
    implicit val pos = method.pos

    val methodFun = js.Function(method.args.map(transformParamDef),
        desugarBody(method.body, method.resultType == NoType))

    if (method.static) {
      val Ident(methodName, origName) = method.name
      js.Assign(
          js.DotSelect(envField("s"),
              js.Ident(className + "__" + methodName, origName)),
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
      js.BracketSelect(js.VarRef(js.Ident("Object"), false),
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
      val wget =
        if (property.getterBody == EmptyTree) base
        else js.StringLiteral("get") ->
          js.Function(Nil, desugarBody(property.getterBody, isStat = false)) :: base

      // Optionally add setter
      if (property.setterBody == EmptyTree) wget
      else js.StringLiteral("set") ->
          js.Function(transformParamDef(property.setterArg) :: Nil,
              desugarBody(property.setterBody, isStat = true)) :: wget
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

  def genInstanceTests(tree: ClassDef): js.Tree = {
    import Definitions._
    import TreeDSL._

    implicit val pos = tree.pos

    val classIdent = transformIdent(tree.name)
    val className = classIdent.name
    val displayName = decodeClassName(className)

    val isAncestorOfString =
      AncestorsOfStringClass.contains(className)
    val isAncestorOfHijackedNumberClass =
      AncestorsOfHijackedNumberClasses.contains(className)
    val isAncestorOfBoxedBooleanClass =
      AncestorsOfBoxedBooleanClass.contains(className)

    val objParam = js.ParamDef(Ident("obj"), mutable = false)
    val obj = objParam.ref

    val createIsStat = {
      envField("is") DOT classIdent :=
        js.Function(List(objParam), js.Return(className match {
          case Definitions.ObjectClass =>
            js.BinaryOp("!==", obj, js.Null())

          case Definitions.StringClass =>
            js.UnaryOp("typeof", obj) === js.StringLiteral("string")

          case _ =>
            var test = (obj && (obj DOT "$classData") &&
                (obj DOT "$classData" DOT "ancestors" DOT classIdent))

            if (isAncestorOfString)
              test = test || (
                  js.UnaryOp("typeof", obj) === js.StringLiteral("string"))
            if (isAncestorOfHijackedNumberClass)
              test = test || (
                  js.UnaryOp("typeof", obj) === js.StringLiteral("number"))
            if (isAncestorOfBoxedBooleanClass)
              test = test || (
                  js.UnaryOp("typeof", obj) === js.StringLiteral("boolean"))

            !(!test)
        }))
    }

    val createAsStat = if (semantics.asInstanceOfs == Unchecked) {
      js.Skip()
    } else {
      envField("as") DOT classIdent :=
        js.Function(List(objParam), js.Return(className match {
          case Definitions.ObjectClass =>
            obj

          case _ =>
            js.If(js.Apply(envField("is") DOT classIdent, List(obj)) ||
                (obj === js.Null()), {
              obj
            }, {
              genCallHelper("throwClassCastException",
                  obj, js.StringLiteral(displayName))
            })
      }))
    }

    js.Block(createIsStat, createAsStat)
  }

  def genArrayInstanceTests(tree: ClassDef): js.Tree = {
    import Definitions._
    import TreeDSL._

    implicit val pos = tree.pos

    val classIdent = transformIdent(tree.name)
    val className = classIdent.name
    val displayName = decodeClassName(className)

    val objParam = js.ParamDef(Ident("obj"), mutable = false)
    val obj = objParam.ref

    val depthParam = js.ParamDef(Ident("depth"), mutable = false)
    val depth = depthParam.ref

    val createIsArrayOfStat = {
      envField("isArrayOf") DOT classIdent :=
        js.Function(List(objParam, depthParam), className match {
          case Definitions.ObjectClass =>
            val dataVarDef = js.VarDef(Ident("data"), false, {
              obj && (obj DOT "$classData")
            })
            val data = dataVarDef.ref
            js.Block(
              dataVarDef,
              js.If(!data, {
                js.Return(js.BooleanLiteral(false))
              }, {
                val arrayDepthVarDef = js.VarDef(Ident("arrayDepth"), false, {
                  (data DOT "arrayDepth") || js.IntLiteral(0)
                })
                val arrayDepth = arrayDepthVarDef.ref
                js.Block(
                  arrayDepthVarDef,
                  js.Return {
                    // Array[A] </: Array[Array[A]]
                    !js.BinaryOp("<", arrayDepth, depth) && (
                      // Array[Array[A]] <: Array[Object]
                      js.BinaryOp(">", arrayDepth, depth) ||
                      // Array[Int] </: Array[Object]
                      !js.BracketSelect(data DOT "arrayBase", js.StringLiteral("isPrimitive"))
                    )
                  })
              }))

          case _ =>
            js.Return(!(!(obj && (obj DOT "$classData") &&
                ((obj DOT "$classData" DOT "arrayDepth") === depth) &&
                (obj DOT "$classData" DOT "arrayBase" DOT "ancestors" DOT classIdent))))
        })
    }

    val createAsArrayOfStat = if (semantics.asInstanceOfs == Unchecked) {
      js.Skip()
    } else {
      envField("asArrayOf") DOT classIdent :=
        js.Function(List(objParam, depthParam), js.Return {
          js.If(js.Apply(envField("isArrayOf") DOT classIdent, List(obj, depth)) ||
              (obj === js.Null()), {
            obj
          }, {
            genCallHelper("throwArrayCastException",
                obj, js.StringLiteral("L"+displayName+";"), depth)
          })
        })
    }

    js.Block(createIsArrayOfStat, createAsArrayOfStat)
  }

  def genTypeData(tree: ClassDef, ancestors: List[String]): js.Tree = {
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

    val parentData = tree.superClass.fold[js.Tree] {
      if (isObjectClass) js.Null()
      else js.Undefined()
    } { parent =>
      envField("d") DOT parent
    }

    val ancestorsRecord = js.ObjectConstr(
        ancestors.map(ancestor => (js.Ident(ancestor), js.IntLiteral(1))))

    val typeData = js.New(envField("ClassTypeData"), List(
        js.ObjectConstr(List(classIdent -> js.IntLiteral(0))),
        js.BooleanLiteral(kind == ClassKind.Interface),
        js.StringLiteral(decodeClassName(className)),
        parentData,
        ancestorsRecord
    ) ++ (
        // Optional parameter isInstance
        if (isObjectClass) {
          /* Object has special ScalaJS.is.O *and* ScalaJS.isArrayOf.O. */
          List(
            envField("is") DOT classIdent,
            envField("isArrayOf") DOT classIdent)
        } else if (isHijackedBoxedClass) {
          /* Hijacked boxed classes have a special isInstanceOf test. */
          val xParam = js.ParamDef(Ident("x"), mutable = false)
          List(js.Function(List(xParam), js.Return {
            genIsInstanceOf(xParam.ref, ClassType(className))
          }))
        } else if (isAncestorOfHijackedClass || className == StringClass) {
          /* java.lang.String and ancestors of hijacked classes have a normal
           * ScalaJS.is.pack_Class test but with a non-standard behavior. */
          List(envField("is") DOT classIdent)
        } else {
          // For other classes, the isInstance function can be inferred.
          Nil
        }
    ))

    envField("d") DOT classIdent := typeData
  }

  def genSetTypeData(tree: ClassDef): js.Tree = {
    import TreeDSL._

    implicit val pos = tree.pos

    assert(tree.kind.isClass)

    encodeClassVar(tree.name.name).prototype DOT "$classData" :=
      envField("d") DOT tree.name
  }

  def genModuleAccessor(tree: ClassDef): js.Tree = {
    import TreeDSL._

    implicit val pos = tree.pos

    val classIdent = transformIdent(tree.name)
    val className = classIdent.name
    val tpe = ClassType(className)

    require(tree.kind == ClassKind.ModuleClass,
        s"genModuleAccessor called with non-module class: $className")
    assert(className.endsWith("$"))

    val moduleName = className.dropRight(1)
    val moduleIdent = Ident(moduleName)

    val moduleInstanceVar = envField("n") DOT moduleIdent
    val accessorVar = envField("m") DOT moduleIdent

    val createModuleInstanceField = {
      moduleInstanceVar := js.Undefined()
    }

    val createAccessor = {
      accessorVar := js.Function(Nil, js.Block(
        js.If(!(moduleInstanceVar), {
          moduleInstanceVar :=
            js.Apply(js.New(encodeClassVar(className), Nil) DOT js.Ident("init___"), Nil)
        }, js.Skip()),
        js.Return(moduleInstanceVar)
      ))
    }

    js.Block(createModuleInstanceField, createAccessor)
  }

  def genClassExports(tree: ClassDef): js.Tree = {
    val exports = tree.defs collect {
      case e: ConstructorExportDef =>
        genConstructorExportDef(tree, e)
      case e: ModuleExportDef =>
        genModuleExportDef(tree, e)
    }

    js.Block(exports)(tree.pos)
  }

  def genConstructorExportDef(cd: ClassDef, tree: ConstructorExportDef): js.Tree = {
    import TreeDSL._

    implicit val pos = tree.pos
    val classType = ClassType(cd.name.name)
    val ConstructorExportDef(fullName, args, body) = tree

    val baseCtor = envField("c") DOT cd.name
    val (createNamespace, expCtorVar) = genCreateNamespaceInExports(fullName)

    js.Block(
      createNamespace,
      js.DocComment("@constructor"),
      expCtorVar := js.Function(args.map(transformParamDef), js.Block(
        js.Apply(js.DotSelect(baseCtor, js.Ident("call")), List(js.This())),
        desugarBody(body, isStat = true)
      )),
      expCtorVar DOT "prototype" := baseCtor DOT "prototype"
    )
  }

  def genModuleExportDef(cd: ClassDef, tree: ModuleExportDef): js.Tree = {
    import TreeDSL._

    implicit val pos = tree.pos

    val baseAccessor =
      envField("m") DOT cd.name.name.dropRight(1)
    val (createNamespace, expAccessorVar) =
      genCreateNamespaceInExports(tree.fullName)

    js.Block(
      createNamespace,
      expAccessorVar := baseAccessor
    )
  }

  /** Generate a dummy parent. Used by ScalaJSOptimizer */
  def genDummyParent(name: String): js.Tree = {
    implicit val pos = Position.NoPosition

    js.Block(
        js.DocComment("@constructor (dummy parent)"))
        js.Assign(js.DotSelect(envField("h"), js.Ident(name)),
            js.Function(Nil, js.Skip())
    )
  }

  // Helpers

  /** Desugars a function body of the IR into ES5 JavaScript. */
  private def desugarBody(tree: Tree, isStat: Boolean): js.Tree = {
    implicit val pos = tree.pos
    val withReturn =
      if (isStat) tree
      else Return(tree)
    desugarJavaScript(withReturn, semantics) match {
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
        js.Assign(namespace, js.BinaryOp("||", namespace, js.ObjectConstr(Nil)))
    }
    val lhs = js.BracketSelect(namespace, js.StringLiteral(parts.last))
    (js.Block(statements.result()), lhs)
  }

}
