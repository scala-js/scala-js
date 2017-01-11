/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.linker.backend.emitter

import org.scalajs.core.ir._
import Position._
import Transformers._
import org.scalajs.core.ir.Trees._
import Types._

import org.scalajs.core.tools.sem._
import CheckedBehavior.Unchecked

import org.scalajs.core.tools.javascript.{Trees => js}
import org.scalajs.core.tools.linker.backend.OutputMode
import org.scalajs.core.tools.linker.{LinkedClass, LinkingUnit}

/** Emitter for the skeleton of classes. */
private[emitter] final class ScalaJSClassEmitter(semantics: Semantics,
    outputMode: OutputMode, internalOptions: InternalOptions) {

  private val jsDesugaring =
    new JSDesugaring(semantics, outputMode, internalOptions)

  import ScalaJSClassEmitter._
  import jsDesugaring._

  def this(semantics: Semantics, outputMode: OutputMode) =
    this(semantics, outputMode, InternalOptions())

  private implicit def implicitOutputMode: OutputMode = outputMode

  /** Desugars a Scala.js class specifically for use by the Rhino interpreter.
   *
   *  @param tree The IR tree to emit to raw JavaScript
   */
  def genClassDefForRhino(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {

    implicit val pos = tree.pos
    val kind = tree.kind

    var reverseParts: List[js.Tree] = Nil

    reverseParts ::= genStaticMembers(tree)
    if (kind == ClassKind.Interface)
      reverseParts ::= genDefaultMethods(tree)
    if (kind.isAnyScalaJSDefinedClass && tree.hasInstances)
      reverseParts ::= genClassForRhino(tree)
    if (needInstanceTests(tree)) {
      reverseParts ::= genInstanceTests(tree)
      reverseParts ::= genArrayInstanceTests(tree)
    }
    if (tree.hasRuntimeTypeInfo)
      reverseParts ::= genTypeData(tree)
    if (kind.isClass && tree.hasInstances && tree.hasRuntimeTypeInfo)
      reverseParts ::= genSetTypeData(tree)
    if (kind.hasModuleAccessor)
      reverseParts ::= genModuleAccessor(tree)
    if (!kind.isJSType) {
      reverseParts ::= genCreateStaticFieldsOfScalaClass(tree)
      reverseParts ::= genStaticInitialization(tree)
    }
    reverseParts ::= genClassExports(tree)

    js.Block(reverseParts.reverse)
  }

  def genStaticMembers(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {
    val className = tree.name.name
    val staticMemberDefs =
      tree.staticMethods.map(m => genMethod(className, m.tree))
    js.Block(staticMemberDefs)(tree.pos)
  }

  def genDefaultMethods(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {
    val className = tree.name.name
    val defaultMethodDefs =
      tree.memberMethods.map(m => genDefaultMethod(className, m.tree))
    js.Block(defaultMethodDefs)(tree.pos)
  }

  private def genClassForRhino(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {

    val className = tree.name.name
    val ctor = genConstructor(tree)
    val memberDefs =
      tree.memberMethods.map(m => genMethod(className, m.tree))
    val exportedDefs = genExportedMembers(tree)

    buildClass(tree, ctor, memberDefs, exportedDefs)
  }

  def buildClass(tree: LinkedClass, ctor: js.Tree, memberDefs: List[js.Tree],
      exportedDefs: js.Tree)(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {
    val className = tree.name.name
    val allDefsBlock =
      js.Block(ctor +: memberDefs :+ exportedDefs)(tree.pos)

    val entireClassDef = outputMode match {
      case OutputMode.ECMAScript51Global | OutputMode.ECMAScript51Isolated =>
        allDefsBlock

      case OutputMode.ECMAScript6 =>
        val allDefs = allDefsBlock match {
          case js.Block(allDefs) => allDefs
          case js.Skip()         => Nil
          case oneDef            => List(oneDef)
        }
        genES6Class(tree, allDefs)
    }

    if (!tree.kind.isJSClass) {
      entireClassDef
    } else {
      // Wrap the entire class def in an accessor function
      import TreeDSL._
      implicit val pos = tree.pos

      val createClassValueVar =
        envFieldDef("b", className, js.Undefined(), mutable = true)

      val createAccessor = {
        val classValueVar = envField("b", className)

        val body = js.Block(
            js.If(!classValueVar, {
              js.Block(
                  entireClassDef,
                  genCreateStaticFieldsOfJSClass(tree),
                  classValueVar := envField("c", className),
                  genStaticInitialization(tree)
              )
            }, {
              js.Skip()
            }),
            js.Return(classValueVar)
        )

        envFieldDef("a", className, js.Function(Nil, body))
      }

      js.Block(createClassValueVar, createAccessor)
    }
  }

  /** Generates an ECMAScript 6 class for a linked class. */
  def genES6Class(tree: LinkedClass, members: List[js.Tree])(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {

    require(outputMode == OutputMode.ECMAScript6)

    val className = tree.name.name
    val classIdent =
      encodeClassVar(className)(tree.name.pos).asInstanceOf[js.VarRef].ident

    val parentVar = for (parentIdent <- tree.superClass) yield {
      implicit val pos = parentIdent.pos
      if (!tree.kind.isJSClass)
        encodeClassVar(parentIdent.name)
      else
        genRawJSClassConstructor(parentIdent.name)
    }

    js.ClassDef(Some(classIdent), parentVar, members)(tree.pos)
  }

  /** Generates the JS constructor for a class. */
  def genConstructor(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {

    assert(tree.kind.isAnyScalaJSDefinedClass)
    assert(tree.superClass.isDefined || tree.name.name == Definitions.ObjectClass,
        s"Class ${tree.name.name} is missing a parent class")

    outputMode match {
      case OutputMode.ECMAScript51Global | OutputMode.ECMAScript51Isolated =>
        genES5Constructor(tree)

      case OutputMode.ECMAScript6 =>
        genES6Constructor(tree)
    }
  }

  /** Generates the JS constructor for a class, ES5 style. */
  private def genES5Constructor(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {
    implicit val pos = tree.pos

    val className = tree.name.name
    val isJSClass = tree.kind.isJSClass

    def makeInheritableCtorDef(ctorToMimic: js.Tree) = {
      js.Block(
        js.DocComment("@constructor"),
        envFieldDef("h", className, None, js.Function(Nil, js.Skip()),
            mutable = false, keepFunctionExpression = isJSClass),
        js.Assign(envField("h", className).prototype, ctorToMimic.prototype)
      )
    }

    val ctorFun = if (!isJSClass) {
      val superCtorCall = tree.superClass.fold[js.Tree] {
        js.Skip()
      } { parentIdent =>
        js.Apply(
            js.DotSelect(encodeClassVar(parentIdent.name), js.Ident("call")),
            List(js.This()))
      }
      val fieldDefs = genFieldDefs(tree)
      js.Function(Nil, js.Block(superCtorCall :: fieldDefs))
    } else {
      genConstructorFunForJSClass(tree)
    }

    val typeVar = encodeClassVar(className)
    val docComment = js.DocComment("@constructor")
    val ctorDef = envFieldDef("c", className, None, ctorFun, mutable = false,
        keepFunctionExpression = isJSClass)

    val chainProto = tree.superClass.fold[js.Tree] {
      js.Skip()
    } { parentIdent =>
      val (inheritedCtorDef, inheritedCtorRef) = if (!isJSClass) {
        (js.Skip(), envField("h", parentIdent.name))
      } else {
        val superCtor = genRawJSClassConstructor(parentIdent.name)
        (makeInheritableCtorDef(superCtor), envField("h", className))
      }
      js.Block(
          inheritedCtorDef,
          js.Assign(typeVar.prototype, js.New(inheritedCtorRef, Nil)),
          genAddToPrototype(className, js.StringLiteral("constructor"), typeVar)
      )
    }

    val inheritableCtorDef =
      if (isJSClass) js.Skip()
      else makeInheritableCtorDef(typeVar)

    js.Block(docComment, ctorDef, chainProto, inheritableCtorDef)
  }

  /** Generates the JS constructor for a class, ES6 style. */
  private def genES6Constructor(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {
    implicit val pos = tree.pos

    if (tree.kind.isJSClass) {
      val js.Function(params, body) = genConstructorFunForJSClass(tree)
      js.MethodDef(static = false, js.Ident("constructor"), params, body)
    } else {
      val fieldDefs = genFieldDefs(tree)
      if (fieldDefs.isEmpty && outputMode == OutputMode.ECMAScript6) {
        js.Skip()
      } else {
        val superCtorCall = tree.superClass.fold[js.Tree] {
          js.Skip()(tree.pos)
        } { parentIdent =>
          js.Apply(js.Super(), Nil)
        }
        js.MethodDef(static = false, js.Ident("constructor"), Nil,
            js.Block(superCtorCall :: fieldDefs))
      }
    }
  }

  private def genConstructorFunForJSClass(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): js.Function = {
    implicit val pos = tree.pos

    require(tree.kind.isJSClass)

    tree.exportedMembers.map(_.tree) collectFirst {
      case MethodDef(false, StringLiteral("constructor"), params, _, body) =>
        desugarToFunction(tree.encodedName, params, body.get, isStat = true)
    } getOrElse {
      throw new IllegalArgumentException(
          s"${tree.encodedName} does not have an exported constructor")
    }
  }

  /** Generates the creation of fields for a class. */
  private def genFieldDefs(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): List[js.Tree] = {
    val tpe = ClassType(tree.encodedName)
    for {
      field @ FieldDef(false, name, ftpe, mutable) <- tree.fields
    } yield {
      implicit val pos = field.pos
      val selectField = (name: @unchecked) match {
        case name: Ident => Select(This()(tpe), name)(ftpe)
      }
      desugarTree(Some(tree.encodedName),
          Assign(selectField, zeroOf(ftpe)), isStat = true)
    }
  }

  /** Generates the creation of the static fields for a Scala class. */
  def genCreateStaticFieldsOfScalaClass(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {
    val className = tree.encodedName
    val stats = for {
      field @ FieldDef(true, Ident(name, origName), ftpe, mutable) <- tree.fields
    } yield {
      implicit val pos = field.pos
      val fullName = className + "__" + name
      envFieldDef("t", fullName, origName, genZeroOf(ftpe), mutable)
    }
    js.Block(stats)(tree.pos)
  }

  /** Generates the creation of the static fields for a JavaScript class. */
  private def genCreateStaticFieldsOfJSClass(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {
    val className = tree.encodedName
    val stats = for {
      field @ FieldDef(true, name, ftpe, mutable) <- tree.fields
    } yield {
      implicit val pos = field.pos
      val classVar = envField("c", className)
      val select = genPropSelect(classVar, genPropertyName(name))
      js.Assign(select, genZeroOf(ftpe))
    }
    js.Block(stats)(tree.pos)
  }

  /** Generates the static initializer invocation of a JavaScript class. */
  def genStaticInitialization(tree: LinkedClass): js.Tree = {
    import Definitions.StaticInitializerName
    implicit val pos = tree.pos
    if (tree.staticMethods.exists(_.tree.name.encodedName == StaticInitializerName)) {
      val fullName = tree.encodedName + "__" + StaticInitializerName
      js.Apply(envField("s", fullName, Some("<clinit>")), Nil)
    } else {
      js.Skip()
    }
  }

  /** Generates a method. */
  def genMethod(className: String, method: MethodDef)(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {
    val methodBody = method.body.getOrElse(
        throw new AssertionError("Cannot generate an abstract method"))

    implicit val pos = method.pos

    val methodFun0 = desugarToFunction(className,
        method.args, methodBody, method.resultType == NoType)

    val methodFun = if (Definitions.isConstructorName(method.name.encodedName)) {
      // init methods have to return `this` so that we can chain them to `new`
      js.Function(methodFun0.args, {
        implicit val pos = methodFun0.body.pos
        js.Block(
            methodFun0.body,
            js.Return(js.This()))
      })(methodFun0.pos)
    } else {
      methodFun0
    }

    if (method.static) {
      method.name match {
        case Ident(methodName, origName) =>
          envFieldDef(
              "s", className + "__" + methodName, origName,
              methodFun)

        case methodName =>
          outputMode match {
            case OutputMode.ECMAScript51Global | OutputMode.ECMAScript51Isolated =>
              genAddToObject(className, encodeClassVar(className), methodName,
                  methodFun)

            case OutputMode.ECMAScript6 =>
              js.MethodDef(static = true, genPropertyName(methodName),
                  methodFun.args, methodFun.body)
          }
      }
    } else {
      outputMode match {
        case OutputMode.ECMAScript51Global | OutputMode.ECMAScript51Isolated =>
          genAddToPrototype(className, method.name, methodFun)

        case OutputMode.ECMAScript6 =>
          js.MethodDef(static = false, genPropertyName(method.name),
              methodFun.args, methodFun.body)
      }
    }
  }

  /** Generates a default method. */
  def genDefaultMethod(className: String, method: MethodDef)(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {
    implicit val pos = method.pos

    /* TODO The identifier `$thiz` cannot be produced by 0.6.x compilers due to
     * their name mangling, which guarantees that it is unique. We should find
     * a better way to do this in the future, though.
     */
    val thisIdent = js.Ident("$thiz", Some("this"))

    val methodFun0 = desugarToFunction(className, Some(thisIdent),
        method.args, method.body.get, method.resultType == NoType)

    val methodFun = js.Function(
        js.ParamDef(thisIdent, rest = false) :: methodFun0.args,
        methodFun0.body)(methodFun0.pos)

    val Ident(methodName, origName) = method.name

    envFieldDef(
        "f", className + "__" + methodName, origName,
        methodFun)
  }

  /** Generates a property. */
  def genProperty(className: String, property: PropertyDef)(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {
    outputMode match {
      case OutputMode.ECMAScript51Global | OutputMode.ECMAScript51Isolated =>
        genPropertyES5(className, property)
      case OutputMode.ECMAScript6 =>
        genPropertyES6(className, property)
    }
  }

  private def genPropertyES5(className: String, property: PropertyDef)(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {
    implicit val pos = property.pos

    // defineProperty method
    val defProp =
      genIdentBracketSelect(js.VarRef(js.Ident("Object")), "defineProperty")

    // class prototype
    val classVar = encodeClassVar(className)
    val targetObject =
      if (property.static) classVar
      else classVar.prototype

    // property name
    val name = genPropertyName(property.name) match {
      case value: js.StringLiteral => value
      case js.ComputedName(tree)   => tree

      case id: js.Ident =>
        // We need to work around the closure compiler. Call propertyName to
        // get a string representation of the optimized name
        genCallHelper("propertyName",
            js.ObjectConstr(id -> js.IntLiteral(0) :: Nil))
    }

    // optional getter definition
    val optGetter = property.getterBody map { body =>
      val fun = desugarToFunction(className, Nil, body, isStat = false)
      js.StringLiteral("get") -> fun
    }

    // optional setter definition
    val optSetter = property.setterArgAndBody map { case (arg, body) =>
      val fun = desugarToFunction(className, arg :: Nil, body, isStat = true)
      js.StringLiteral("set") -> fun
    }

    // Options passed to the defineProperty method
    val descriptor = js.ObjectConstr(
      optGetter.toList ++
      optSetter ++
      List(js.StringLiteral("configurable") -> js.BooleanLiteral(true))
    )

    js.Apply(defProp, targetObject :: name :: descriptor :: Nil)
  }

  private def genPropertyES6(className: String, property: PropertyDef)(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {
    implicit val pos = property.pos

    val static = property.static
    val propName = genPropertyName(property.name)

    val getter = property.getterBody.fold[js.Tree] {
      js.Skip()
    } { body =>
      val fun = desugarToFunction(className, Nil, body, isStat = false)
      js.GetterDef(static, propName, fun.body)
    }

    val setter = property.setterArgAndBody.fold[js.Tree] {
      js.Skip()
    } { case (arg, body) =>
      val fun = desugarToFunction(className, arg :: Nil, body, isStat = true)
      js.SetterDef(static, propName, fun.args.head, fun.body)
    }

    js.Block(getter, setter)
  }

  /** Generate `classVar.prototype.name = value` */
  def genAddToPrototype(className: String, name: js.PropertyName, value: js.Tree)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): js.Tree = {
    genAddToObject(encodeClassVar(className).prototype, name, value)
  }

  /** Generate `classVar.prototype.name = value` */
  def genAddToPrototype(className: String, name: PropertyName, value: js.Tree)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): js.Tree = {
    genAddToPrototype(className, genPropertyName(name), value)
  }

  /** Generate `obj.name = value` */
  def genAddToObject(obj: js.Tree, name: js.PropertyName,
      value: js.Tree)(implicit pos: Position): js.Tree = {
    js.Assign(genPropSelect(obj, name), value)
  }

  /** Generate `obj.name = value` */
  def genAddToObject(className: String, obj: js.Tree, name: PropertyName,
      value: js.Tree)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): js.Tree = {
    genAddToObject(obj, genPropertyName(name), value)
  }

  def genPropertyName(name: PropertyName)(
      implicit globalKnowledge: GlobalKnowledge): js.PropertyName = {
    name match {
      case ident: Ident         => transformIdent(ident)
      case StringLiteral(value) => js.StringLiteral(value)(name.pos)

      case ComputedName(tree, _) =>
        implicit val pos = name.pos
        val fun = desugarToFunction(params = Nil, body = tree, isStat = false)
        val nameTree = fun match {
          case js.Function(Nil, js.Return(expr)) =>
            // no need for an IIFE, we can just use `expr` directly
            expr
          case _ =>
            js.Apply(fun, Nil)
        }
        js.ComputedName(nameTree)
    }
  }

  private[tools] def needInstanceTests(tree: LinkedClass): Boolean = {
    tree.hasInstanceTests || {
      tree.hasRuntimeTypeInfo &&
      ClassesWhoseDataReferToTheirInstanceTests.contains(tree.encodedName)
    }
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
      val isAncestorOfBoxedUnitClass =
        AncestorsOfBoxedUnitClass.contains(className)

      val objParam = js.ParamDef(Ident("obj"), rest = false)
      val obj = objParam.ref

      val createIsStat = {
        envFieldDef("is", className,
          js.Function(List(objParam), js.Return(className match {
            case Definitions.ObjectClass =>
              js.BinaryOp(JSBinaryOp.!==, obj, js.Null())

            case Definitions.StringClass =>
              js.UnaryOp(JSUnaryOp.typeof, obj) === js.StringLiteral("string")

            case Definitions.RuntimeNothingClass =>
              // Even null is not an instance of Nothing
              js.BooleanLiteral(false)

            case _ =>
              var test = {
                genIsScalaJSObject(obj) &&
                genIsClassNameInAncestors(className,
                    obj DOT "$classData" DOT "ancestors")
              }

              if (isAncestorOfString)
                test = test || (
                    js.UnaryOp(JSUnaryOp.typeof, obj) === js.StringLiteral("string"))
              if (isAncestorOfHijackedNumberClass)
                test = test || (
                    js.UnaryOp(JSUnaryOp.typeof, obj) === js.StringLiteral("number"))
              if (isAncestorOfBoxedBooleanClass)
                test = test || (
                    js.UnaryOp(JSUnaryOp.typeof, obj) === js.StringLiteral("boolean"))
              if (isAncestorOfBoxedUnitClass)
                test = test || (obj === js.Undefined())

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
              val throwError = {
                genCallHelper("throwClassCastException",
                    obj, js.StringLiteral(displayName))
              }
              if (className == RuntimeNothingClass) {
                // Always throw for .asInstanceOf[Nothing], even for null
                throwError
              } else {
                js.If(js.Apply(envField("is", className), List(obj)) ||
                    (obj === js.Null()), {
                  obj
                }, {
                  throwError
                })
              }
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

    val objParam = js.ParamDef(Ident("obj"), rest = false)
    val obj = objParam.ref

    val depthParam = js.ParamDef(Ident("depth"), rest = false)
    val depth = depthParam.ref

    val createIsArrayOfStat = {
      envFieldDef("isArrayOf", className,
        js.Function(List(objParam, depthParam), className match {
          case Definitions.ObjectClass =>
            val dataVarDef = genLet(Ident("data"), mutable = false, {
              obj && (obj DOT "$classData")
            })
            val data = dataVarDef.ref
            js.Block(
              dataVarDef,
              js.If(!data, {
                js.Return(js.BooleanLiteral(false))
              }, {
                val arrayDepthVarDef = genLet(Ident("arrayDepth"), mutable = false, {
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
                      !genIdentBracketSelect(data DOT "arrayBase", "isPrimitive")
                    )
                  })
              }))

          case _ =>
            js.Return(!(!({
              genIsScalaJSObject(obj) &&
              ((obj DOT "$classData" DOT "arrayDepth") === depth) &&
              genIsClassNameInAncestors(className,
                  obj DOT "$classData" DOT "arrayBase" DOT "ancestors")
            })))
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

  private def genIsScalaJSObject(obj: js.Tree)(implicit pos: Position): js.Tree = {
    import TreeDSL._
    obj && (obj DOT "$classData")
  }

  private def genIsClassNameInAncestors(className: String, ancestors: js.Tree)(
      implicit pos: Position): js.Tree = {
    import TreeDSL._
    ancestors DOT className
  }

  def genTypeData(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {
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
    val isJSType =
      kind.isJSType

    val isRawJSTypeParam =
      if (isJSType) js.BooleanLiteral(true)
      else js.Undefined()

    val parentData = if (globalKnowledge.isParentDataAccessed) {
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

    val (isInstanceFun, isArrayOfFun) = {
      if (isObjectClass) {
        /* Object has special ScalaJS.is.O *and* ScalaJS.isArrayOf.O. */
        (envField("is", className), envField("isArrayOf", className))
      } else if (isHijackedBoxedClass) {
        /* Hijacked boxed classes have a special isInstanceOf test. */
        val xParam = js.ParamDef(Ident("x"), rest = false)
        (js.Function(List(xParam), js.Return {
          genIsInstanceOf(xParam.ref, ClassType(className))
        }), js.Undefined())
      } else if (isAncestorOfHijackedClass || className == StringClass) {
        /* java.lang.String and ancestors of hijacked classes have a normal
         * ScalaJS.is.pack_Class test but with a non-standard behavior. */
        (envField("is", className), js.Undefined())
      } else if (isJSType) {
        /* Native JS classes have an instanceof operator-based isInstanceOf
         * test dictated by their jsNativeLoadSpec.
         * Non-native JS classes have something similar, based on their
         * constructor.
         * Other JS types do not have any instanceof operator, so the test
         * cannot be performed and must throw.
         */
        if (kind != ClassKind.JSClass && kind != ClassKind.NativeJSClass) {
          (envField("noIsInstance"), js.Undefined())
        } else {
          val jsCtor = genRawJSClassConstructor(className, tree.jsNativeLoadSpec)
          (js.Function(List(js.ParamDef(Ident("x"), rest = false)), js.Return {
            js.BinaryOp(JSBinaryOp.instanceof, js.VarRef(Ident("x")), jsCtor)
          }), js.Undefined())
        }
      } else {
        // For other classes, the isInstance function can be inferred.
        (js.Undefined(), js.Undefined())
      }
    }

    val allParams = List(
        js.ObjectConstr(List(classIdent -> js.IntLiteral(0))),
        js.BooleanLiteral(kind == ClassKind.Interface),
        js.StringLiteral(semantics.runtimeClassName(tree)),
        ancestorsRecord,
        isRawJSTypeParam,
        parentData,
        isInstanceFun,
        isArrayOfFun
    )

    val prunedParams =
      allParams.reverse.dropWhile(_.isInstanceOf[js.Undefined]).reverse

    val typeData = js.Apply(js.New(envField("TypeData"), Nil) DOT "initClass",
        prunedParams)

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

    require(tree.kind.hasModuleAccessor,
        s"genModuleAccessor called with non-module class: $className")

    val createModuleInstanceField =
      envFieldDef("n", className, js.Undefined(), mutable = true)

    val createAccessor = {
      val moduleInstanceVar = envField("n", className)

      val assignModule = {
        moduleInstanceVar := {
          if (tree.kind == ClassKind.JSModuleClass) {
            js.New(
                genRawJSClassConstructor(className, None),
                Nil)
          } else {
            js.Apply(
                js.New(encodeClassVar(className), Nil) DOT js.Ident("init___"),
                Nil)
          }
        }
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

      val body = js.Block(initBlock, js.Return(moduleInstanceVar))

      envFieldDef("m", className, js.Function(Nil, body))
    }

    js.Block(createModuleInstanceField, createAccessor)
  }

  def genExportedMembers(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {
    val exports = tree.exportedMembers map { member =>
      member.tree match {
        case MethodDef(false, StringLiteral("constructor"), _, _, _)
            if tree.kind.isJSClass =>
          js.Skip()(member.tree.pos)
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

  def genClassExports(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {
    val exports = tree.classExports map {
      case e: ConstructorExportDef =>
        genConstructorExportDef(tree, e)
      case e: JSClassExportDef =>
        genJSClassExportDef(tree, e)
      case e: ModuleExportDef =>
        genModuleExportDef(tree, e)
      case e: TopLevelModuleExportDef =>
        genTopLevelModuleExportDef(tree, e)
      case e: TopLevelMethodExportDef =>
        genTopLevelMethodExportDef(tree, e)
      case e: TopLevelFieldExportDef =>
        genTopLevelFieldExportDef(tree, e)
      case tree =>
        throw new AssertionError(
            "Illegal class export " + tree.getClass.getName)
    }

    js.Block(exports)(tree.pos)
  }

  def genConstructorExportDef(cd: LinkedClass, tree: ConstructorExportDef)(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {
    import TreeDSL._

    implicit val pos = tree.pos
    val classType = ClassType(cd.name.name)
    val ConstructorExportDef(fullName, args, body) = tree

    val baseCtor = envField("c", cd.name.name, cd.name.originalName)

    val thisIdent = js.Ident("$thiz")

    val js.Function(ctorParams, ctorBody) =
      desugarToFunction(cd.encodedName,
          Some(thisIdent), args, body, isStat = true)

    val exportedCtor = js.Function(ctorParams, js.Block(
      genLet(thisIdent, mutable = false, js.New(baseCtor, Nil)),
      ctorBody,
      js.Return(js.VarRef(thisIdent))
    ))

    val (createNamespace, expCtorVar) =
      genCreateNamespaceInExports(fullName)
    js.Block(
      createNamespace,
      js.DocComment("@constructor"),
      expCtorVar := exportedCtor,
      expCtorVar DOT "prototype" := baseCtor DOT "prototype"
    )
  }

  def genJSClassExportDef(cd: LinkedClass, tree: JSClassExportDef): js.Tree = {
    import TreeDSL._

    implicit val pos = tree.pos

    val classVar = genRawJSClassConstructor(cd.name.name, None)
    genClassOrModuleExportDef(cd, tree.fullName, classVar)
  }

  /** Generates an exporter for a module as a 0-arg function.
   *
   *  This corresponds to the old-style `@JSExport` of modules. Basically this
   *  exports the module accessor. The object will be initialized lazily on
   *  the first call of the accessor, exactly like the accesses to objects from
   *  Scala code.
   */
  def genModuleExportDef(cd: LinkedClass, tree: ModuleExportDef): js.Tree = {
    import TreeDSL._

    implicit val pos = tree.pos

    val baseAccessor = envField("m", cd.name.name)
    genClassOrModuleExportDef(cd, tree.fullName, baseAccessor)
  }

  /** Generates an exporter for a module at the "top-level", which is directly
   *  as a variable holding the module instance.
   *
   *  This corresponds the the new-style `@JSExportTopLevel` of modules. In
   *  this case, the module instance is initialized during ES moduleµ
   *  instantiation.
   */
  def genTopLevelModuleExportDef(cd: LinkedClass,
      tree: TopLevelModuleExportDef): js.Tree = {
    import TreeDSL._

    implicit val pos = tree.pos

    val moduleVar = genLoadModule(cd.name.name)
    genClassOrModuleExportDef(cd, tree.fullName, moduleVar)
  }

  private def genClassOrModuleExportDef(cd: LinkedClass, exportFullName: String,
      exportedValue: js.Tree)(implicit pos: Position): js.Tree = {
    import TreeDSL._

    val (createNamespace, expAccessorVar) =
      genCreateNamespaceInExports(exportFullName)
    js.Block(
      createNamespace,
      expAccessorVar := exportedValue
    )
  }

  private def genTopLevelMethodExportDef(cd: LinkedClass,
      tree: TopLevelMethodExportDef)(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {
    import TreeDSL._

    val MethodDef(true, StringLiteral(fullName), args, resultType, Some(body)) =
      tree.methodDef

    implicit val pos = tree.pos

    val (createNamespace, expAccessorVar) =
      genCreateNamespaceInExports(fullName)

    val methodDef = desugarToFunction(cd.encodedName, args, body,
        isStat = resultType == NoType)

    js.Block(
        createNamespace,
        expAccessorVar := methodDef
    )
  }

  private def genTopLevelFieldExportDef(cd: LinkedClass,
      tree: TopLevelFieldExportDef)(
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {
    import TreeDSL._

    val TopLevelFieldExportDef(fullName, field) = tree

    implicit val pos = tree.pos

    val (createNamespace, namespace, fieldName) =
      genCreateNamespaceInExportsAndGetNamespace(fullName)

    // defineProperty method
    val defProp =
      genIdentBracketSelect(js.VarRef(js.Ident("Object")), "defineProperty")

    // optional getter definition
    val getterDef = {
      js.StringLiteral("get") -> js.Function(Nil, {
        js.Return(genSelectStatic(cd.encodedName, field))
      })
    }

    // Options passed to the defineProperty method
    val descriptor = js.ObjectConstr(
        getterDef ::
        (js.StringLiteral("configurable") -> js.BooleanLiteral(true)) ::
        Nil
    )

    val callDefineProp =
      js.Apply(defProp, namespace :: fieldName :: descriptor :: Nil)

    js.Block(createNamespace, callDefineProp)
  }

  // Helpers

  /** Gen JS code for assigning an rhs to a qualified name in the exports scope.
   *  For example, given the qualified name `"foo.bar.Something"`, generates:
   *
   *  {{{
   *  $e["foo"] = $e["foo"] || {};
   *  $e["foo"]["bar"] = $e["foo"]["bar"] || {};
   *  }}}
   *
   *  Returns `(statements, $e["foo"]["bar"]["Something"])`
   */
  private def genCreateNamespaceInExports(qualName: String)(
      implicit pos: Position): (js.Tree, js.Tree) = {
    val (createNamespace, namespace, lastPart) =
      genCreateNamespaceInExportsAndGetNamespace(qualName)
    (createNamespace, genBracketSelect(namespace, lastPart))
  }

  /** Gen JS code for assigning an rhs to a qualified name in the exports scope.
   *  For example, given the qualified name `"foo.bar.Something"`, generates:
   *
   *  {{{
   *  $e["foo"] = $e["foo"] || {};
   *  $e["foo"]["bar"] = $e["foo"]["bar"] || {};
   *  }}}
   *
   *  Returns `(statements, $e["foo"]["bar"], "Something")`
   */
  private def genCreateNamespaceInExportsAndGetNamespace(qualName: String)(
      implicit pos: Position): (js.Tree, js.Tree, js.StringLiteral) = {
    val parts = qualName.split("\\.")
    val statements = List.newBuilder[js.Tree]
    var namespace = envField("e")
    for (i <- 0 until parts.length-1) {
      namespace = genBracketSelect(namespace, js.StringLiteral(parts(i)))
      statements +=
        js.Assign(namespace, js.BinaryOp(JSBinaryOp.||,
            namespace, js.ObjectConstr(Nil)))
    }
    (js.Block(statements.result()), namespace, js.StringLiteral(parts.last))
  }

}

private object ScalaJSClassEmitter {
  private val ClassesWhoseDataReferToTheirInstanceTests = {
    Definitions.AncestorsOfHijackedClasses +
    Definitions.ObjectClass + Definitions.StringClass
  }
}
