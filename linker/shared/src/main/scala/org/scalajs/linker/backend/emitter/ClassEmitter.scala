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

package org.scalajs.linker.backend.emitter

import org.scalajs.ir._
import Position._
import Transformers._
import org.scalajs.ir.Trees._
import Types._

import org.scalajs.linker._
import org.scalajs.linker.standard._
import org.scalajs.linker.backend.javascript.{Trees => js}

import CheckedBehavior.Unchecked

/** Emitter for the skeleton of classes. */
private[emitter] final class ClassEmitter(jsGen: JSGen) {

  private val functionEmitter = new FunctionEmitter(jsGen)

  import ClassEmitter._
  import functionEmitter._
  import jsGen._

  def genStaticMembers(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    val className = tree.name.name
    val staticMemberDefsWithGlobals =
      tree.staticMethods.map(m => genMethod(className, m.value))
    for (staticMemberDefs <- WithGlobals.list(staticMemberDefsWithGlobals))
      yield js.Block(staticMemberDefs)(tree.pos)
  }

  def genDefaultMethods(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    val className = tree.name.name
    val defaultMethodDefsWithGlobals =
      tree.memberMethods.map(m => genDefaultMethod(className, m.value))
    for (defaultMethodDefs <- WithGlobals.list(defaultMethodDefsWithGlobals))
      yield js.Block(defaultMethodDefs)(tree.pos)
  }

  def buildClass(tree: LinkedClass, ctor: WithGlobals[js.Tree],
      memberDefs: List[WithGlobals[js.Tree]], exportedDefs: WithGlobals[js.Tree])(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {

    WithGlobals.list(ctor +: memberDefs :+ exportedDefs).flatMap { allDefs =>
      val className = tree.name.name
      val allDefsBlock = js.Block(allDefs)(tree.pos)

      val entireClassDefWithGlobals = if (useClasses) {
        val allDefs = allDefsBlock match {
          case js.Block(allDefs) => allDefs
          case js.Skip()         => Nil
          case oneDef            => List(oneDef)
        }
        genES6Class(tree, allDefs)
      } else {
        WithGlobals(allDefsBlock)
      }

      if (!tree.kind.isJSClass) {
        assert(tree.jsSuperClass.isEmpty, className)
        entireClassDefWithGlobals
      } else {
        // Wrap the entire class def in an accessor function
        import TreeDSL._
        implicit val pos = tree.pos

        val genStoreJSSuperClass = tree.jsSuperClass.map { jsSuperClass =>
          for (rhs <- desugarExpr(jsSuperClass, resultType = AnyType)) yield {
            js.VarDef(envField("superClass").ident, Some(rhs))
          }
        }

        for {
          optStoreJSSuperClass <- WithGlobals.option(genStoreJSSuperClass)
          entireClassDef <- entireClassDefWithGlobals
          createStaticFields <- genCreateStaticFieldsOfJSClass(tree)
        } yield {
          tree.jsClassCaptures.fold {
            val createClassValueVar =
              envFieldDef("b", className, js.Undefined(), mutable = true)

            val createAccessor = {
              val classValueVar = envField("b", className)

              val body = js.Block(
                  js.If(!classValueVar, {
                    js.Block(
                        optStoreJSSuperClass.toList :::
                        entireClassDef ::
                        createStaticFields :::
                        (classValueVar := envField("c", className)) ::
                        genStaticInitialization(tree)
                    )
                  }, {
                    js.Skip()
                  }),
                  js.Return(classValueVar)
              )

              envFunctionDef("a", className, Nil, body)
            }

            js.Block(createClassValueVar, createAccessor)
          } { jsClassCaptures =>
            val captureParamDefs = for (param <- jsClassCaptures) yield {
              implicit val pos = param.pos
              val ident =
                envFieldIdent("cc", param.name.name, param.name.originalName)
              js.ParamDef(ident, rest = false)
            }

            assert(!hasStaticInitializer(tree),
                s"Found a static initializer in the non-top-level class $className")

            val body = js.Block(
                optStoreJSSuperClass.toList :::
                entireClassDef ::
                createStaticFields :::
                js.Return(envField("c", className)) ::
                Nil
            )

            envFunctionDef("a", className, captureParamDefs, body)
          }
        }
      }
    }
  }

  /** Generates an ECMAScript 6 class for a linked class. */
  def genES6Class(tree: LinkedClass, members: List[js.Tree])(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {

    require(useClasses)

    val className = tree.name.name
    val classIdent = encodeClassVar(className)(tree.name.pos).ident

    val parentVarWithGlobals = for (parentIdent <- tree.superClass) yield {
      implicit val pos = parentIdent.pos
      if (!tree.kind.isJSClass) {
        if (shouldExtendJSError(tree))
          WithGlobals(js.VarRef(js.Ident("Error")))
        else
          WithGlobals(encodeClassVar(parentIdent.name))
      } else if (tree.jsSuperClass.isDefined) {
        WithGlobals(envField("superClass"))
      } else {
        genJSClassConstructor(parentIdent.name,
            keepOnlyDangerousVarNames = true)
      }
    }

    for (parentVar <- WithGlobals.option(parentVarWithGlobals))
      yield js.ClassDef(Some(classIdent), parentVar, members)(tree.pos)
  }

  /** Extracts the inlineable init method, if there is one. */
  def extractInlineableInit(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): (Option[Versioned[MethodDef]], List[Versioned[MethodDef]]) = {

    val memberMethods = tree.memberMethods

    if (globalKnowledge.hasInlineableInit(tree.encodedName)) {
      val (constructors, otherMethods) = memberMethods.partition { method =>
        Definitions.isConstructorName(method.value.encodedName)
      }
      assert(constructors.size == 1,
          s"Found ${constructors.size} constructors in class " +
          s"${tree.encodedName} which has an inlined init.")
      (Some(constructors.head), otherMethods)
    } else {
      (None, memberMethods)
    }
  }

  /** Generates the JS constructor for a class. */
  def genConstructor(tree: LinkedClass, initToInline: Option[MethodDef])(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {

    assert(tree.kind.isAnyNonNativeClass)
    assert(tree.superClass.isDefined || tree.name.name == Definitions.ObjectClass,
        s"Class ${tree.name.name} is missing a parent class")

    if (useClasses)
      genES6Constructor(tree, initToInline)
    else
      genES5Constructor(tree, initToInline)
  }

  /** Generates the JS constructor for a class, ES5 style. */
  private def genES5Constructor(tree: LinkedClass,
      initToInline: Option[MethodDef])(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    import TreeDSL._
    implicit val pos = tree.pos

    val className = tree.name.name
    val isJSClass = tree.kind.isJSClass
    val typeVar = encodeClassVar(className)

    def makeInheritableCtorDef(ctorToMimic: js.Tree, field: String) = {
      js.Block(
        js.DocComment("@constructor"),
        envFieldDef(field, className, js.Function(false, Nil, js.Skip()),
            keepFunctionExpression = isJSClass),
        envField(field, className).prototype := ctorToMimic.prototype
      )
    }

    val ctorFunWithGlobals =
      if (!isJSClass) genJSConstructorFun(tree, initToInline)
      else genConstructorFunForJSClass(tree)

    val chainProtoWithGlobals = tree.superClass.fold[WithGlobals[js.Tree]] {
      WithGlobals(js.Skip())
    } { parentIdent =>
      val (inheritedCtorDefWithGlobals, inheritedCtorRef) = if (!isJSClass) {
        if (shouldExtendJSError(tree)) {
          val inheritableCtorDef =
            makeInheritableCtorDef(js.VarRef(js.Ident("Error")), "hh")
          (WithGlobals(inheritableCtorDef), envField("hh", className))
        } else {
          (WithGlobals(js.Skip()), envField("h", parentIdent.name))
        }
      } else {
        val superCtor = if (tree.jsSuperClass.isDefined) {
          WithGlobals(envField("superClass"))
        } else {
          genJSClassConstructor(parentIdent.name,
              keepOnlyDangerousVarNames = true)
        }
        (superCtor.map(makeInheritableCtorDef(_, "h")), envField("h", className))
      }

      for (inheritedCtorDef <- inheritedCtorDefWithGlobals) yield {
        js.Block(
            inheritedCtorDef,
            typeVar.prototype := js.New(inheritedCtorRef, Nil),
            genAddToPrototype(className, js.StringLiteral("constructor"), typeVar)
        )
      }
    }

    for {
      ctorFun <- ctorFunWithGlobals
      chainProto <- chainProtoWithGlobals
    } yield {
      val docComment = js.DocComment("@constructor")
      val ctorDef = envFieldDef("c", className, ctorFun,
          keepFunctionExpression = isJSClass)

      val inheritableCtorDef =
        if (isJSClass) js.Skip()
        else makeInheritableCtorDef(typeVar, "h")

      js.Block(docComment, ctorDef, chainProto, inheritableCtorDef)
    }
  }

  /** Generates the JS constructor for a class, ES6 style. */
  private def genES6Constructor(tree: LinkedClass,
      initToInline: Option[MethodDef])(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    implicit val pos = tree.pos

    if (tree.kind.isJSClass) {
      for (fun <- genConstructorFunForJSClass(tree)) yield {
        js.MethodDef(static = false, js.Ident("constructor"), fun.args,
            fun.body)
      }
    } else {
      val jsConstructorFunWithGlobals =
        genJSConstructorFun(tree, initToInline)

      for (jsConstructorFun <- jsConstructorFunWithGlobals) yield {
        val js.Function(_, args, body) = jsConstructorFun

        def isTrivialCtorBody: Boolean = body match {
          case js.Skip()                 => true
          case js.Apply(js.Super(), Nil) => true
          case _                         => false
        }

        if (args.isEmpty && isTrivialCtorBody)
          js.Skip()
        else
          js.MethodDef(static = false, js.Ident("constructor"), args, body)
      }
    }
  }

  /** Generates the JavaScript constructor of a class, as a `js.Function`.
   *
   *  For ECMAScript 2015, that `js.Function` must be decomposed and reformed
   *  into a `js.MethodDef` afterwards.
   *
   *  The generated function performs the following steps:
   *
   *  - Create the fields of the current class, initialized to the zero of
   *    their respective types
   *    - In ECMAScript 5.1, all the fields in the parent chain are directly
   *      created, avoiding expensive super calls
   *    - In ECMAScript 2015, calling the super constructor is mandatory, so we
   *      first call it then create the fields directly declared in this class
   *  - Executes the body of the `init` method to inline, if any
   *
   *  Note that we know that a super JS constructor never contains an inlined
   *  init (which the above ES 5.1 treatment would throw away), because classes
   *  that have instantiated subclasses are not eligible for the inlined init
   *  optimization.
   *
   *  @param tree
   *    The `LinkedClass` for which we generate a JS constructor.
   *
   *  @param initToInline
   *    The `init` method to inline in the JS constructor, if any.
   */
  private def genJSConstructorFun(tree: LinkedClass,
      initToInline: Option[MethodDef])(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Function] = {

    implicit val pos = tree.pos

    val superCtorCallAndFieldDefs = if (useClasses) {
      val fieldDefs = genFieldDefsOfScalaClass(tree.fields)
      if (tree.superClass.isEmpty)
        fieldDefs
      else
        js.Apply(js.Super(), Nil) :: fieldDefs
    } else {
      val allFields =
        globalKnowledge.getAllScalaClassFieldDefs(tree.encodedName)
      genFieldDefsOfScalaClass(allFields)
    }

    initToInline.fold {
      WithGlobals(
          js.Function(arrow = false, Nil, js.Block(superCtorCallAndFieldDefs)))
    } { initMethodDef =>
      val generatedInitMethodFunWithGlobals = {
        implicit val pos = initMethodDef.pos
        val initMethodBody = initMethodDef.body.getOrElse {
          throw new AssertionError("Cannot generate an abstract constructor")
        }
        assert(initMethodDef.resultType == NoType,
            s"Found a constructor with type ${initMethodDef.resultType} at $pos")
        desugarToFunction(tree.encodedName, initMethodDef.args, initMethodBody,
            resultType = NoType)
      }

      for (generatedInitMethodFun <- generatedInitMethodFunWithGlobals) yield {
        val js.Function(arrow, args, initMethodFunBody) = generatedInitMethodFun
        js.Function(arrow, args,
            js.Block(superCtorCallAndFieldDefs ::: initMethodFunBody :: Nil))
      }
    }
  }

  private def genConstructorFunForJSClass(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Function] = {
    implicit val pos = tree.pos

    require(tree.kind.isJSClass)

    tree.exportedMembers.map(_.value) collectFirst {
      case MethodDef(false, StringLiteral("constructor"), params, _, body) =>
        desugarToFunction(tree.encodedName, params, body.get, resultType = NoType)
    } getOrElse {
      throw new IllegalArgumentException(
          s"${tree.encodedName} does not have an exported constructor")
    }
  }

  /** Generates the creation of fields for a Scala class. */
  private def genFieldDefsOfScalaClass(fields: List[FieldDef])(
      implicit globalKnowledge: GlobalKnowledge): List[js.Tree] = {
    for {
      field @ FieldDef(false, name, ftpe, mutable) <- fields
    } yield {
      implicit val pos = field.pos
      val jsIdent = (name: @unchecked) match {
        case Ident(name, origName) => js.Ident(name, origName)
      }
      js.Assign(js.DotSelect(js.This(), jsIdent), genZeroOf(ftpe))
    }
  }

  /** Generates the creation of the static fields for a Scala class. */
  def genCreateStaticFieldsOfScalaClass(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): List[js.Tree] = {
    for {
      field @ FieldDef(true, Ident(name, origName), ftpe, mutable) <- tree.fields
    } yield {
      implicit val pos = field.pos
      val fullName = tree.encodedName + "__" + name
      envFieldDef("t", fullName, genZeroOf(ftpe), origName, mutable)
    }
  }

  /** Generates the creation of the static fields for a JavaScript class. */
  private def genCreateStaticFieldsOfJSClass(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[List[js.Tree]] = {
    val className = tree.encodedName
    val statsWithGlobals = for {
      field @ FieldDef(true, name, ftpe, mutable) <- tree.fields
    } yield {
      implicit val pos = field.pos
      val classVar = envField("c", className)
      for (propName <- genPropertyName(name)) yield {
        val select = genPropSelect(classVar, propName)
        val zero =
          if (ftpe == CharType) js.VarRef(js.Ident("$bC0"))
          else genZeroOf(ftpe)
        js.Assign(select, zero)
      }
    }
    WithGlobals.list(statsWithGlobals)
  }

  /** Generates the static initializer invocation of a class. */
  def genStaticInitialization(tree: LinkedClass): List[js.Tree] = {
    import Definitions.StaticInitializerName
    implicit val pos = tree.pos
    if (hasStaticInitializer(tree)) {
      val fullName = tree.encodedName + "__" + StaticInitializerName
      js.Apply(envField("s", fullName, Some("<clinit>")), Nil) :: Nil
    } else {
      Nil
    }
  }

  private def hasStaticInitializer(tree: LinkedClass): Boolean = {
    import Definitions.StaticInitializerName
    tree.staticMethods.exists(_.value.encodedName == StaticInitializerName)
  }

  /** Generates a method. */
  def genMethod(className: String, method: MethodDef)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    val methodBody = method.body.getOrElse(
        throw new AssertionError("Cannot generate an abstract method"))

    implicit val pos = method.pos

    val methodFun0WithGlobals = desugarToFunction(className,
        method.args, methodBody, method.resultType)

    methodFun0WithGlobals.flatMap { methodFun0 =>
      val methodFun = if (Definitions.isConstructorName(method.encodedName)) {
        // init methods have to return `this` so that we can chain them to `new`
        js.Function(arrow = false, methodFun0.args, {
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
            WithGlobals(envFieldDef("s", className + "__" + methodName,
                methodFun, origName))

          case methodName =>
            if (useClasses) {
              for (propName <- genPropertyName(methodName)) yield {
                js.MethodDef(static = true, propName, methodFun.args,
                    methodFun.body)
              }
            } else {
              genAddToObject(className, encodeClassVar(className), methodName,
                  methodFun)
            }
        }
      } else {
        if (useClasses) {
          for (propName <- genPropertyName(method.name)) yield {
            js.MethodDef(static = false, propName, methodFun.args,
                methodFun.body)
          }
        } else {
          genAddToPrototype(className, method.name, methodFun)
        }
      }
    }
  }

  /** Generates a default method. */
  def genDefaultMethod(className: String, method: MethodDef)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    implicit val pos = method.pos

    val methodFunWithGlobals = desugarToFunctionWithExplicitThis(
        className, method.args, method.body.get, method.resultType)

    for (methodFun <- methodFunWithGlobals) yield {
      val Ident(methodName, origName) = method.name
      envFieldDef("f", className + "__" + methodName, methodFun, origName)
    }
  }

  /** Generates an instance method of a hijacked class. */
  def genHijackedMethod(className: String, method: MethodDef)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    // We abuse `genDefaultMethod` as it does everything the way we want
    genDefaultMethod(className, method)
  }

  /** Generates a property. */
  def genProperty(className: String, property: PropertyDef)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    if (useClasses)
      genPropertyES6(className, property)
    else
      genPropertyES5(className, property)
  }

  private def genPropertyES5(className: String, property: PropertyDef)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    import TreeDSL._
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
    val propNameWithGlobals = genPropertyName(property.name)

    // optional getter definition
    val optGetterWithGlobals = property.getterBody map { body =>
      desugarToFunction(className, Nil, body, resultType = AnyType)
    }

    // optional setter definition
    val optSetterWithGlobals = property.setterArgAndBody map {
      case (arg, body) =>
        desugarToFunction(className, arg :: Nil, body, resultType = NoType)
    }

    for {
      propName <- propNameWithGlobals
      optGetter <- WithGlobals.option(optGetterWithGlobals)
      optSetter <- WithGlobals.option(optSetterWithGlobals)
    } yield {
      val name = propName match {
        case value: js.StringLiteral => value
        case js.ComputedName(tree)   => tree

        case id: js.Ident =>
          // We need to work around the closure compiler. Call propertyName to
          // get a string representation of the optimized name
          genCallHelper("propertyName",
              js.ObjectConstr(id -> js.IntLiteral(0) :: Nil))
      }

      // Options passed to the defineProperty method
      val descriptor = js.ObjectConstr(
        optGetter.map(js.StringLiteral("get") -> _).toList :::
        optSetter.map(js.StringLiteral("set") -> _).toList :::
        (js.StringLiteral("configurable") -> js.BooleanLiteral(true)) ::
        Nil
      )

      js.Apply(defProp, targetObject :: name :: descriptor :: Nil)
    }
  }

  private def genPropertyES6(className: String, property: PropertyDef)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    implicit val pos = property.pos

    val static = property.static

    genPropertyName(property.name).flatMap { propName =>
      val getterWithGlobals = property.getterBody.fold {
        WithGlobals[js.Tree](js.Skip())
      } { body =>
        for (fun <- desugarToFunction(className, Nil, body, resultType = AnyType))
          yield js.GetterDef(static, propName, fun.body)
      }

      val setterWithGlobals = property.setterArgAndBody.fold {
        WithGlobals[js.Tree](js.Skip())
      } { case (arg, body) =>
        for (fun <- desugarToFunction(className, arg :: Nil, body, resultType = NoType))
          yield js.SetterDef(static, propName, fun.args.head, fun.body)
      }

      for {
        getter <- getterWithGlobals
        setter <- setterWithGlobals
      } yield {
        js.Block(getter, setter)
      }
    }
  }

  /** Generate `classVar.prototype.name = value` */
  def genAddToPrototype(className: String, name: js.PropertyName, value: js.Tree)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): js.Tree = {
    import TreeDSL._

    genAddToObject(encodeClassVar(className).prototype, name, value)
  }

  /** Generate `classVar.prototype.name = value` */
  def genAddToPrototype(className: String, name: PropertyName, value: js.Tree)(
      implicit globalKnowledge: GlobalKnowledge,
      pos: Position): WithGlobals[js.Tree] = {
    for (propName <- genPropertyName(name))
      yield genAddToPrototype(className, propName, value)
  }

  /** Generate `obj.name = value` */
  def genAddToObject(obj: js.Tree, name: js.PropertyName,
      value: js.Tree)(implicit pos: Position): js.Tree = {
    js.Assign(genPropSelect(obj, name), value)
  }

  /** Generate `obj.name = value` */
  def genAddToObject(className: String, obj: js.Tree, name: PropertyName,
      value: js.Tree)(
      implicit globalKnowledge: GlobalKnowledge,
      pos: Position): WithGlobals[js.Tree] = {
    for (propName <- genPropertyName(name))
      yield genAddToObject(obj, propName, value)
  }

  def genPropertyName(name: PropertyName)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.PropertyName] = {
    name match {
      case Ident(nameStr, origName) =>
        WithGlobals(js.Ident(nameStr, origName)(name.pos))

      case StringLiteral(value) =>
        WithGlobals(js.StringLiteral(value)(name.pos))

      case ComputedName(tree, _) =>
        implicit val pos = name.pos
        desugarExpr(tree, resultType = AnyType).map(js.ComputedName(_))
    }
  }

  def needInstanceTests(tree: LinkedClass): Boolean = {
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
        tree.name.name == Definitions.BoxedStringClass) {
      val className = tree.name.name
      val displayName = decodeClassName(className)

      val isAncestorOfString =
        NonObjectAncestorsOfStringClass.contains(className)
      val isAncestorOfHijackedNumberClass =
        NonObjectAncestorsOfHijackedNumberClasses.contains(className)
      val isAncestorOfBoxedBooleanClass =
        NonObjectAncestorsOfBoxedBooleanClass.contains(className)
      val isAncestorOfBoxedCharacterClass =
        NonObjectAncestorsOfBoxedCharacterClass.contains(className)

      val objParam = js.ParamDef(js.Ident("obj"), rest = false)
      val obj = objParam.ref

      val createIsStat = {
        envFunctionDef("is", className, List(objParam), js.Return {
          className match {
            case Definitions.ObjectClass =>
              js.BinaryOp(JSBinaryOp.!==, obj, js.Null())

            case Definitions.BoxedStringClass =>
              js.UnaryOp(JSUnaryOp.typeof, obj) === js.StringLiteral("string")

            case _ =>
              var test = {
                genIsScalaJSObject(obj) &&
                genIsClassNameInAncestors(className,
                    obj DOT "$classData" DOT "ancestors")
              }

              def typeOfTest(typeString: String): js.Tree =
                js.UnaryOp(JSUnaryOp.typeof, obj) === js.StringLiteral(typeString)

              if (isAncestorOfString)
                test = test || typeOfTest("string")
              if (isAncestorOfHijackedNumberClass) {
                test = test || typeOfTest("number")
                if (useBigIntForLongs)
                  test = test || genCallHelper("isLong", obj)
              }
              if (isAncestorOfBoxedBooleanClass)
                test = test || typeOfTest("boolean")
              if (isAncestorOfBoxedCharacterClass)
                test = test || (obj instanceof envField("Char"))

              !(!test)
          }
        })
      }

      val createAsStat = if (semantics.asInstanceOfs == Unchecked) {
        js.Skip()
      } else {
        envFunctionDef("as", className, List(objParam), js.Return {
          className match {
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
          }
        })
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

    val objParam = js.ParamDef(js.Ident("obj"), rest = false)
    val obj = objParam.ref

    val depthParam = js.ParamDef(js.Ident("depth"), rest = false)
    val depth = depthParam.ref

    val createIsArrayOfStat = {
      envFunctionDef("isArrayOf", className, List(objParam, depthParam), {
        className match {
          case Definitions.ObjectClass =>
            val dataVarDef = genLet(js.Ident("data"), mutable = false, {
              obj && (obj DOT "$classData")
            })
            val data = dataVarDef.ref
            js.Block(
              dataVarDef,
              js.If(!data, {
                js.Return(js.BooleanLiteral(false))
              }, {
                val arrayDepthVarDef = genLet(js.Ident("arrayDepth"), mutable = false, {
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
        }
      })
    }

    val createAsArrayOfStat = if (semantics.asInstanceOfs == Unchecked) {
      js.Skip()
    } else {
      envFunctionDef("asArrayOf", className, List(objParam, depthParam), {
        js.Return {
          js.If(js.Apply(envField("isArrayOf", className), List(obj, depth)) ||
              (obj === js.Null()), {
            obj
          }, {
            genCallHelper("throwArrayCastException",
                obj, js.StringLiteral("L"+displayName+";"), depth)
          })
        }
      })
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
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    import Definitions._
    import TreeDSL._

    implicit val pos = tree.pos

    val className = tree.name.name
    val kind = tree.kind

    val isObjectClass =
      className == ObjectClass
    val isHijackedClass =
      HijackedClasses.contains(className)
    val isAncestorOfHijackedClass =
      isObjectClass || AncestorsOfHijackedClasses.contains(className)
    val isJSType =
      kind.isJSType

    val isJSTypeParam =
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

    val isInstanceFunWithGlobals: WithGlobals[js.Tree] = {
      if (isAncestorOfHijackedClass || className == BoxedStringClass) {
        /* java.lang.String and ancestors of hijacked classes, including
         * java.lang.Object, have a normal $is_pack_Class test but with a
         * non-standard behavior.
         */
        WithGlobals(envField("is", className))
      } else if (isHijackedClass) {
        /* Other hijacked classes have a special isInstanceOf test. */
        val xParam = js.ParamDef(js.Ident("x"), rest = false)
        WithGlobals(genArrowFunction(List(xParam), js.Return {
          genIsInstanceOf(xParam.ref, ClassRef(className))
        }))
      } else if (isJSType) {
        /* Native JS classes have an instanceof operator-based isInstanceOf
         * test dictated by their jsNativeLoadSpec.
         * Non-native JS classes have something similar, based on their
         * constructor.
         * Other JS types do not have any instanceof operator, so the test
         * cannot be performed and must throw.
         */
        if (kind != ClassKind.JSClass && kind != ClassKind.NativeJSClass) {
          WithGlobals(envField("noIsInstance"))
        } else {
          for {
            jsCtor <- genJSClassConstructor(className, tree.jsNativeLoadSpec,
                keepOnlyDangerousVarNames = true)
          } yield {
            genArrowFunction(List(js.ParamDef(js.Ident("x"), rest = false)), js.Return {
              js.VarRef(js.Ident("x")) instanceof jsCtor
            })
          }
        }
      } else {
        // For other classes, the isInstance function can be inferred.
        WithGlobals(js.Undefined())
      }
    }

    val isArrayOfFun = {
      if (isObjectClass) {
        // Object is the only class that has a special $isArrayOf_O.
        envField("isArrayOf", className)
      } else {
        // For other classes, the isArrayOf function can be inferred.
        js.Undefined()
      }
    }

    for (isInstanceFun <- isInstanceFunWithGlobals) yield {
      val allParams = List(
          js.ObjectConstr(List(js.Ident(className) -> js.IntLiteral(0))),
          js.BooleanLiteral(kind == ClassKind.Interface),
          js.StringLiteral(semantics.runtimeClassNameMapper(tree.fullName)),
          ancestorsRecord,
          isJSTypeParam,
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

    val className = tree.name.name
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
                genNonNativeJSClassConstructor(className),
                Nil)
          } else {
            js.New(encodeClassVar(className), Nil)
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

      envFunctionDef("m", className, Nil, body)
    }

    js.Block(createModuleInstanceField, createAccessor)
  }

  def genExportedMembers(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    val exportsWithGlobals = tree.exportedMembers map { member =>
      member.value match {
        case MethodDef(false, StringLiteral("constructor"), _, _, _)
            if tree.kind.isJSClass =>
          WithGlobals(js.Skip()(member.value.pos))
        case m: MethodDef =>
          genMethod(tree.encodedName, m)
        case p: PropertyDef =>
          genProperty(tree.encodedName, p)
        case tree =>
          throw new AssertionError(
              "Illegal exportedMember " + tree.getClass.getName)
      }
    }

    for (exports <- WithGlobals.list(exportsWithGlobals))
      yield js.Block(exports)(tree.pos)
  }

  def genTopLevelExports(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[List[js.Tree]] = {
    val exportsWithGlobals = tree.topLevelExports.map { versionedTopLevelExport =>
      val topLevelExport = versionedTopLevelExport.value
      implicit val pos = topLevelExport.pos

      topLevelExport match {
        case TopLevelJSClassExportDef(exportName) =>
          genConstValueExportDef(
              exportName, genNonNativeJSClassConstructor(tree.name.name))
        case TopLevelModuleExportDef(exportName) =>
          genConstValueExportDef(exportName, genLoadModule(tree.name.name))
        case e: TopLevelMethodExportDef =>
          genTopLevelMethodExportDef(tree, e)
        case e: TopLevelFieldExportDef =>
          genTopLevelFieldExportDef(tree, e)
      }
    }

    WithGlobals.list(exportsWithGlobals)
  }

  private def genTopLevelMethodExportDef(cd: LinkedClass,
      tree: TopLevelMethodExportDef)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    import TreeDSL._

    val MethodDef(true, StringLiteral(exportName), args, resultType, Some(body)) =
      tree.methodDef

    implicit val pos = tree.pos

    val methodDefWithGlobals = desugarToFunction(cd.encodedName, args, body,
        resultType)

    methodDefWithGlobals.flatMap { methodDef =>
      genConstValueExportDef(exportName, methodDef)
    }
  }

  private def genConstValueExportDef(exportName: String,
      exportedValue: js.Tree)(
      implicit pos: Position): WithGlobals[js.Tree] = {
    moduleKind match {
      case ModuleKind.NoModule =>
        genAssignToNoModuleExportVar(exportName, exportedValue)

      case ModuleKind.ESModule =>
        val field = envField("e", exportName)
        val let = js.Let(field.ident, mutable = true, Some(exportedValue))
        val export = js.Export((field.ident -> js.ExportName(exportName)) :: Nil)
        WithGlobals(js.Block(let, export))

      case ModuleKind.CommonJSModule =>
        val exportsVarRef = js.VarRef(js.Ident("exports"))
        WithGlobals(js.Assign(
            genBracketSelect(exportsVarRef, js.StringLiteral(exportName)),
            exportedValue))
    }
  }

  private def genAssignToNoModuleExportVar(exportName: String, rhs: js.Tree)(
      implicit pos: Position): WithGlobals[js.Tree] = {
    val dangerousGlobalRefs: Set[String] =
      if (GlobalRefUtils.isDangerousGlobalRef(exportName)) Set(exportName)
      else Set.empty
    WithGlobals(
        js.Assign(js.VarRef(js.Ident(exportName)), rhs),
        dangerousGlobalRefs)
  }

  private def genTopLevelFieldExportDef(cd: LinkedClass,
      tree: TopLevelFieldExportDef)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    import TreeDSL._

    val TopLevelFieldExportDef(exportName, field) = tree

    implicit val pos = tree.pos

    moduleKind match {
      case ModuleKind.NoModule =>
        /* Initial value of the export. Updates are taken care of explicitly
         * when we assign to the static field.
         */
        genAssignToNoModuleExportVar(exportName,
            genSelectStatic(cd.encodedName, field))

      case ModuleKind.ESModule =>
        val staticVarIdent = genSelectStatic(cd.encodedName, field).ident
        WithGlobals(
            js.Export((staticVarIdent -> js.ExportName(exportName)) :: Nil))

      case ModuleKind.CommonJSModule =>
        // defineProperty method
        val defProp =
          genIdentBracketSelect(js.VarRef(js.Ident("Object")), "defineProperty")

        val exportsVarRef = js.VarRef(js.Ident("exports"))

        // optional getter definition
        val getterDef = {
          js.StringLiteral("get") -> js.Function(arrow = false, Nil, {
            js.Return(genSelectStatic(cd.encodedName, field))
          })
        }

        // Options passed to the defineProperty method
        val descriptor = js.ObjectConstr(
            getterDef ::
            (js.StringLiteral("configurable") -> js.BooleanLiteral(true)) ::
            Nil
        )

        WithGlobals(js.Apply(defProp,
            exportsVarRef :: js.StringLiteral(exportName) :: descriptor :: Nil))
    }
  }

  // Helpers

  private def envFunctionDef(field: String, subField: String,
      args: List[js.ParamDef], body: js.Tree, origName: Option[String] = None)(
      implicit pos: Position): js.FunctionDef = {

    val globalVar = envField(field, subField, origName)
    val globalVarIdent = globalVar.ident
    js.FunctionDef(globalVarIdent, args, body)
  }

  private def envFieldDef(field: String, subField: String, value: js.Tree,
      origName: Option[String] = None, mutable: Boolean = false,
      keepFunctionExpression: Boolean = false)(
      implicit pos: Position): js.Tree = {
    val globalVar = envField(field, subField, origName)
    val globalVarIdent = globalVar.ident

    if (esFeatures.useECMAScript2015) {
      genLet(globalVarIdent, mutable, value)
    } else {
      value match {
        case js.Function(false, args, body) =>
          // Make sure the function has a meaningful `name` property
          val functionExpr = js.FunctionDef(globalVarIdent, args, body)
          if (keepFunctionExpression)
            js.VarDef(globalVarIdent, Some(functionExpr))
          else
            functionExpr
        case _ =>
          js.VarDef(globalVarIdent, Some(value))
      }
    }
  }

  /** Gen JS code for an [[ModuleInitializer]]. */
  def genModuleInitializer(moduleInitializer: ModuleInitializer): js.Tree = {
    import TreeDSL._
    import Definitions.BoxedStringClass

    implicit val pos = Position.NoPosition

    moduleInitializer match {
      case ModuleInitializer.VoidMainMethod(moduleClassName, mainMethodName) =>
        js.Apply(genLoadModule(moduleClassName) DOT mainMethodName, Nil)

      case ModuleInitializer.MainMethodWithArgs(moduleClassName, mainMethodName,
          args) =>
        val stringArrayTpe = ArrayType(ArrayTypeRef(BoxedStringClass, 1))
        js.Apply(genLoadModule(moduleClassName) DOT mainMethodName,
            genArrayValue(stringArrayTpe, args.map(js.StringLiteral(_))) :: Nil)
    }
  }

}

private[emitter] object ClassEmitter {
  // TODO We should compute all of those from the Class Hierarchy

  private val CharSequenceClass = "jl_CharSequence"
  private val SerializableClass = "Ljava_io_Serializable"
  private val ComparableClass = "jl_Comparable"
  private val NumberClass = "jl_Number"

  private val NonObjectAncestorsOfStringClass =
    Set(CharSequenceClass, ComparableClass, SerializableClass)
  private val NonObjectAncestorsOfBoxedCharacterClass =
    Set(ComparableClass, SerializableClass)
  private val NonObjectAncestorsOfHijackedNumberClasses =
    Set(NumberClass, ComparableClass, SerializableClass)
  private val NonObjectAncestorsOfBoxedBooleanClass =
    Set(ComparableClass, SerializableClass)

  private[emitter] val AncestorsOfHijackedClasses = Set(
      Definitions.ObjectClass,
      CharSequenceClass,
      SerializableClass,
      ComparableClass,
      NumberClass
  )

  private val ClassesWhoseDataReferToTheirInstanceTests =
    AncestorsOfHijackedClasses + Definitions.BoxedStringClass

  private final val ThrowableClass = "jl_Throwable"

  def shouldExtendJSError(linkedClass: LinkedClass): Boolean = {
    linkedClass.name.name == ThrowableClass &&
    linkedClass.superClass.exists(_.name == Definitions.ObjectClass)
  }
}
