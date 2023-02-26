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
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Position._
import org.scalajs.ir.Transformers._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker._
import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable._
import org.scalajs.linker.standard._
import org.scalajs.linker.backend.javascript.{Trees => js}

import CheckedBehavior.Unchecked

import EmitterNames._

/** Emitter for the skeleton of classes. */
private[emitter] final class ClassEmitter(sjsGen: SJSGen) {

  private val functionEmitter = new FunctionEmitter(sjsGen)

  import ClassEmitter._
  import functionEmitter._
  import sjsGen._
  import jsGen._
  import config._
  import nameGen._
  import varGen._

  def buildClass(tree: LinkedClass, useESClass: Boolean, ctor: js.Tree,
      memberDefs: List[js.MethodDef], exportedDefs: List[js.Tree])(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {

    implicit val pos = tree.pos

    val className = tree.name.name
    def allES6Defs = {
      js.Block(ctor +: (memberDefs ++ exportedDefs))(tree.pos) match {
        case js.Block(allDefs) => allDefs
        case js.Skip()         => Nil
        case oneDef            => List(oneDef)
      }
    }

    def allES5Defs(classVar: js.Tree) = {
      WithGlobals(js.Block(
          ctor, assignES5ClassMembers(classVar, memberDefs), js.Block(exportedDefs: _*)))
    }

    if (!tree.kind.isJSClass) {
      assert(tree.jsSuperClass.isEmpty, className)

      if (useESClass) {
        val parentVarWithGlobals = for (parentIdent <- tree.superClass) yield {
          implicit val pos = parentIdent.pos
          if (shouldExtendJSError(tree)) untrackedGlobalRef("Error")
          else WithGlobals(globalVar("c", parentIdent.name))
        }

        WithGlobals.option(parentVarWithGlobals).flatMap { parentVar =>
          globalClassDef("c", className, parentVar, allES6Defs)
        }
      } else {
        allES5Defs(globalVar("c", className))
      }
    } else {
      // Wrap the entire class def in an accessor function
      import TreeDSL._

      val genStoreJSSuperClass = tree.jsSuperClass.map { jsSuperClass =>
        for (rhs <- desugarExpr(jsSuperClass, resultType = AnyType)) yield {
          js.VarDef(fileLevelVar("superClass").ident, Some(rhs))
        }
      }

      val classValueIdent = fileLevelVarIdent("b", genName(className))
      val classValueVar = js.VarRef(classValueIdent)
      val createClassValueVar = genEmptyMutableLet(classValueIdent)

      val entireClassDefWithGlobals = if (useESClass) {
        genJSSuperCtor(tree).map { jsSuperClass =>
          classValueVar := js.ClassDef(Some(classValueIdent), Some(jsSuperClass), allES6Defs)
        }
      } else {
        allES5Defs(classValueVar)
      }

      val classDefStatsWithGlobals = for {
        optStoreJSSuperClass <- WithGlobals.option(genStoreJSSuperClass)
        entireClassDef <- entireClassDefWithGlobals
        createStaticFields <- genCreateStaticFieldsOfJSClass(tree)
      } yield {
        optStoreJSSuperClass.toList ::: entireClassDef :: createStaticFields
      }

      tree.jsClassCaptures.fold {
        for {
          classDefStats <- classDefStatsWithGlobals
          body = js.Block(
              js.If(!classValueVar, {
                js.Block(
                    classDefStats :::
                    genClassInitialization(tree)
                )
              }, {
                js.Skip()
              }),
              js.Return(classValueVar)
          )
          createAccessor <- globalFunctionDef("a", className, Nil, None, body)
        } yield {
          js.Block(createClassValueVar, createAccessor)
        }
      } { jsClassCaptures =>
        val captureParamDefs = for (param <- jsClassCaptures) yield {
          implicit val pos = param.pos
          val ident = fileLevelVarIdent("cc", genName(param.name.name),
              param.originalName.orElse(param.name.name))
          js.ParamDef(ident)
        }

        assert(!hasClassInitializer(tree),
            s"Found a class initializer in the non-top-level class $className")

        classDefStatsWithGlobals.flatMap { classDefStats =>
          val body = js.Block(
              createClassValueVar ::
              classDefStats :::
              js.Return(classValueVar) ::
              Nil
          )

          globalFunctionDef("a", className, captureParamDefs, None, body)
        }
      }
    }
  }

  /** Extracts the inlineable init method, if there is one. */
  def extractInlineableInit(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): (Option[MethodDef], List[MethodDef]) = {

    if (globalKnowledge.hasInlineableInit(tree.className)) {
      val (constructors, otherMethods) = tree.methods.partition { m =>
        m.flags.namespace == MemberNamespace.Constructor
      }
      assert(constructors.size == 1,
          s"Found ${constructors.size} constructors in class " +
          s"${tree.className} which has an inlined init.")
      (Some(constructors.head), otherMethods)
    } else {
      (None, tree.methods)
    }
  }

  /** Generates the JS constructor for a class. */
  def genConstructor(tree: LinkedClass, useESClass: Boolean,
      initToInline: Option[MethodDef])(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {

    assert(tree.kind.isAnyNonNativeClass)
    assert(tree.superClass.isDefined || tree.name.name == ObjectClass,
        s"Class ${tree.name.name} is missing a parent class")

    if (useESClass)
      genES6Constructor(tree, initToInline)
    else
      genES5Constructor(tree, initToInline)
  }

  /** Generates the JS constructor for a class, ES5 style. */
  private def genES5Constructor(tree: LinkedClass,
      initToInline: Option[MethodDef])(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    import TreeDSL._
    implicit val pos = tree.pos

    val className = tree.name.name

    def chainPrototypeWithLocalCtor(ctorVar: js.Tree, superCtor: js.Tree): js.Tree = {
      val dummyCtor = fileLevelVar("hh", genName(className))

      js.Block(
          js.DocComment("@constructor"),
          genConst(dummyCtor.ident, js.Function(false, Nil, None, js.Skip())),
          dummyCtor.prototype := superCtor.prototype,
          ctorVar.prototype := js.New(dummyCtor, Nil)
      )
    }

    if (!tree.kind.isJSClass) {
      val ctorVar = globalVar("c", className)

      val chainProtoWithGlobals = tree.superClass match {
        case None =>
          WithGlobals(js.Skip())

        case Some(_) if shouldExtendJSError(tree) =>
          untrackedGlobalRef("Error").map(chainPrototypeWithLocalCtor(ctorVar, _))

        case Some(parentIdent) =>
          WithGlobals(ctorVar.prototype := js.New(globalVar("h", parentIdent.name), Nil))
      }

      for {
        ctorFun <- genJSConstructorFun(tree, initToInline, forESClass = false)
        realCtorDef <-
            globalFunctionDef("c", className, ctorFun.args, ctorFun.restParam, ctorFun.body)
        inheritableCtorDef <-
            globalFunctionDef("h", className, Nil, None, js.Skip())
        chainProto <- chainProtoWithGlobals
      } yield {
        js.Block(
            // Real constructor
            js.DocComment("@constructor"),
            realCtorDef,
            chainProto,
            genIdentBracketSelect(ctorVar.prototype, "constructor") := ctorVar,

            // Inheritable constructor
            js.DocComment("@constructor"),
            inheritableCtorDef,
            globalVar("h", className).prototype := ctorVar.prototype
        )
      }
    } else {
      for {
        ctorFun <- genConstructorFunForJSClass(tree)
        superCtor <- genJSSuperCtor(tree)
      } yield {
        val ctorVar = fileLevelVar("b", genName(className))

        js.Block(
            js.DocComment("@constructor"),
            ctorVar := ctorFun,
            chainPrototypeWithLocalCtor(ctorVar, superCtor),
            genIdentBracketSelect(ctorVar.prototype, "constructor") := ctorVar
        )
      }
    }
  }

  /** Generates the JS constructor for a class, ES6 style. */
  private def genES6Constructor(tree: LinkedClass,
      initToInline: Option[MethodDef])(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    implicit val pos = tree.pos

    if (tree.kind.isJSClass) {
      for (fun <- genConstructorFunForJSClass(tree)) yield {
        js.MethodDef(static = false, js.Ident("constructor"),
            fun.args, fun.restParam, fun.body)
      }
    } else {
      val jsConstructorFunWithGlobals =
        genJSConstructorFun(tree, initToInline, forESClass = true)

      for (jsConstructorFun <- jsConstructorFunWithGlobals) yield {
        val js.Function(_, args, restParam, body) = jsConstructorFun

        def isTrivialCtorBody: Boolean = body match {
          case js.Skip()                 => true
          case js.Apply(js.Super(), Nil) => true
          case _                         => false
        }

        if (args.isEmpty && isTrivialCtorBody)
          js.Skip()
        else
          js.MethodDef(static = false, js.Ident("constructor"), args, restParam, body)
      }
    }
  }

  private def genJSSuperCtor(tree: LinkedClass)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): WithGlobals[js.Tree] = {
    assert(tree.kind.isJSClass)

    if (tree.jsSuperClass.isDefined) {
      WithGlobals(fileLevelVar("superClass"))
    } else {
      genJSClassConstructor(tree.superClass.get.name,
          keepOnlyDangerousVarNames = true)
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
      initToInline: Option[MethodDef], forESClass: Boolean)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Function] = {

    implicit val pos = tree.pos

    val superCtorCallAndFieldDefs = if (forESClass) {
      val fieldDefs = genFieldDefsOfScalaClass(tree.className, tree.fields)
      if (tree.superClass.isEmpty)
        fieldDefs
      else
        js.Apply(js.Super(), Nil) :: fieldDefs
    } else {
      val allFields =
        globalKnowledge.getAllScalaClassFieldDefs(tree.className)
      allFields.flatMap { classAndFields =>
        genFieldDefsOfScalaClass(classAndFields._1, classAndFields._2)
      }
    }

    initToInline.fold {
      WithGlobals(
          js.Function(arrow = false, Nil, None, js.Block(superCtorCallAndFieldDefs)))
    } { initMethodDef =>
      val generatedInitMethodFunWithGlobals = {
        implicit val pos = initMethodDef.pos
        val initMethodBody = initMethodDef.body.getOrElse {
          throw new AssertionError("Cannot generate an abstract constructor")
        }
        assert(initMethodDef.resultType == NoType,
            s"Found a constructor with type ${initMethodDef.resultType} at $pos")
        desugarToFunction(tree.className, initMethodDef.args, initMethodBody,
            resultType = NoType)
      }

      for (generatedInitMethodFun <- generatedInitMethodFunWithGlobals) yield {
        val js.Function(arrow, args, restParam, initMethodFunBody) = generatedInitMethodFun
        js.Function(arrow, args, restParam,
            js.Block(superCtorCallAndFieldDefs ::: initMethodFunBody :: Nil))
      }
    }
  }

  private def genConstructorFunForJSClass(tree: LinkedClass)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Function] = {
    implicit val pos = tree.pos

    require(tree.kind.isJSClass)

    val JSConstructorDef(_, params, restParam, body) = tree.jsConstructorDef.getOrElse {
      throw new IllegalArgumentException(
          s"${tree.className} does not have an exported constructor")
    }

    desugarToFunction(tree.className, params, restParam, body)
  }

  /** Generates the creation of fields for a Scala class. */
  private def genFieldDefsOfScalaClass(className: ClassName,
      fields: List[AnyFieldDef])(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): List[js.Tree] = {
    for {
      anyField <- fields
      if !anyField.flags.namespace.isStatic
    } yield {
      val field = anyField.asInstanceOf[FieldDef]
      implicit val pos = field.pos
      js.Assign(genSelect(js.This(), className, field.name, field.originalName),
          genZeroOf(field.ftpe))
    }
  }

  /** Generates the creation of the static fields for a Scala class. */
  def genCreateStaticFieldsOfScalaClass(tree: LinkedClass)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[List[js.Tree]] = {
    val defs = for {
      field @ FieldDef(flags, FieldIdent(name), origName, ftpe) <- tree.fields
      if flags.namespace.isStatic
    } yield {
      implicit val pos = field.pos

      val varScope = (tree.className, name)
      val value = genZeroOf(ftpe)

      if (flags.isMutable)
        globallyMutableVarDef("t", "u", varScope, value, origName.orElse(name))
      else
        globalVarDef("t", varScope, value, origName.orElse(name))
    }

    WithGlobals.list(defs)
  }

  /** Generates the creation of the private JS field defs for a JavaScript
   *  class.
   */
  def genCreatePrivateJSFieldDefsOfJSClass(tree: LinkedClass)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[List[js.Tree]] = {
    val defs = for {
      field @ FieldDef(flags, FieldIdent(name), origName, _) <- tree.fields
      if !flags.namespace.isStatic
    } yield {
      implicit val pos = field.pos

      val symbolValueWithGlobals = {
        def description = origName.getOrElse(name).toString()
        val args =
          if (semantics.productionMode) Nil
          else js.StringLiteral(description) :: Nil
        genCallPolyfillableBuiltin(PolyfillableBuiltin.PrivateSymbolBuiltin, args: _*)
      }

      symbolValueWithGlobals.flatMap { symbolValue =>
        globalVarDef("r", (tree.className, name), symbolValue, origName.orElse(name))
      }
    }

    WithGlobals.list(defs)
  }

  /** Generates the creation of the static fields for a JavaScript class. */
  private def genCreateStaticFieldsOfJSClass(tree: LinkedClass)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[List[js.Tree]] = {
    val className = tree.className
    val statsWithGlobals = for {
      field <- tree.fields
      if field.flags.namespace.isStatic
    } yield {
      implicit val pos = field.pos
      val classVarRef = fileLevelVar("b", genName(className))
      val zero = genBoxedZeroOf(field.ftpe)
      field match {
        case FieldDef(_, name, originalName, _) =>
          WithGlobals(
              js.Assign(js.DotSelect(classVarRef, genMemberFieldIdent(name, originalName)), zero))
        case JSFieldDef(_, name, _) =>
          for (propName <- genMemberNameTree(name))
            yield js.Assign(genPropSelect(classVarRef, propName), zero)
      }
    }
    WithGlobals.list(statsWithGlobals)
  }

  /** Does the class need static initialization generated by `genStaticInitialization`? */
  def needStaticInitialization(tree: LinkedClass): Boolean = {
    tree.methods.exists { m =>
      m.flags.namespace == MemberNamespace.StaticConstructor &&
      m.methodName.isStaticInitializer
    }
  }

  /** Generates the static initializer invocation of a class. */
  def genStaticInitialization(tree: LinkedClass)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): List[js.Tree] = {
    implicit val pos = tree.pos
    val field = globalVar("sct", (tree.className, StaticInitializerName),
        StaticInitializerOriginalName)
    js.Apply(field, Nil) :: Nil
  }

  /** Generates the class initializer invocation of a class. */
  private def genClassInitialization(tree: LinkedClass)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): List[js.Tree] = {
    implicit val pos = tree.pos
    if (hasClassInitializer(tree)) {
      val field = globalVar("sct", (tree.className, ClassInitializerName),
          ClassInitializerOriginalName)
      js.Apply(field, Nil) :: Nil
    } else {
      Nil
    }
  }

  private def hasClassInitializer(tree: LinkedClass): Boolean = {
    tree.methods.exists { m =>
      m.flags.namespace == MemberNamespace.StaticConstructor &&
      m.methodName.isClassInitializer
    }
  }

  def genMemberMethod(className: ClassName, method: MethodDef)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.MethodDef] = {
    assert(method.flags.namespace == MemberNamespace.Public)

    implicit val pos = method.pos

    for {
      methodFun <- desugarToFunction(className, method.args, method.body.get, method.resultType)
    } yield {
      val jsMethodName = genMemberMethodIdent(method.name, method.originalName)
      js.MethodDef(static = false, jsMethodName, methodFun.args, methodFun.restParam, methodFun.body)
    }
  }

  def genStaticLikeMethod(className: ClassName, method: MethodDef)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    val methodBody = method.body.getOrElse(
        throw new AssertionError("Cannot generate an abstract method"))

    implicit val pos = method.pos

    val namespace = method.flags.namespace

    val methodFun0WithGlobals = {
      if (namespace.isStatic) {
        desugarToFunction(className, method.args, methodBody, method.resultType)
      } else {
        desugarToFunctionWithExplicitThis(className, method.args, methodBody,
            method.resultType)
      }
    }

    methodFun0WithGlobals.flatMap { methodFun0 =>
      val methodFun = if (namespace == MemberNamespace.Constructor) {
        // init methods have to return `this` so that we can chain them to `new`
        js.Function(arrow = false, methodFun0.args, methodFun0.restParam, {
          implicit val pos = methodFun0.body.pos
          js.Block(
              methodFun0.body,
              js.Return(methodFun0.args.head.ref))
        })(methodFun0.pos)
      } else {
        methodFun0
      }

      val field = namespace match {
        case MemberNamespace.Public            => "f"
        case MemberNamespace.Private           => "p"
        case MemberNamespace.PublicStatic      => "s"
        case MemberNamespace.PrivateStatic     => "ps"
        case MemberNamespace.Constructor       => "ct"
        case MemberNamespace.StaticConstructor => "sct"
      }

      val methodName = method.name.name

      globalFunctionDef(field, (className, methodName), methodFun.args,
          methodFun.restParam, methodFun.body, method.originalName.orElse(methodName))
    }
  }

  /** Generates a JS method. */
  private def genJSMethod(tree: LinkedClass, useESClass: Boolean,
      method: JSMethodDef)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    implicit val pos = method.pos

    val namespace = method.flags.namespace
    assert(!namespace.isPrivate && !namespace.isConstructor)

    for {
      methodFun <- desugarToFunction(tree.className, method.args, method.restParam, method.body, AnyType)
      propName <- genMemberNameTree(method.name)
    } yield {
      if (useESClass) {
        js.MethodDef(static = namespace.isStatic, propName, methodFun.args, methodFun.restParam, methodFun.body)
      } else {
        val targetObject = exportTargetES5(tree, namespace)
        js.Assign(genPropSelect(targetObject, propName), methodFun)
      }
    }
  }

  /** Generates a property. */
  private def genJSProperty(tree: LinkedClass, useESClass: Boolean,
      property: JSPropertyDef)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    if (useESClass)
      genJSPropertyES6(tree.className, property)
    else
      genJSPropertyES5(tree, property)
  }

  private def genJSPropertyES5(tree: LinkedClass, property: JSPropertyDef)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    implicit val pos = property.pos

    val targetObject = exportTargetES5(tree, property.flags.namespace)

    // optional getter definition
    val optGetterWithGlobals = property.getterBody map { body =>
      desugarToFunction(tree.className, Nil, body, resultType = AnyType)
    }

    // optional setter definition
    val optSetterWithGlobals = property.setterArgAndBody map {
      case (arg, body) =>
        desugarToFunction(tree.className, arg :: Nil, body, resultType = NoType)
    }

    for {
      propName <- desugarExpr(property.name, resultType = AnyType)
      getter <- WithGlobals.option(optGetterWithGlobals)
      setter <- WithGlobals.option(optSetterWithGlobals)
      descriptor = (
          getter.map("get" -> _).toList :::
          setter.map("set" -> _).toList :::
          List("configurable" -> js.BooleanLiteral(true))
      )
      tree <- genDefineProperty(targetObject, propName, descriptor)
    } yield {
      tree
    }
  }

  private def genJSPropertyES6(className: ClassName, property: JSPropertyDef)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    implicit val pos = property.pos

    val static = property.flags.namespace.isStatic

    genMemberNameTree(property.name).flatMap { propName =>
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

  private def exportTargetES5(tree: LinkedClass, namespace: MemberNamespace)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): js.Tree = {
    import TreeDSL._

    val classVarRef =
      if (tree.kind.isJSClass) fileLevelVar("b", genName(tree.className))
      else globalVar("c", tree.className)

    if (namespace.isStatic) classVarRef
    else classVarRef.prototype
  }

  def genMemberNameTree(name: Tree)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.PropertyName] = {
    /* When it's a string literal but not "constructor", we can directly use a
     * string literal in the ES class definition. Otherwise, we must wrap the
     * expression in a `js.ComputedName`, which is `[tree]` in ES syntax.
     * We must exclude "constructor" because that would represent the actual
     * ES class constructor (which is taken care of by a JSConstructorDef),
     * whereas `["constructor"]` represents a non-constructor method called
     * "constructor".
     */
    name match {
      case StringLiteral(value) if value != "constructor" =>
        WithGlobals(js.StringLiteral(value)(name.pos))

      case _ =>
        implicit val pos = name.pos
        desugarExpr(name, resultType = AnyType).map(js.ComputedName(_))
    }
  }

  private def genMemberFieldIdent(ident: FieldIdent,
      originalName: OriginalName): js.Ident = {
    val jsName = genName(ident.name)
    js.Ident(jsName, genOriginalName(ident.name, originalName, jsName))(
        ident.pos)
  }

  private def genMemberMethodIdent(ident: MethodIdent,
      originalName: OriginalName): js.Ident = {
    val jsName = genName(ident.name)
    js.Ident(jsName, genOriginalName(ident.name, originalName, jsName))(
        ident.pos)
  }

  def needInstanceTests(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): Boolean = {
    tree.hasInstanceTests || (tree.hasRuntimeTypeInfo &&
        globalKnowledge.isAncestorOfHijackedClass(tree.className))
  }

  def genInstanceTests(tree: LinkedClass)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    for {
      single <- genSingleInstanceTests(tree)
      array <- genArrayInstanceTests(tree)
    } yield {
      js.Block(single ::: array)(tree.pos)
    }
  }

  private def genSingleInstanceTests(tree: LinkedClass)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[List[js.Tree]] = {
    import TreeDSL._

    implicit val pos = tree.pos

    val className = tree.name.name

    // Instance tests for java.lang.Object are generated by the CoreJSLib
    assert(className != ObjectClass,
        "cannot call genSingleInstanceTests for java.lang.Object")

    val isHijackedClass =
      tree.kind == ClassKind.HijackedClass

    if (tree.kind.isClass || tree.kind == ClassKind.Interface || isHijackedClass) {
      val displayName = className.nameString

      val objParam = js.ParamDef(js.Ident("obj"))
      val obj = objParam.ref

      val isExpression = if (isHijackedClass) {
        genIsInstanceOfHijackedClass(obj, className)
      } else {
        val baseTest = if (tree.kind.isClass) {
          genIsInstanceOfClass(obj, className)
        } else {
          !(!(
              genIsScalaJSObject(obj) &&
              genIsClassNameInAncestors(className,
                  obj DOT "$classData" DOT "ancestors")
          ))
        }

        val hijacked = globalKnowledge.hijackedDescendants(className)
        if (hijacked.nonEmpty) {
          val orderedSubset = subsetOfHijackedClassesOrderedForTypeTests(hijacked)
          orderedSubset.foldLeft(baseTest) {
            case (test, hijackedClass) =>
              test || genIsInstanceOfHijackedClass(obj, hijackedClass)
          }
        } else {
          baseTest
        }
      }

      val needIsFunction = !isHijackedClass && {
        !tree.kind.isClass ||
        globalKnowledge.isAncestorOfHijackedClass(className)
      }

      val createIsStatWithGlobals = if (needIsFunction) {
        globalFunctionDef("is", className, List(objParam), None, js.Return(isExpression))
      } else {
        WithGlobals(js.Skip())
      }

      val createAsStatWithGlobals = if (semantics.asInstanceOfs == Unchecked) {
        WithGlobals(js.Skip())
      } else {
        globalFunctionDef("as", className, List(objParam), None, js.Return {
          val isCond =
            if (needIsFunction) js.Apply(globalVar("is", className), List(obj))
            else isExpression

          js.If(isCond || (obj === js.Null()), {
            obj
          }, {
            genCallHelper("throwClassCastException",
                obj, js.StringLiteral(displayName))
          })
        })
      }

      for {
        createIsStat <- createIsStatWithGlobals
        createAsStat <- createAsStatWithGlobals
      } yield {
        List(createIsStat, createAsStat)
      }
    } else {
      WithGlobals(Nil)
    }
  }

  private def genArrayInstanceTests(tree: LinkedClass)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[List[js.Tree]] = {
    import TreeDSL._

    implicit val pos = tree.pos

    val className = tree.name.name
    val displayName = className.nameString

    // Array instance tests for java.lang.Object are generated by the CoreJSLib
    assert(className != ObjectClass,
        "cannot call genArrayInstanceTests for java.lang.Object")

    val objParam = js.ParamDef(js.Ident("obj"))
    val obj = objParam.ref

    val depthParam = js.ParamDef(js.Ident("depth"))
    val depth = depthParam.ref

    val createIsArrayOfStatWithGlobals = {
      globalFunctionDef("isArrayOf", className, List(objParam, depthParam), None, {
        js.Return(!(!({
          genIsScalaJSObject(obj) &&
          ((obj DOT "$classData" DOT "arrayDepth") === depth) &&
          genIsClassNameInAncestors(className,
              obj DOT "$classData" DOT "arrayBase" DOT "ancestors")
        })))
      })
    }

    val createAsArrayOfStatWithGlobals = if (semantics.asInstanceOfs == Unchecked) {
      WithGlobals(js.Skip())
    } else {
      globalFunctionDef("asArrayOf", className, List(objParam, depthParam), None, {
        js.Return {
          js.If(js.Apply(globalVar("isArrayOf", className), List(obj, depth)) ||
              (obj === js.Null()), {
            obj
          }, {
            genCallHelper("throwArrayCastException",
                obj, js.StringLiteral("L"+displayName+";"), depth)
          })
        }
      })
    }

    for {
      createIsArrayOfStat <- createIsArrayOfStatWithGlobals
      createAsArrayOfStat <- createAsArrayOfStatWithGlobals
    } yield {
      List(createIsArrayOfStat, createAsArrayOfStat)
    }
  }

  private def genIsScalaJSObject(obj: js.Tree)(implicit pos: Position): js.Tree = {
    import TreeDSL._
    obj && (obj DOT "$classData")
  }

  private def genIsClassNameInAncestors(className: ClassName,
      ancestors: js.Tree)(
      implicit pos: Position): js.Tree = {
    import TreeDSL._
    ancestors DOT genName(className)
  }

  def genTypeData(tree: LinkedClass)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    import TreeDSL._

    implicit val pos = tree.pos

    val className = tree.name.name
    val kind = tree.kind

    val isObjectClass =
      className == ObjectClass
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
        globalVar("d", parent.name)
      }
    } else {
      js.Undefined()
    }

    val ancestorsRecord = js.ObjectConstr(
        tree.ancestors.map(ancestor => (js.Ident(genName(ancestor)), js.IntLiteral(1))))

    val isInstanceFunWithGlobals: WithGlobals[js.Tree] = {
      if (globalKnowledge.isAncestorOfHijackedClass(className)) {
        /* Ancestors of hijacked classes, including java.lang.Object, have a
         * normal $is_pack_Class test but with a non-standard behavior.
         */
        WithGlobals(globalVar("is", className))
      } else if (HijackedClasses.contains(className)) {
        /* Hijacked classes have a special isInstanceOf test. */
        val xParam = js.ParamDef(js.Ident("x"))
        WithGlobals(genArrowFunction(List(xParam), None, js.Return {
          genIsInstanceOfHijackedClass(xParam.ref, className)
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
          WithGlobals(globalVar("noIsInstance", CoreVar))
        } else {
          for {
            jsCtor <- genJSClassConstructor(className, tree.jsNativeLoadSpec,
                keepOnlyDangerousVarNames = true)
          } yield {
            genArrowFunction(List(js.ParamDef(js.Ident("x"))), None, js.Return {
              js.VarRef(js.Ident("x")) instanceof jsCtor
            })
          }
        }
      } else {
        // For other classes, the isInstance function can be inferred.
        WithGlobals(js.Undefined())
      }
    }

    isInstanceFunWithGlobals.flatMap { isInstanceFun =>
      val allParams = List(
          js.ObjectConstr(List(js.Ident(genName(className)) -> js.IntLiteral(0))),
          js.BooleanLiteral(kind == ClassKind.Interface),
          js.StringLiteral(RuntimeClassNameMapperImpl.map(
              semantics.runtimeClassNameMapper, tree.fullName)),
          ancestorsRecord,
          isJSTypeParam,
          parentData,
          isInstanceFun
      )

      val prunedParams =
        allParams.reverse.dropWhile(_.isInstanceOf[js.Undefined]).reverse

      val typeData = js.Apply(js.New(globalVar("TypeData", CoreVar), Nil) DOT "initClass",
          prunedParams)

      globalVarDef("d", className, typeData)
    }
  }

  def genSetTypeData(tree: LinkedClass)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): js.Tree = {
    import TreeDSL._

    implicit val pos = tree.pos

    assert(tree.kind.isClass)

    globalVar("c", tree.name.name).prototype DOT "$classData" :=
      globalVar("d", tree.name.name)
  }

  def genModuleAccessor(tree: LinkedClass)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    import TreeDSL._

    implicit val pos = tree.pos

    val className = tree.name.name
    val tpe = ClassType(className)

    require(tree.kind.hasModuleAccessor,
        s"genModuleAccessor called with non-module class: $className")

    val moduleInstance = fileLevelVarIdent("n", genName(className))

    val createModuleInstanceField = genEmptyMutableLet(moduleInstance)

    val createAccessor = {
      val moduleInstanceVar = js.VarRef(moduleInstance)

      val assignModule = {
        moduleInstanceVar := {
          if (tree.kind == ClassKind.JSModuleClass) {
            js.New(
                genNonNativeJSClassConstructor(className),
                Nil)
          } else {
            js.New(globalVar("c", className), Nil)
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
            val decodedName = className.nameString.stripSuffix("$")
            genCallHelper("throwModuleInitError", js.StringLiteral(decodedName))
          }, js.Skip()))
      }

      val body = js.Block(initBlock, js.Return(moduleInstanceVar))

      globalFunctionDef("m", className, Nil, None, body)
    }

    createAccessor.map(js.Block(createModuleInstanceField, _))
  }

  def genExportedMember(tree: LinkedClass, useESClass: Boolean, member: JSMethodPropDef)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    member match {
      case m: JSMethodDef   => genJSMethod(tree, useESClass, m)
      case p: JSPropertyDef => genJSProperty(tree, useESClass, p)
    }
  }

  def genTopLevelExports(topLevelExports: List[LinkedTopLevelExport])(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[List[js.Tree]] = {
    val exportsWithGlobals = topLevelExports.map { topLevelExport =>
      implicit val pos = topLevelExport.tree.pos

      assert(moduleContext.moduleID.id == topLevelExport.tree.moduleID)

      topLevelExport.tree match {
        case TopLevelJSClassExportDef(_, exportName) =>
          genConstValueExportDef(
              exportName, genNonNativeJSClassConstructor(topLevelExport.owningClass))
        case TopLevelModuleExportDef(_, exportName) =>
          genConstValueExportDef(exportName, genLoadModule(topLevelExport.owningClass))
        case e: TopLevelMethodExportDef =>
          genTopLevelMethodExportDef(e)
        case e: TopLevelFieldExportDef =>
          genTopLevelFieldExportDef(topLevelExport.owningClass, e)
      }
    }

    WithGlobals.list(exportsWithGlobals)
  }

  private def genTopLevelMethodExportDef(tree: TopLevelMethodExportDef)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    import TreeDSL._

    val JSMethodDef(flags, StringLiteral(exportName), args, restParam, body) =
      tree.methodDef

    assert(flags.namespace == MemberNamespace.PublicStatic, exportName)

    implicit val pos = tree.pos

    val methodDefWithGlobals = desugarToFunction(args, restParam, body, AnyType)

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
        val field = fileLevelVar("e", exportName)
        val let = js.Let(field.ident, mutable = true, Some(exportedValue))
        val exportStat = js.Export((field.ident -> js.ExportName(exportName)) :: Nil)
        WithGlobals(js.Block(let, exportStat))

      case ModuleKind.CommonJSModule =>
        globalRef("exports").map { exportsVarRef =>
          js.Assign(
              genBracketSelect(exportsVarRef, js.StringLiteral(exportName)),
              exportedValue)
        }
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

  private def genTopLevelFieldExportDef(className: ClassName,
      tree: TopLevelFieldExportDef)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    import TreeDSL._

    val TopLevelFieldExportDef(_, exportName, field) = tree

    implicit val pos = tree.pos

    val varScope = (className, field.name)

    moduleKind match {
      case ModuleKind.NoModule =>
        /* Initial value of the export. Updates are taken care of explicitly
         * when we assign to the static field.
         */
        genAssignToNoModuleExportVar(exportName, globalVar("t", varScope))

      case ModuleKind.ESModule =>
        WithGlobals(globalVarExport("t", varScope, js.ExportName(exportName)))

      case ModuleKind.CommonJSModule =>
        globalRef("exports").flatMap { exportsVarRef =>
          genDefineProperty(
              exportsVarRef,
              js.StringLiteral(exportName),
              List(
                  "get" -> js.Function(arrow = false, Nil, None, {
                    js.Return(globalVar("t", varScope))
                  }),
                  "configurable" -> js.BooleanLiteral(true)
              )
          )
        }
    }
  }

  /** Gen JS code for an [[ModuleInitializer]]. */
  def genModuleInitializer(initializer: ModuleInitializer.Initializer)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    import ModuleInitializerImpl._

    implicit val pos = Position.NoPosition

    ModuleInitializerImpl.fromInitializer(initializer) match {
      case VoidMainMethod(className, mainMethodName) =>
        WithGlobals(js.Apply(globalVar("s", (className, mainMethodName)), Nil))

      case MainMethodWithArgs(className, mainMethodName, args) =>
        val stringArrayTypeRef = ArrayTypeRef(ClassRef(BoxedStringClass), 1)
        val argsArrayWithGlobals =
          genArrayValue(stringArrayTypeRef, args.map(js.StringLiteral(_)))
        for (argsArray <- argsArrayWithGlobals) yield {
          js.Apply(globalVar("s", (className, mainMethodName)), argsArray :: Nil)
        }
    }
  }

}

private[emitter] object ClassEmitter {
  private val StaticInitializerOriginalName: OriginalName =
    OriginalName("<stinit>")

  private val ClassInitializerOriginalName: OriginalName =
    OriginalName("<clinit>")

  def shouldExtendJSError(linkedClass: LinkedClass): Boolean = {
    linkedClass.name.name == ThrowableClass &&
    linkedClass.superClass.exists(_.name == ObjectClass)
  }
}
