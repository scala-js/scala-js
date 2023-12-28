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

  def buildClass(className: ClassName, kind: ClassKind, jsClassCaptures: Option[List[ParamDef]],
      hasClassInitializer: Boolean,
      superClass: Option[ClassIdent], jsSuperClass: Option[Tree], useESClass: Boolean, ctorDefs: List[js.Tree],
      memberDefs: List[js.MethodDef], exportedDefs: List[js.Tree])(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge, pos: Position): WithGlobals[List[js.Tree]] = {

    def allES6Defs = ctorDefs ::: memberDefs ::: exportedDefs

    def allES5Defs(classVar: js.Tree) =
      WithGlobals(ctorDefs ::: assignES5ClassMembers(classVar, memberDefs) ::: exportedDefs)

    if (!kind.isJSClass) {
      assert(jsSuperClass.isEmpty, className)

      if (useESClass) {
        val parentVarWithGlobals = for (parentIdent <- superClass) yield {
          implicit val pos = parentIdent.pos
          if (shouldExtendJSError(className, superClass)) untrackedGlobalRef("Error")
          else WithGlobals(globalVar(VarField.c, parentIdent.name))
        }

        WithGlobals.option(parentVarWithGlobals).flatMap { parentVar =>
          globalClassDef(VarField.c, className, parentVar, allES6Defs)
        }
      } else {
        allES5Defs(globalVar(VarField.c, className))
      }
    } else {
      // Wrap the entire class def in an accessor function
      import TreeDSL._

      val genStoreJSSuperClass = jsSuperClass.map { jsSuperClass =>
        for (rhs <- desugarExpr(jsSuperClass, resultType = AnyType)) yield {
          js.VarDef(fileLevelVar(VarField.superClass).ident, Some(rhs))
        }
      }

      val classValueIdent = fileLevelVarIdent(VarField.b, genName(className))
      val classValueVar = js.VarRef(classValueIdent)
      val createClassValueVar = genEmptyMutableLet(classValueIdent)

      val entireClassDefWithGlobals = if (useESClass) {
        genJSSuperCtor(superClass, jsSuperClass).map { jsSuperClass =>
          List(classValueVar := js.ClassDef(Some(classValueIdent), Some(jsSuperClass), allES6Defs))
        }
      } else {
        allES5Defs(classValueVar)
      }

      val classDefStatsWithGlobals = for {
        optStoreJSSuperClass <- WithGlobals.option(genStoreJSSuperClass)
        entireClassDef <- entireClassDefWithGlobals
        createStaticFields <- genCreateStaticFieldsOfJSClass(className)
      } yield {
        optStoreJSSuperClass.toList ::: entireClassDef ::: createStaticFields
      }

      jsClassCaptures.fold {
        for {
          classDefStats <- classDefStatsWithGlobals
          body = js.Block(
              js.If(!classValueVar, {
                js.Block(
                    classDefStats :::
                    genClassInitialization(className, hasClassInitializer)
                )
              }, {
                js.Skip()
              }),
              js.Return(classValueVar)
          )
          createAccessor <- globalFunctionDef(VarField.a, className, Nil, None, body)
        } yield {
          createClassValueVar :: createAccessor
        }
      } { jsClassCaptures =>
        val captureParamDefs = for (param <- jsClassCaptures) yield {
          implicit val pos = param.pos
          val ident = fileLevelVarIdent(VarField.cc, genName(param.name.name),
              param.originalName.orElse(param.name.name))
          js.ParamDef(ident)
        }

        assert(!hasClassInitializer,
            s"Found a class initializer in the non-top-level class $className")

        classDefStatsWithGlobals.flatMap { classDefStats =>
          val body = js.Block(
              createClassValueVar ::
              classDefStats :::
              js.Return(classValueVar) ::
              Nil
          )

          globalFunctionDef(VarField.a, className, captureParamDefs, None, body)
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

  /** Generates the JS constructor for a Scala class. */
  def genScalaClassConstructor(className: ClassName, superClass: Option[ClassIdent],
      useESClass: Boolean, initToInline: Option[MethodDef])(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge, pos: Position): WithGlobals[List[js.Tree]] = {

    assert(superClass.isDefined || className == ObjectClass,
        s"Class $className is missing a parent class")

    val jsConstructorFunWithGlobals =
      genJSConstructorFun(className, superClass, initToInline, useESClass)

    if (useESClass) {
      for (jsConstructorFun <- jsConstructorFunWithGlobals) yield {
        val js.Function(_, args, restParam, body) = jsConstructorFun

        def isTrivialCtorBody: Boolean = body match {
          case js.Skip()                 => true
          case js.Apply(js.Super(), Nil) => true
          case _                         => false
        }

        if (args.isEmpty && isTrivialCtorBody)
          Nil
        else
          js.MethodDef(static = false, js.Ident("constructor"), args, restParam, body) :: Nil
      }
    } else {
      import TreeDSL._

      val ctorVar = globalVar(VarField.c, className)

      val chainProtoWithGlobals = superClass match {
        case None =>
          WithGlobals.nil

        case Some(_) if shouldExtendJSError(className, superClass) =>
          untrackedGlobalRef("Error").map(chainPrototypeWithLocalCtor(className, ctorVar, _))

        case Some(parentIdent) =>
          WithGlobals(List(ctorVar.prototype := js.New(globalVar(VarField.h, parentIdent.name), Nil)))
      }

      for {
        ctorFun <- jsConstructorFunWithGlobals
        realCtorDef <-
            globalFunctionDef(VarField.c, className, ctorFun.args, ctorFun.restParam, ctorFun.body)
        inheritableCtorDef <-
            globalFunctionDef(VarField.h, className, Nil, None, js.Skip())
        chainProto <- chainProtoWithGlobals
      } yield {
        (
          // Real constructor
          js.DocComment("@constructor") ::
          realCtorDef :::
          chainProto :::
          (genIdentBracketSelect(ctorVar.prototype, "constructor") := ctorVar) ::

          // Inheritable constructor
          js.DocComment("@constructor") ::
          inheritableCtorDef :::
          (globalVar(VarField.h, className).prototype := ctorVar.prototype) :: Nil
        )
      }
    }
  }

  /** Generates the JS constructor for a JS class. */
  def genJSConstructor(className: ClassName, superClass: Option[ClassIdent],
      jsSuperClass: Option[Tree], useESClass: Boolean, jsConstructorDef: JSConstructorDef)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge, pos: Position): WithGlobals[List[js.Tree]] = {

    val JSConstructorDef(_, params, restParam, body) = jsConstructorDef
    val ctorFunWithGlobals = desugarToFunction(className, params, restParam, body)

    if (useESClass) {
      for (fun <- ctorFunWithGlobals) yield {
        js.MethodDef(static = false, js.Ident("constructor"),
            fun.args, fun.restParam, fun.body) :: Nil
      }
    } else {
      for {
        ctorFun <- ctorFunWithGlobals
        superCtor <- genJSSuperCtor(superClass, jsSuperClass)
      } yield {
        import TreeDSL._

        val ctorVar = fileLevelVar(VarField.b, genName(className))

        js.DocComment("@constructor") ::
        (ctorVar := ctorFun) ::
        chainPrototypeWithLocalCtor(className, ctorVar, superCtor) :::
        (genIdentBracketSelect(ctorVar.prototype, "constructor") := ctorVar) :: Nil
      }
    }
  }

  private def genJSSuperCtor(superClass: Option[ClassIdent], jsSuperClass: Option[Tree])(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): WithGlobals[js.Tree] = {
    if (jsSuperClass.isDefined) {
      WithGlobals(fileLevelVar(VarField.superClass))
    } else {
      genJSClassConstructor(superClass.get.name, keepOnlyDangerousVarNames = true)
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
  private def genJSConstructorFun(className: ClassName,
      superClass: Option[ClassIdent], initToInline: Option[MethodDef], forESClass: Boolean)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge, pos: Position): WithGlobals[js.Function] = {
    val superCtorCallAndFieldDefs = if (forESClass) {
      val fieldDefs = genFieldDefsOfScalaClass(className,
          globalKnowledge.getFieldDefs(className))
      if (superClass.isEmpty)
        fieldDefs
      else
        js.Apply(js.Super(), Nil) :: fieldDefs
    } else {
      val allFields =
        globalKnowledge.getAllScalaClassFieldDefs(className)
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
        desugarToFunction(className, initMethodDef.args, initMethodBody,
            resultType = NoType)
      }

      for (generatedInitMethodFun <- generatedInitMethodFunWithGlobals) yield {
        val js.Function(arrow, args, restParam, initMethodFunBody) = generatedInitMethodFun
        js.Function(arrow, args, restParam,
            js.Block(superCtorCallAndFieldDefs ::: initMethodFunBody :: Nil))
      }
    }
  }

  private def chainPrototypeWithLocalCtor(className: ClassName, ctorVar: js.Tree,
      superCtor: js.Tree)(implicit pos: Position): List[js.Tree] = {
    import TreeDSL._

    val dummyCtor = fileLevelVar(VarField.hh, genName(className))

    List(
        js.DocComment("@constructor"),
        genConst(dummyCtor.ident, js.Function(false, Nil, None, js.Skip())),
        dummyCtor.prototype := superCtor.prototype,
        ctorVar.prototype := js.New(dummyCtor, Nil)
    )
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
  def genCreateStaticFieldsOfScalaClass(className: ClassName)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[List[js.Tree]] = {
    val defs = for {
      field @ FieldDef(flags, FieldIdent(name), origName, ftpe) <- globalKnowledge.getFieldDefs(className)
      if flags.namespace.isStatic
    } yield {
      implicit val pos = field.pos

      val varScope = (className, name)
      val value = genZeroOf(ftpe)

      if (flags.isMutable)
        globallyMutableVarDef(VarField.t, VarField.u, varScope, value, origName.orElse(name))
      else
        globalVarDef(VarField.t, varScope, value, origName.orElse(name))
    }

    WithGlobals.flatten(defs)
  }

  /** Generates the creation of the private JS field defs for a JavaScript
   *  class.
   */
  def genCreatePrivateJSFieldDefsOfJSClass(className: ClassName)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[List[js.Tree]] = {
    val defs = for {
      field @ FieldDef(flags, FieldIdent(name), origName, _) <- globalKnowledge.getFieldDefs(className)
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
        globalVarDef(VarField.r, (className, name), symbolValue, origName.orElse(name))
      }
    }

    WithGlobals.flatten(defs)
  }

  /** Generates the creation of the static fields for a JavaScript class. */
  private def genCreateStaticFieldsOfJSClass(className: ClassName)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[List[js.Tree]] = {
    val statsWithGlobals = for {
      field <- globalKnowledge.getFieldDefs(className)
      if field.flags.namespace.isStatic
    } yield {
      implicit val pos = field.pos
      val classVarRef = fileLevelVar(VarField.b, genName(className))
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
  def genStaticInitialization(className: ClassName)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge, pos: Position): List[js.Tree] = {
    val field = globalVar(VarField.sct, (className, StaticInitializerName),
        StaticInitializerOriginalName)
    js.Apply(field, Nil) :: Nil
  }

  /** Generates the class initializer invocation of a class. */
  private def genClassInitialization(className: ClassName, hasClassInitializer: Boolean)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge, pos: Position): List[js.Tree] = {
    if (hasClassInitializer) {
      val field = globalVar(VarField.sct, (className, ClassInitializerName),
          ClassInitializerOriginalName)
      js.Apply(field, Nil) :: Nil
    } else {
      Nil
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
      globalKnowledge: GlobalKnowledge): WithGlobals[List[js.Tree]] = {
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
        case MemberNamespace.Public            => VarField.f
        case MemberNamespace.Private           => VarField.p
        case MemberNamespace.PublicStatic      => VarField.s
        case MemberNamespace.PrivateStatic     => VarField.ps
        case MemberNamespace.Constructor       => VarField.ct
        case MemberNamespace.StaticConstructor => VarField.sct
      }

      val methodName = method.name.name

      globalFunctionDef(field, (className, methodName), methodFun.args,
          methodFun.restParam, methodFun.body, method.originalName.orElse(methodName))
    }
  }

  /** Generates a JS method. */
  private def genJSMethod(className: ClassName, kind: ClassKind, useESClass: Boolean,
      method: JSMethodDef)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    implicit val pos = method.pos

    val namespace = method.flags.namespace
    assert(!namespace.isPrivate && !namespace.isConstructor)

    for {
      methodFun <- desugarToFunction(className, method.args, method.restParam, method.body, AnyType)
      propName <- genMemberNameTree(method.name)
    } yield {
      if (useESClass) {
        js.MethodDef(static = namespace.isStatic, propName, methodFun.args, methodFun.restParam, methodFun.body)
      } else {
        val targetObject = exportTargetES5(className, kind, namespace)
        js.Assign(genPropSelect(targetObject, propName), methodFun)
      }
    }
  }

  /** Generates a property. */
  private def genJSProperty(className: ClassName, kind: ClassKind, useESClass: Boolean,
      property: JSPropertyDef)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[List[js.Tree]] = {
    if (useESClass)
      genJSPropertyES6(className, property)
    else
      genJSPropertyES5(className, kind, property).map(_ :: Nil)
  }

  private def genJSPropertyES5(className: ClassName, kind: ClassKind, property: JSPropertyDef)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    implicit val pos = property.pos

    val targetObject = exportTargetES5(className, kind, property.flags.namespace)

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
      globalKnowledge: GlobalKnowledge): WithGlobals[List[js.Tree]] = {
    implicit val pos = property.pos

    val static = property.flags.namespace.isStatic

    genMemberNameTree(property.name).flatMap { propName =>
      val getterWithGlobals = property.getterBody.map { body =>
        for (fun <- desugarToFunction(className, Nil, body, resultType = AnyType))
          yield js.GetterDef(static, propName, fun.body)
      }

      val setterWithGlobals = property.setterArgAndBody.map { case (arg, body) =>
        for (fun <- desugarToFunction(className, arg :: Nil, body, resultType = NoType))
          yield js.SetterDef(static, propName, fun.args.head, fun.body)
      }

      for {
        getter <- WithGlobals.option(getterWithGlobals)
        setter <- WithGlobals.option(setterWithGlobals)
      } yield {
        getter.toList ::: setter.toList
      }
    }
  }

  private def exportTargetES5(className: ClassName, kind: ClassKind, namespace: MemberNamespace)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): js.Tree = {
    import TreeDSL._

    val classVarRef =
      if (kind.isJSClass) fileLevelVar(VarField.b, genName(className))
      else globalVar(VarField.c, className)

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

  def genInstanceTests(className: ClassName, kind: ClassKind)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge, pos: Position): WithGlobals[List[js.Tree]] = {
    for {
      single <- genSingleInstanceTests(className, kind)
      array <- genArrayInstanceTests(className)
    } yield {
      single ::: array
    }
  }

  private def genSingleInstanceTests(className: ClassName, kind: ClassKind)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge, pos: Position): WithGlobals[List[js.Tree]] = {
    import TreeDSL._

    // Instance tests for java.lang.Object are generated by the CoreJSLib
    assert(className != ObjectClass,
        "cannot call genSingleInstanceTests for java.lang.Object")

    val isHijackedClass = kind == ClassKind.HijackedClass

    if (kind.isClass || kind == ClassKind.Interface || isHijackedClass) {
      val displayName = className.nameString

      val objParam = js.ParamDef(js.Ident("obj"))
      val obj = objParam.ref

      val isExpression = if (isHijackedClass) {
        genIsInstanceOfHijackedClass(obj, className)
      } else {
        val baseTest = if (kind.isClass) {
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
        !kind.isClass ||
        globalKnowledge.isAncestorOfHijackedClass(className)
      }

      val createIsStatWithGlobals = if (needIsFunction) {
        globalFunctionDef(VarField.is, className, List(objParam), None, js.Return(isExpression))
      } else {
        WithGlobals.nil
      }

      val createAsStatWithGlobals = if (semantics.asInstanceOfs == Unchecked) {
        WithGlobals.nil
      } else {
        globalFunctionDef(VarField.as, className, List(objParam), None, js.Return {
          val isCond =
            if (needIsFunction) js.Apply(globalVar(VarField.is, className), List(obj))
            else isExpression

          js.If(isCond || (obj === js.Null()), {
            obj
          }, {
            genCallHelper(VarField.throwClassCastException,
                obj, js.StringLiteral(displayName))
          })
        })
      }

      for {
        createIsStat <- createIsStatWithGlobals
        createAsStat <- createAsStatWithGlobals
      } yield {
        createIsStat ::: createAsStat
      }
    } else {
      WithGlobals.nil
    }
  }

  private def genArrayInstanceTests(className: ClassName)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge, pos: Position): WithGlobals[List[js.Tree]] = {
    import TreeDSL._

    val displayName = className.nameString

    // Array instance tests for java.lang.Object are generated by the CoreJSLib
    assert(className != ObjectClass,
        "cannot call genArrayInstanceTests for java.lang.Object")

    val objParam = js.ParamDef(js.Ident("obj"))
    val obj = objParam.ref

    val depthParam = js.ParamDef(js.Ident("depth"))
    val depth = depthParam.ref

    val createIsArrayOfStatWithGlobals = {
      globalFunctionDef(VarField.isArrayOf, className, List(objParam, depthParam), None, {
        js.Return(!(!({
          genIsScalaJSObject(obj) &&
          ((obj DOT "$classData" DOT "arrayDepth") === depth) &&
          genIsClassNameInAncestors(className,
              obj DOT "$classData" DOT "arrayBase" DOT "ancestors")
        })))
      })
    }

    val createAsArrayOfStatWithGlobals = if (semantics.asInstanceOfs == Unchecked) {
      WithGlobals.nil
    } else {
      globalFunctionDef(VarField.asArrayOf, className, List(objParam, depthParam), None, {
        js.Return {
          js.If(js.Apply(globalVar(VarField.isArrayOf, className), List(obj, depth)) ||
              (obj === js.Null()), {
            obj
          }, {
            genCallHelper(VarField.throwArrayCastException,
                obj, js.StringLiteral("L"+displayName+";"), depth)
          })
        }
      })
    }

    for {
      createIsArrayOfStat <- createIsArrayOfStatWithGlobals
      createAsArrayOfStat <- createAsArrayOfStatWithGlobals
    } yield {
      createIsArrayOfStat ::: createAsArrayOfStat
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

  def genTypeData(className: ClassName, kind: ClassKind,
      superClass: Option[ClassIdent], ancestors: List[ClassName],
      jsNativeLoadSpec: Option[JSNativeLoadSpec])(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge, pos: Position): WithGlobals[List[js.Tree]] = {
    import TreeDSL._

    val isObjectClass =
      className == ObjectClass
    val isJSType =
      kind.isJSType

    val isJSTypeParam =
      if (isJSType) js.BooleanLiteral(true)
      else js.Undefined()

    val parentData = if (globalKnowledge.isParentDataAccessed) {
      superClass.fold[js.Tree] {
        if (isObjectClass) js.Null()
        else js.Undefined()
      } { parent =>
        globalVar(VarField.d, parent.name)
      }
    } else {
      js.Undefined()
    }

    val ancestorsRecord = js.ObjectConstr(
        ancestors.map(ancestor => (js.Ident(genName(ancestor)), js.IntLiteral(1))))

    val isInstanceFunWithGlobals: WithGlobals[js.Tree] = {
      if (globalKnowledge.isAncestorOfHijackedClass(className)) {
        /* Ancestors of hijacked classes, including java.lang.Object, have a
         * normal $is_pack_Class test but with a non-standard behavior.
         */
        WithGlobals(globalVar(VarField.is, className))
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
          WithGlobals(globalVar(VarField.noIsInstance, CoreVar))
        } else if (kind == ClassKind.JSClass && !globalKnowledge.hasInstances(className)) {
          /* We need to constant-fold the instance test, to avoid emitting
           * `x instanceof $a_TheClass()`, because `$a_TheClass` won't be
           * declared at all. Otherwise, we'd get a `ReferenceError`.
           */
          WithGlobals(genArrowFunction(List(js.ParamDef(js.Ident("x"))), None, js.Return {
            js.BooleanLiteral(false)
          }))
        } else {
          for {
            jsCtor <- genJSClassConstructor(className, jsNativeLoadSpec,
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
              semantics.runtimeClassNameMapper, className.nameString)),
          ancestorsRecord,
          isJSTypeParam,
          parentData,
          isInstanceFun
      )

      val prunedParams =
        allParams.reverse.dropWhile(_.isInstanceOf[js.Undefined]).reverse

      val typeData = js.Apply(js.New(globalVar(VarField.TypeData, CoreVar), Nil) DOT "initClass",
          prunedParams)

      globalVarDef(VarField.d, className, typeData)
    }
  }

  def genSetTypeData(className: ClassName)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge, pos: Position): js.Tree = {
    import TreeDSL._

    globalVar(VarField.c, className).prototype DOT "$classData" := globalVar(VarField.d, className)
  }

  def genModuleAccessor(className: ClassName, kind: ClassKind)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge, pos: Position): WithGlobals[List[js.Tree]] = {
    import TreeDSL._

    val tpe = ClassType(className)

    require(kind.hasModuleAccessor,
        s"genModuleAccessor called with non-module class: $className")

    val moduleInstance = fileLevelVarIdent(VarField.n, genName(className))

    val createModuleInstanceField = genEmptyMutableLet(moduleInstance)

    val createAccessor = {
      val moduleInstanceVar = js.VarRef(moduleInstance)

      val assignModule = {
        moduleInstanceVar := {
          if (kind == ClassKind.JSModuleClass) {
            js.New(
                genNonNativeJSClassConstructor(className),
                Nil)
          } else {
            js.New(globalVar(VarField.c, className), Nil)
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
            genCallHelper(VarField.throwModuleInitError, js.StringLiteral(decodedName))
          }, js.Skip()))
      }

      val body = js.Block(initBlock, js.Return(moduleInstanceVar))

      globalFunctionDef(VarField.m, className, Nil, None, body)
    }

    createAccessor.map(createModuleInstanceField :: _)
  }

  def genExportedMember(className: ClassName, kind: ClassKind, useESClass: Boolean, member: JSMethodPropDef)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[List[js.Tree]] = {
    member match {
      case m: JSMethodDef   => genJSMethod(className, kind, useESClass, m).map(_ :: Nil)
      case p: JSPropertyDef => genJSProperty(className, kind, useESClass, p)
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
        val field = fileLevelVar(VarField.e, exportName)
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
        genAssignToNoModuleExportVar(exportName, globalVar(VarField.t, varScope))

      case ModuleKind.ESModule =>
        WithGlobals(globalVarExport(VarField.t, varScope, js.ExportName(exportName)))

      case ModuleKind.CommonJSModule =>
        globalRef("exports").flatMap { exportsVarRef =>
          genDefineProperty(
              exportsVarRef,
              js.StringLiteral(exportName),
              List(
                  "get" -> js.Function(arrow = false, Nil, None, {
                    js.Return(globalVar(VarField.t, varScope))
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
        WithGlobals(js.Apply(globalVar(VarField.s, (className, mainMethodName)), Nil))

      case MainMethodWithArgs(className, mainMethodName, args) =>
        val stringArrayTypeRef = ArrayTypeRef(ClassRef(BoxedStringClass), 1)
        val argsArrayWithGlobals =
          genArrayValue(stringArrayTypeRef, args.map(js.StringLiteral(_)))
        for (argsArray <- argsArrayWithGlobals) yield {
          js.Apply(globalVar(VarField.s, (className, mainMethodName)), argsArray :: Nil)
        }
    }
  }

}

private[emitter] object ClassEmitter {
  private val StaticInitializerOriginalName: OriginalName =
    OriginalName("<stinit>")

  private val ClassInitializerOriginalName: OriginalName =
    OriginalName("<clinit>")

  def shouldExtendJSError(className: ClassName, superClass: Option[ClassIdent]): Boolean =
    className == ThrowableClass && superClass.exists(_.name == ObjectClass)
}
