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

  def buildClass(tree: LinkedClass, ctor: js.Tree, memberDefs: List[js.MethodDef],
      exportedDefs: js.Tree)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {

    implicit val pos = tree.pos

    val className = tree.name.name
    def allES6Defs = {
      js.Block(ctor +: memberDefs :+ exportedDefs)(tree.pos) match {
        case js.Block(allDefs) => allDefs
        case js.Skip()         => Nil
        case oneDef            => List(oneDef)
      }
    }

    def allES5Defs(classVar: js.Tree) = {
      WithGlobals(js.Block(
          ctor, assignES5ClassMembers(classVar, memberDefs), exportedDefs))
    }

    if (!tree.kind.isJSClass) {
      assert(tree.jsSuperClass.isEmpty, className)

      if (useClasses) {
        val parentVarWithGlobals = for (parentIdent <- tree.superClass) yield {
          implicit val pos = parentIdent.pos
          if (shouldExtendJSError(tree)) globalRef("Error")
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

      val entireClassDefWithGlobals = if (useClasses) {
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
          createAccessor <- globalFunctionDef("a", className, Nil, body)
        } yield {
          js.Block(createClassValueVar, createAccessor)
        }
      } { jsClassCaptures =>
        val captureParamDefs = for (param <- jsClassCaptures) yield {
          implicit val pos = param.pos
          val ident = fileLevelVarIdent("cc", genName(param.name.name),
              param.originalName.orElse(param.name.name))
          js.ParamDef(ident, rest = false)
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

          globalFunctionDef("a", className, captureParamDefs, body)
        }
      }
    }
  }

  /** Extracts the inlineable init method, if there is one. */
  def extractInlineableInit(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): (Option[Versioned[MethodDef]], List[Versioned[MethodDef]]) = {

    if (globalKnowledge.hasInlineableInit(tree.className)) {
      val (constructors, otherMethods) = tree.methods.partition { m =>
        m.value.flags.namespace == MemberNamespace.Constructor
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
  def genConstructor(tree: LinkedClass, initToInline: Option[MethodDef])(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {

    assert(tree.kind.isAnyNonNativeClass)
    assert(tree.superClass.isDefined || tree.name.name == ObjectClass,
        s"Class ${tree.name.name} is missing a parent class")

    if (useClasses)
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
          genConst(dummyCtor.ident, js.Function(false, Nil, js.Skip())),
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
          globalRef("Error").map(chainPrototypeWithLocalCtor(ctorVar, _))

        case Some(parentIdent) =>
          WithGlobals(ctorVar.prototype := js.New(globalVar("h", parentIdent.name), Nil))
      }

      for {
        ctorFun <- genJSConstructorFun(tree, initToInline)
        realCtorDef <-
            globalFunctionDef("c", className, ctorFun.args, ctorFun.body)
        inheritableCtorDef <-
            globalFunctionDef("h", className, Nil, js.Skip())
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
      initToInline: Option[MethodDef])(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Function] = {

    implicit val pos = tree.pos

    val superCtorCallAndFieldDefs = if (useClasses) {
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
          js.Function(arrow = false, Nil, js.Block(superCtorCallAndFieldDefs)))
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
        val js.Function(arrow, args, initMethodFunBody) = generatedInitMethodFun
        js.Function(arrow, args,
            js.Block(superCtorCallAndFieldDefs ::: initMethodFunBody :: Nil))
      }
    }
  }

  private def genConstructorFunForJSClass(tree: LinkedClass)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Function] = {
    implicit val pos = tree.pos

    require(tree.kind.isJSClass)

    tree.exportedMembers.map(_.value) collectFirst {
      case JSMethodDef(flags, StringLiteral("constructor"), params, body)
          if flags.namespace == MemberNamespace.Public =>
        desugarToFunction(tree.className, params, body, resultType = AnyType)
    } getOrElse {
      throw new IllegalArgumentException(
          s"${tree.className} does not have an exported constructor")
    }
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

      val symbolValue = {
        def description = origName.getOrElse(name).toString()
        val args =
          if (semantics.productionMode) Nil
          else js.StringLiteral(description) :: Nil

        if (esFeatures.useECMAScript2015)
          js.Apply(js.VarRef(js.Ident("Symbol")), args)
        else
          genCallHelper("privateJSFieldSymbol", args: _*)
      }

      globalVarDef("r", (tree.className, name), symbolValue,
          origName.orElse(name))
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

  /** Generates the static initializer invocation of a class. */
  def genStaticInitialization(tree: LinkedClass)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): List[js.Tree] = {
    implicit val pos = tree.pos
    val hasStaticInit = tree.methods.exists { m =>
      m.value.flags.namespace == MemberNamespace.StaticConstructor &&
      m.value.methodName.isStaticInitializer
    }
    if (hasStaticInit) {
      val field = globalVar("sct", (tree.className, StaticInitializerName),
          StaticInitializerOriginalName)
      js.Apply(field, Nil) :: Nil
    } else {
      Nil
    }
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
      m.value.flags.namespace == MemberNamespace.StaticConstructor &&
      m.value.methodName.isClassInitializer
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
      js.MethodDef(static = false, jsMethodName, methodFun.args, methodFun.body)
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
      if (namespace != MemberNamespace.Constructor &&
          namespace != MemberNamespace.Private) {
        desugarToFunction(className, method.args, methodBody, method.resultType)
      } else {
        desugarToFunctionWithExplicitThis(className, method.args, methodBody,
            method.resultType)
      }
    }

    methodFun0WithGlobals.flatMap { methodFun0 =>
      val methodFun = if (namespace == MemberNamespace.Constructor) {
        // init methods have to return `this` so that we can chain them to `new`
        js.Function(arrow = false, methodFun0.args, {
          implicit val pos = methodFun0.body.pos
          js.Block(
              methodFun0.body,
              js.Return(methodFun0.args.head.ref))
        })(methodFun0.pos)
      } else {
        methodFun0
      }

      val field = namespace match {
        case MemberNamespace.Private           => "p"
        case MemberNamespace.PublicStatic      => "s"
        case MemberNamespace.PrivateStatic     => "ps"
        case MemberNamespace.Constructor       => "ct"
        case MemberNamespace.StaticConstructor => "sct"

        case MemberNamespace.Public =>
          throw new AssertionError("not a static-like method")
      }

      val methodName = method.name.name

      globalVarDef(field, (className, methodName), methodFun,
          method.originalName.orElse(methodName))
    }
  }

  /** Generates a JS method. */
  private def genJSMethod(tree: LinkedClass, method: JSMethodDef)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    implicit val pos = method.pos

    val namespace = method.flags.namespace
    assert(!namespace.isPrivate && !namespace.isConstructor)

    for {
      methodFun <- desugarToFunction(tree.className, method.args, method.body, AnyType)
      propName <- genMemberNameTree(method.name)
    } yield {
      if (useClasses) {
        js.MethodDef(static = namespace.isStatic, propName, methodFun.args, methodFun.body)
      } else {
        val targetObject = exportTargetES5(tree, namespace)
        js.Assign(genPropSelect(targetObject, propName), methodFun)
      }
    }
  }

  /** Generates a default method. */
  def genDefaultMethod(className: ClassName, method: MethodDef)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    implicit val pos = method.pos

    val methodFunWithGlobals = desugarToFunctionWithExplicitThis(
        className, method.args, method.body.get, method.resultType)

    methodFunWithGlobals.flatMap { methodFun =>
      val methodName = method.name.name
      globalFunctionDef("f", (className, methodName), methodFun.args, methodFun.body,
          method.originalName.orElse(methodName))
    }
  }

  /** Generates an instance method of a hijacked class. */
  def genHijackedMethod(className: ClassName, method: MethodDef)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    // We abuse `genDefaultMethod` as it does everything the way we want
    genDefaultMethod(className, method)
  }

  /** Generates a property. */
  private def genJSProperty(tree: LinkedClass, property: JSPropertyDef)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    if (useClasses)
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
    name match {
      case StringLiteral(value) =>
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

  def needInstanceTests(tree: LinkedClass): Boolean = {
    tree.hasInstanceTests || {
      tree.hasRuntimeTypeInfo &&
      ClassesWhoseDataReferToTheirInstanceTests.contains(tree.className)
    }
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

    val isHijackedClass =
      tree.kind == ClassKind.HijackedClass

    if (tree.kind.isClass || tree.kind == ClassKind.Interface || isHijackedClass) {
      val className = tree.name.name
      val displayName = className.nameString

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

      val isExpression = {
        className match {
          case ObjectClass =>
            js.BinaryOp(JSBinaryOp.!==, obj, js.Null())

          case _ if isHijackedClass =>
            genIsInstanceOfHijackedClass(obj, className)

          case _ =>
            var test = if (tree.kind.isClass) {
              genIsInstanceOfClass(obj, className)
            } else {
              !(!(
                  genIsScalaJSObject(obj) &&
                  genIsClassNameInAncestors(className,
                      obj DOT "$classData" DOT "ancestors")
              ))
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
              test = test || (obj instanceof globalVar("Char", CoreVar))

            test
        }
      }

      val needIsFunction = !isHijackedClass && (isExpression match {
        case js.BinaryOp(JSBinaryOp.instanceof, _, _) =>
          // This is a simple `instanceof`. It will always be inlined at call site.
          false
        case _ =>
          true
      })

      val createIsStatWithGlobals = if (needIsFunction) {
        globalFunctionDef("is", className, List(objParam), js.Return(isExpression))
      } else {
        WithGlobals(js.Skip())
      }

      val createAsStatWithGlobals = if (semantics.asInstanceOfs == Unchecked) {
        WithGlobals(js.Skip())
      } else {
        globalFunctionDef("as", className, List(objParam), js.Return {
          className match {
            case ObjectClass =>
              obj

            case _ =>
              val isCond =
                if (needIsFunction) js.Apply(globalVar("is", className), List(obj))
                else isExpression

              js.If(isCond || (obj === js.Null()), {
                obj
              }, {
                genCallHelper("throwClassCastException",
                    obj, js.StringLiteral(displayName))
              })
          }
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

    val objParam = js.ParamDef(js.Ident("obj"), rest = false)
    val obj = objParam.ref

    val depthParam = js.ParamDef(js.Ident("depth"), rest = false)
    val depth = depthParam.ref

    val createIsArrayOfStatWithGlobals = {
      globalFunctionDef("isArrayOf", className, List(objParam, depthParam), {
        className match {
          case ObjectClass =>
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

    val createAsArrayOfStatWithGlobals = if (semantics.asInstanceOfs == Unchecked) {
      WithGlobals(js.Skip())
    } else {
      globalFunctionDef("asArrayOf", className, List(objParam, depthParam), {
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
        globalVar("d", parent.name)
      }
    } else {
      js.Undefined()
    }

    val ancestorsRecord = js.ObjectConstr(
        tree.ancestors.map(ancestor => (js.Ident(genName(ancestor)), js.IntLiteral(1))))

    val isInstanceFunWithGlobals: WithGlobals[js.Tree] = {
      if (isAncestorOfHijackedClass) {
        /* Ancestors of hijacked classes, including java.lang.Object, have a
         * normal $is_pack_Class test but with a non-standard behavior.
         */
        WithGlobals(globalVar("is", className))
      } else if (isHijackedClass) {
        /* Hijacked classes have a special isInstanceOf test. */
        val xParam = js.ParamDef(js.Ident("x"), rest = false)
        WithGlobals(genArrowFunction(List(xParam), js.Return {
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
        globalVar("isArrayOf", className)
      } else {
        // For other classes, the isArrayOf function can be inferred.
        js.Undefined()
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
          isInstanceFun,
          isArrayOfFun
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

      globalFunctionDef("m", className, Nil, body)
    }

    createAccessor.map(js.Block(createModuleInstanceField, _))
  }

  def genExportedMembers(tree: LinkedClass)(
      implicit moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    val exportsWithGlobals = tree.exportedMembers map { member =>
      member.value match {
        case JSMethodDef(flags, StringLiteral("constructor"), _, _)
            if flags.namespace == MemberNamespace.Public && tree.kind.isJSClass =>
          WithGlobals(js.Skip()(member.value.pos))
        case m: JSMethodDef =>
          genJSMethod(tree, m)
        case p: JSPropertyDef =>
          genJSProperty(tree, p)
      }
    }

    for (exports <- WithGlobals.list(exportsWithGlobals))
      yield js.Block(exports)(tree.pos)
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

    val JSMethodDef(flags, StringLiteral(exportName), args, body) =
      tree.methodDef

    assert(flags.namespace == MemberNamespace.PublicStatic, exportName)

    implicit val pos = tree.pos

    val methodDefWithGlobals = desugarToFunction(args, body, AnyType)

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
        val export = js.Export((field.ident -> js.ExportName(exportName)) :: Nil)
        WithGlobals(js.Block(let, export))

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
                  "get" -> js.Function(arrow = false, Nil, {
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
      globalKnowledge: GlobalKnowledge): js.Tree = {
    import ModuleInitializerImpl._

    implicit val pos = Position.NoPosition

    ModuleInitializerImpl.fromInitializer(initializer) match {
      case VoidMainMethod(className, mainMethodName) =>
        js.Apply(globalVar("s", (className, mainMethodName)), Nil)

      case MainMethodWithArgs(className, mainMethodName, args) =>
        val stringArrayTypeRef = ArrayTypeRef(ClassRef(BoxedStringClass), 1)
        js.Apply(globalVar("s", (className, mainMethodName)),
            genArrayValue(stringArrayTypeRef, args.map(js.StringLiteral(_))) :: Nil)
    }
  }

}

private[emitter] object ClassEmitter {
  private val StaticInitializerOriginalName: OriginalName =
    OriginalName("<stinit>")

  private val ClassInitializerOriginalName: OriginalName =
    OriginalName("<clinit>")

  private val ClassesWhoseDataReferToTheirInstanceTests =
    AncestorsOfHijackedClasses + BoxedStringClass

  def shouldExtendJSError(linkedClass: LinkedClass): Boolean = {
    linkedClass.name.name == ThrowableClass &&
    linkedClass.superClass.exists(_.name == ObjectClass)
  }
}
