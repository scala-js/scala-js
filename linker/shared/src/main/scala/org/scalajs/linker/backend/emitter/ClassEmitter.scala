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
            js.VarDef(fileLevelVar("superClass").ident, Some(rhs))
          }
        }

        for {
          optStoreJSSuperClass <- WithGlobals.option(genStoreJSSuperClass)
          entireClassDef <- entireClassDefWithGlobals
          createStaticFields <- genCreateStaticFieldsOfJSClass(tree)
        } yield {
          tree.jsClassCaptures.fold {
            val classValueIdent = fileLevelVarIdent("b", genName(className))

            val createClassValueVar = genEmptyMutableLet(classValueIdent)

            val createAccessor = {
              val classValueVar = js.VarRef(classValueIdent)

              val body = js.Block(
                  js.If(!classValueVar, {
                    js.Block(
                        optStoreJSSuperClass.toList :::
                        entireClassDef ::
                        createStaticFields :::
                        (classValueVar := classVar("c", className)) ::
                        genClassInitialization(tree)
                    )
                  }, {
                    js.Skip()
                  }),
                  js.Return(classValueVar)
              )

              classFunctionDef("a", className, Nil, body)
            }

            js.Block(createClassValueVar, createAccessor)
          } { jsClassCaptures =>
            val captureParamDefs = for (param <- jsClassCaptures) yield {
              implicit val pos = param.pos
              val ident = fileLevelVarIdent("cc", genName(param.name.name),
                  param.originalName.orElse(param.name.name))
              js.ParamDef(ident, rest = false)
            }

            assert(!hasClassInitializer(tree),
                s"Found a class initializer in the non-top-level class $className")

            val body = js.Block(
                optStoreJSSuperClass.toList :::
                entireClassDef ::
                createStaticFields :::
                js.Return(classVar("c", className)) ::
                Nil
            )

            classFunctionDef("a", className, captureParamDefs, body)
          }
        }
      }
    }
  }

  /** Generates an ECMAScript 6 class for a linked class. */
  def genES6Class(tree: LinkedClass, members: List[js.Tree])(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {

    require(useClasses)

    val parentVarWithGlobals = for (parentIdent <- tree.superClass) yield {
      implicit val pos = parentIdent.pos
      if (!tree.kind.isJSClass) {
        if (shouldExtendJSError(tree))
          globalRef("Error")
        else
          WithGlobals(classVar("c", parentIdent.name))
      } else if (tree.jsSuperClass.isDefined) {
        WithGlobals(fileLevelVar("superClass"))
      } else {
        genJSClassConstructor(parentIdent.name,
            keepOnlyDangerousVarNames = true)
      }
    }

    for (parentVar <- WithGlobals.option(parentVarWithGlobals))
      yield classClassDef("c", tree.name.name, parentVar, members)(tree.pos)
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
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {

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
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    import TreeDSL._
    implicit val pos = tree.pos

    val className = tree.name.name
    val isJSClass = tree.kind.isJSClass
    val typeVar = classVar("c", className)

    def makeInheritableCtorDef(ctorToMimic: js.Tree, field: String) = {
      js.Block(
        js.DocComment("@constructor"),
        if (isJSClass) classVarDef(field, className, js.Function(false, Nil, js.Skip()))
        else classFunctionDef(field, className, Nil, js.Skip()),
        classVar(field, className).prototype := ctorToMimic.prototype
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
          val inheritableCtorDefWithGlobals = globalRef("Error").map { errorRef =>
            makeInheritableCtorDef(errorRef, "hh")
          }
          (inheritableCtorDefWithGlobals, classVar("hh", className))
        } else {
          (WithGlobals(js.Skip()), classVar("h", parentIdent.name))
        }
      } else {
        val superCtor = if (tree.jsSuperClass.isDefined) {
          WithGlobals(fileLevelVar("superClass"))
        } else {
          genJSClassConstructor(parentIdent.name,
              keepOnlyDangerousVarNames = true)
        }
        (superCtor.map(makeInheritableCtorDef(_, "h")), classVar("h", className))
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
      val ctorDef =
        if (isJSClass) classVarDef("c", className, ctorFun)
        else classFunctionDef("c", className, ctorFun.args, ctorFun.body)

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
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Function] = {
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
      implicit globalKnowledge: GlobalKnowledge): List[js.Tree] = {
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
      implicit globalKnowledge: GlobalKnowledge): List[js.Tree] = {
    for {
      field @ FieldDef(flags, FieldIdent(name), origName, ftpe) <- tree.fields
      if flags.namespace.isStatic
    } yield {
      implicit val pos = field.pos
      classVarDef("t", tree.className, name, genZeroOf(ftpe),
          origName.orElse(name), flags.isMutable)
    }
  }

  /** Generates the creation of the private JS field defs for a JavaScript
   *  class.
   */
  def genCreatePrivateJSFieldDefsOfJSClass(tree: LinkedClass): List[js.Tree] = {
    for {
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

      classVarDef("r", tree.className, name, symbolValue,
          origName.orElse(name), mutable = false)
    }
  }

  /** Generates the creation of the static fields for a JavaScript class. */
  private def genCreateStaticFieldsOfJSClass(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[List[js.Tree]] = {
    val className = tree.className
    val statsWithGlobals = for {
      field <- tree.fields
      if field.flags.namespace.isStatic
    } yield {
      implicit val pos = field.pos
      val classVarRef = classVar("c", className)
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
  def genStaticInitialization(tree: LinkedClass): List[js.Tree] = {
    implicit val pos = tree.pos
    val hasStaticInit = tree.methods.exists { m =>
      m.value.flags.namespace == MemberNamespace.StaticConstructor &&
      m.value.methodName.isStaticInitializer
    }
    if (hasStaticInit) {
      val field = classVar("sct", tree.className, StaticInitializerName,
          StaticInitializerOriginalName)
      js.Apply(field, Nil) :: Nil
    } else {
      Nil
    }
  }

  /** Generates the class initializer invocation of a class. */
  private def genClassInitialization(tree: LinkedClass): List[js.Tree] = {
    implicit val pos = tree.pos
    if (hasClassInitializer(tree)) {
      val field = classVar("sct", tree.className, ClassInitializerName,
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

  /** Generates a method. */
  def genMethod(className: ClassName, method: MethodDef)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
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

    methodFun0WithGlobals.map { methodFun0 =>
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

      val methodName = method.name
      val originalName = method.originalName

      if (namespace != MemberNamespace.Public) {
        val field = namespace match {
          case MemberNamespace.Private           => "p"
          case MemberNamespace.PublicStatic      => "s"
          case MemberNamespace.PrivateStatic     => "ps"
          case MemberNamespace.Constructor       => "ct"
          case MemberNamespace.StaticConstructor => "sct"
        }
        classVarDef(field, className, methodName.name, methodFun,
            originalName.orElse(methodName.name))
      } else {
        val jsMethodName = genMemberMethodIdent(methodName, originalName)
        if (useClasses) {
          js.MethodDef(static = false, jsMethodName,
              methodFun.args, methodFun.body)
        } else {
          genAddToPrototype(className, jsMethodName, methodFun)
        }
      }
    }
  }

  /** Generates a JS method. */
  def genJSMethod(className: ClassName, method: JSMethodDef)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    implicit val pos = method.pos

    val namespace = method.flags.namespace
    assert(!namespace.isPrivate && !namespace.isConstructor)

    val methodFunWithGlobals =
      desugarToFunction(className, method.args, method.body, AnyType)

    methodFunWithGlobals.flatMap { methodFun =>
      val methodName = method.name

      if (useClasses) {
        for (propName <- genMemberNameTree(methodName)) yield {
          js.MethodDef(static = namespace.isStatic, propName, methodFun.args,
              methodFun.body)
        }
      } else {
        if (namespace.isStatic)
          genAddToObject(classVar("c", className), methodName, methodFun)
        else
          genAddToPrototype(className, method.name, methodFun)
      }
    }
  }

  /** Generates a default method. */
  def genDefaultMethod(className: ClassName, method: MethodDef)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    implicit val pos = method.pos

    val methodFunWithGlobals = desugarToFunctionWithExplicitThis(
        className, method.args, method.body.get, method.resultType)

    for (methodFun <- methodFunWithGlobals) yield {
      val methodName = method.name.name
      classFunctionDef("f", className, methodName, methodFun.args, methodFun.body,
          method.originalName.orElse(methodName))
    }
  }

  /** Generates an instance method of a hijacked class. */
  def genHijackedMethod(className: ClassName, method: MethodDef)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    // We abuse `genDefaultMethod` as it does everything the way we want
    genDefaultMethod(className, method)
  }

  /** Generates a property. */
  def genJSProperty(className: ClassName, property: JSPropertyDef)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    if (useClasses)
      genJSPropertyES6(className, property)
    else
      genJSPropertyES5(className, property)
  }

  private def genJSPropertyES5(className: ClassName, property: JSPropertyDef)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    import TreeDSL._
    implicit val pos = property.pos

    // class prototype
    val classVarRef = classVar("c", className)
    val targetObject =
      if (property.flags.namespace.isStatic) classVarRef
      else classVarRef.prototype

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
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
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

  /** Generate `classVar.prototype.name = value` */
  def genAddToPrototype(className: ClassName, name: js.PropertyName,
      value: js.Tree)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): js.Tree = {
    import TreeDSL._

    genAddToObject(classVar("c", className).prototype, name, value)
  }

  /** Generate `classVar.prototype[name] = value` */
  def genAddToPrototype(className: ClassName, name: Tree, value: js.Tree)(
      implicit globalKnowledge: GlobalKnowledge,
      pos: Position): WithGlobals[js.Tree] = {
    for (propName <- genMemberNameTree(name))
      yield genAddToPrototype(className, propName, value)
  }

  /** Generate `obj.name = value` */
  def genAddToObject(obj: js.Tree, name: js.PropertyName,
      value: js.Tree)(implicit pos: Position): js.Tree = {
    js.Assign(genPropSelect(obj, name), value)
  }

  /** Generate `obj.name = value` */
  def genAddToObject(obj: js.Tree, name: Tree, value: js.Tree)(
      implicit globalKnowledge: GlobalKnowledge,
      pos: Position): WithGlobals[js.Tree] = {
    for (propName <- genMemberNameTree(name))
      yield genAddToObject(obj, propName, value)
  }

  def genMemberNameTree(name: Tree)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.PropertyName] = {
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
      implicit globalKnowledge: GlobalKnowledge): js.Tree = {
    js.Block(genSingleInstanceTests(tree) ::: genArrayInstanceTests(tree))(tree.pos)
  }

  private def genSingleInstanceTests(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): List[js.Tree] = {
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
              test = test || (obj instanceof coreJSLibVar("Char"))

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

      val createIsStat = if (needIsFunction) {
        classFunctionDef("is", className, List(objParam), js.Return(isExpression))
      } else {
        js.Skip()
      }

      val createAsStat = if (semantics.asInstanceOfs == Unchecked) {
        js.Skip()
      } else {
        classFunctionDef("as", className, List(objParam), js.Return {
          className match {
            case ObjectClass =>
              obj

            case _ =>
              val isCond =
                if (needIsFunction) js.Apply(classVar("is", className), List(obj))
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

      List(createIsStat, createAsStat)
    } else {
      Nil
    }
  }

  private def genArrayInstanceTests(tree: LinkedClass): List[js.Tree] = {
    import TreeDSL._

    implicit val pos = tree.pos

    val className = tree.name.name
    val displayName = className.nameString

    val objParam = js.ParamDef(js.Ident("obj"), rest = false)
    val obj = objParam.ref

    val depthParam = js.ParamDef(js.Ident("depth"), rest = false)
    val depth = depthParam.ref

    val createIsArrayOfStat = {
      classFunctionDef("isArrayOf", className, List(objParam, depthParam), {
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

    val createAsArrayOfStat = if (semantics.asInstanceOfs == Unchecked) {
      js.Skip()
    } else {
      classFunctionDef("asArrayOf", className, List(objParam, depthParam), {
        js.Return {
          js.If(js.Apply(classVar("isArrayOf", className), List(obj, depth)) ||
              (obj === js.Null()), {
            obj
          }, {
            genCallHelper("throwArrayCastException",
                obj, js.StringLiteral("L"+displayName+";"), depth)
          })
        }
      })
    }

    List(createIsArrayOfStat, createAsArrayOfStat)
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
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
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
        classVar("d", parent.name)
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
        WithGlobals(classVar("is", className))
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
          WithGlobals(coreJSLibVar("noIsInstance"))
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
        classVar("isArrayOf", className)
      } else {
        // For other classes, the isArrayOf function can be inferred.
        js.Undefined()
      }
    }

    for (isInstanceFun <- isInstanceFunWithGlobals) yield {
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

      val typeData = js.Apply(js.New(coreJSLibVar("TypeData"), Nil) DOT "initClass",
          prunedParams)

      classVarDef("d", className, typeData)
    }
  }

  def genSetTypeData(tree: LinkedClass): js.Tree = {
    import TreeDSL._

    implicit val pos = tree.pos

    assert(tree.kind.isClass)

    classVar("c", tree.name.name).prototype DOT "$classData" :=
      classVar("d", tree.name.name)
  }

  def genModuleAccessor(tree: LinkedClass): js.Tree = {
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
            js.New(classVar("c", className), Nil)
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

      classFunctionDef("m", className, Nil, body)
    }

    js.Block(createModuleInstanceField, createAccessor)
  }

  def genExportedMembers(tree: LinkedClass)(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
    val exportsWithGlobals = tree.exportedMembers map { member =>
      member.value match {
        case JSMethodDef(flags, StringLiteral("constructor"), _, _)
            if flags.namespace == MemberNamespace.Public && tree.kind.isJSClass =>
          WithGlobals(js.Skip()(member.value.pos))
        case m: JSMethodDef =>
          genJSMethod(tree.className, m)
        case p: JSPropertyDef =>
          genJSProperty(tree.className, p)
      }
    }

    for (exports <- WithGlobals.list(exportsWithGlobals))
      yield js.Block(exports)(tree.pos)
  }

  def genTopLevelExports(topLevelExports: List[LinkedTopLevelExport])(
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[List[js.Tree]] = {
    val exportsWithGlobals = topLevelExports.map { topLevelExport =>
      implicit val pos = topLevelExport.tree.pos

      topLevelExport.tree match {
        case TopLevelJSClassExportDef(exportName) =>
          genConstValueExportDef(
              exportName, genNonNativeJSClassConstructor(topLevelExport.owningClass))
        case TopLevelModuleExportDef(exportName) =>
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
      implicit globalKnowledge: GlobalKnowledge): WithGlobals[js.Tree] = {
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
            genSelectStatic(className, field))

      case ModuleKind.ESModule =>
        // Hack: Use a classVarIdent even though this is a usage site.
        val staticVarIdent =
          classVarIdent("t", className, field.name, NoOriginalName)

        WithGlobals(
            js.Export((staticVarIdent -> js.ExportName(exportName)) :: Nil))

      case ModuleKind.CommonJSModule =>
        globalRef("exports").flatMap { exportsVarRef =>
          genDefineProperty(
              exportsVarRef,
              js.StringLiteral(exportName),
              List(
                  "get" -> js.Function(arrow = false, Nil, {
                    js.Return(genSelectStatic(className, field))
                  }),
                  "configurable" -> js.BooleanLiteral(true)
              )
          )
        }
    }
  }

  /** Gen JS code for an [[ModuleInitializer]]. */
  def genModuleInitializer(moduleInitializer: ModuleInitializer): js.Tree = {
    import ModuleInitializerImpl._

    implicit val pos = Position.NoPosition

    ModuleInitializerImpl.fromModuleInitializer(moduleInitializer) match {
      case VoidMainMethod(className, mainMethodName) =>
        js.Apply(classVar("s", className, mainMethodName), Nil)

      case MainMethodWithArgs(className, mainMethodName, args) =>
        val stringArrayTypeRef = ArrayTypeRef(ClassRef(BoxedStringClass), 1)
        js.Apply(classVar("s", className, mainMethodName),
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
