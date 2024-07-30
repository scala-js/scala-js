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
import org.scalajs.ir.Types._
import org.scalajs.ir.{Trees => irt}

import org.scalajs.linker.backend.javascript.Trees._
import org.scalajs.linker.interface._

import EmitterNames._
import PolyfillableBuiltin._

/** Scala.js specific tree generators that are used across the board.
 *
 *  This class is fully stateless.
 *
 *  Also carries around lower-level generators.
 */
private[emitter] final class SJSGen(
    val jsGen: JSGen,
    val nameGen: NameGen,
    val varGen: VarGen,
    val nameCompressor: Option[NameCompressor]
) {

  import jsGen._
  import config._
  import coreSpec._
  import nameGen._
  import varGen._

  val useBigIntForLongs = esFeatures.allowBigIntsForLongs

  /** Core Property Names. */
  object cpn {
    // --- Scala.js objects ---

    /** The class-wide classData field of Scala.js objects, which references their TypeData. */
    val classData = "$classData" // always in full; it is used as identification of Scala.js objects

    // --- Class ---

    /** `Char.c`: the int value of the character. */
    val c = "c"

    // --- TypeData private fields ---

    /** `TypeData.constr`: the run-time constructor of the class. */
    val constr = if (minify) "C" else "constr"

    /** `TypeData.parentData`: the super class data. */
    val parentData = if (minify) "P" else "parentData"

    /** `TypeData.ancestors`: dictionary where keys are the ancestor names of all ancestors. */
    val ancestors = if (minify) "n" else "ancestors"

    /** `TypeData.componentData`: the `TypeData` of the component type of an array type. */
    val componentData = if (minify) "O" else "componentData"

    /** `TypeData.arrayBase`: the `TypeData` of the base type of an array type. */
    val arrayBase = if (minify) "B" else "arrayBase"

    /** `TypeData.arrayDepth`: the depth of an array type. */
    val arrayDepth = if (minify) "D" else "arrayDepth"

    /** `TypeData.zero`: the zero value of the type. */
    val zero = if (minify) "z" else "zero"

    /** `TypeData.arrayEncodedName`: the name of the type as it appears in its array type's name. */
    val arrayEncodedName = if (minify) "E" else "arrayEncodedName"

    /** `TypeData._classOf`: the field storing the `jl.Class` instance for that type. */
    val _classOf = if (minify) "L" else "_classOf"

    /** `TypeData._arrayOf`: the field storing the `TypeData` for that type's array type. */
    val _arrayOf = if (minify) "A" else "_arrayOf"

    /** `TypeData.isAssignableFromFun`: the implementation of `jl.Class.isAssignableFrom` without fast path. */
    val isAssignableFromFun = if (minify) "F" else "isAssignableFromFun"

    /** `TypeData.wrapArray`: the function to create an ArrayClass instance from a JS array of its elements. */
    val wrapArray = if (minify) "w" else "wrapArray"

    /** `TypeData.isJSType`: whether it is a JS type. */
    val isJSType = if (minify) "J" else "isJSType"

    // --- TypeData constructors ---

    val initPrim = if (minify) "p" else "initPrim"

    val initClass = if (minify) "i" else "initClass"

    val initSpecializedArray = if (minify) "y" else "initSpecializedArray"

    val initArray = if (minify) "a" else "initArray"

    // --- TypeData private methods ---

    /** `TypeData.getArrayOf()`: the `Type` instance for that type's array type. */
    val getArrayOf = if (minify) "r" else "getArrayOf"

    /** `TypeData.getClassOf()`: the `jl.Class` instance for that type. */
    val getClassOf = if (minify) "l" else "getClassOf"

    // --- TypeData public fields --- never minified

    /** `TypeData.name`: public, the user name of the class (the result of `jl.Class.getName()`). */
    val name = "name"

    /** `TypeData.isPrimitive`: public, whether it is a primitive type. */
    val isPrimitive = "isPrimitive"

    /** `TypeData.isInterface`: public, whether it is an interface type. */
    val isInterface = "isInterface"

    /** `TypeData.isArrayClass`: public, whether it is an array type. */
    val isArrayClass = "isArrayClass"

    /** `TypeData.isInstance()`: public, implementation of `jl.Class.isInstance`. */
    val isInstance = "isInstance"

    /** `TypeData.isAssignableFrom()`: public, implementation of `jl.Class.isAssignableFrom`. */
    val isAssignableFrom = "isAssignableFrom"

    // --- TypeData public methods --- never minified

    val checkCast = "checkCast"

    val getSuperclass = "getSuperclass"

    val getComponentType = "getComponentType"

    val newArrayOfThisClass = "newArrayOfThisClass"
  }

  /* This is a `val` because it is used at the top of every file, outside of
   * any cache. Fortunately it does not depend on any dynamic content.
   */
  val declarePrototypeVar: List[Tree] = {
    implicit val pos = Position.NoPosition
    if (minify) VarDef(fileLevelVarIdent(VarField.p), None) :: Nil
    else Nil
  }

  def prototypeFor(classRef: Tree)(implicit pos: Position): Tree = {
    import TreeDSL._
    if (minify) fileLevelVar(VarField.p)
    else classRef.prototype
  }

  def genAssignPrototype(classRef: Tree, value: Tree, localDecl: Boolean = false)(implicit pos: Position): Tree = {
    import TreeDSL._
    val assign = classRef.prototype := value
    if (!minify)
      assign
    else if (localDecl)
      VarDef(fileLevelVarIdent(VarField.p), Some(assign))
    else
      fileLevelVar(VarField.p) := assign
  }

  /** Under `minify`, set `$p` to `classRef.prototype`. */
  def setPrototypeVar(classRef: Tree)(implicit pos: Position): List[Tree] = {
    import TreeDSL._
    if (minify) (fileLevelVar(VarField.p) := classRef.prototype) :: Nil
    else Nil
  }

  def genZeroOf(tpe: Type)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    tpe match {
      case tpe: PrimType => genZeroOfPrim(tpe)
      case _             => Null()
    }
  }

  def genZeroOf(typeRef: TypeRef)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    typeRef match {
      case PrimRef(tpe) => genZeroOfPrim(tpe)
      case _            => Null()
    }
  }

  private def genZeroOfPrim(tpe: PrimType)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    tpe match {
      case BooleanType => BooleanLiteral(false)
      case CharType    => IntLiteral(0)
      case ByteType    => IntLiteral(0)
      case ShortType   => IntLiteral(0)
      case IntType     => IntLiteral(0)
      case LongType    => genLongZero()
      case FloatType   => DoubleLiteral(0.0)
      case DoubleType  => DoubleLiteral(0.0)
      case StringType  => StringLiteral("")
      case UndefType   => Undefined()
      case NullType    => Null()

      case NoType | NothingType =>
        throw new IllegalArgumentException(s"cannot generate a zero for $tpe")
    }
  }

  def genLongZero()(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    if (useBigIntForLongs)
      BigIntLiteral(0L)
    else
      globalVar(VarField.L0, CoreVar)
  }

  def genBoxedZeroOf(tpe: Type)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    if (tpe == CharType) genBoxedCharZero()
    else genZeroOf(tpe)
  }

  def genBoxedCharZero()(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    globalVar(VarField.bC0, CoreVar)
  }

  def genLongModuleApply(methodName: MethodName, args: Tree*)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    import TreeDSL._
    genApply(
        genLoadModule(LongImpl.RuntimeLongModuleClass), methodName,
        args.toList)
  }

  def usesUnderlyingTypedArray(elemTypeRef: NonArrayTypeRef): Boolean = {
    /* We are only interested in whether `getArrayUnderlyingTypedArrayClassRef`
     * returns a `Some` or not. We do not keep the result, so the `Position`
     * and the `GlobalRefTracking` are irrelevant.
     */
    implicit val dontCareGlobalRefTracking = GlobalRefTracking.Dangerous
    implicit val dontCarePosition = Position.NoPosition
    getArrayUnderlyingTypedArrayClassRef(elemTypeRef).nonEmpty
  }

  def getArrayUnderlyingTypedArrayClassRef(elemTypeRef: NonArrayTypeRef)(
      implicit tracking: GlobalRefTracking, pos: Position): Option[WithGlobals[VarRef]] = {
    elemTypeRef match {
      case _ if esFeatures.esVersion < ESVersion.ES2015 => None
      case primRef: PrimRef                             => typedArrayRef(primRef)
      case _                                            => None
    }
  }

  def typedArrayRef(primRef: PrimRef)(
      implicit tracking: GlobalRefTracking, pos: Position): Option[WithGlobals[VarRef]] = {
    def some(name: String) = Some(globalRef(name))

    primRef match {
      case CharRef   => some("Uint16Array")
      case ByteRef   => some("Int8Array")
      case ShortRef  => some("Int16Array")
      case IntRef    => some("Int32Array")
      case FloatRef  => some("Float32Array")
      case DoubleRef => some("Float64Array")

      case LongRef if useBigIntForLongs => some("BigInt64Array")

      case _ => None
    }
  }

  def genSelect(receiver: Tree, field: irt.FieldIdent)(
      implicit pos: Position): Tree = {
    DotSelect(receiver, genFieldIdent(field.name)(field.pos))
  }

  def genSelectForDef(receiver: Tree, field: irt.FieldIdent,
      originalName: OriginalName)(
      implicit pos: Position): Tree = {
    DotSelect(receiver, genFieldIdentForDef(field.name, originalName)(field.pos))
  }

  private def genFieldIdent(fieldName: FieldName)(
      implicit pos: Position): MaybeDelayedIdent = {
    nameCompressor match {
      case None =>
        Ident(genName(fieldName))
      case Some(compressor) =>
        DelayedIdent(compressor.genResolverFor(fieldName))
    }
  }

  private def genFieldIdentForDef(fieldName: FieldName,
      originalName: OriginalName)(
      implicit pos: Position): MaybeDelayedIdent = {
    nameCompressor match {
      case None =>
        val jsName = genName(fieldName)
        val jsOrigName = genOriginalName(fieldName, originalName, jsName)
        Ident(jsName, jsOrigName)
      case Some(compressor) =>
        DelayedIdent(compressor.genResolverFor(fieldName), originalName.orElse(fieldName))
    }
  }

  def genApply(receiver: Tree, methodName: MethodName, args: List[Tree])(
      implicit pos: Position): Tree = {
    Apply(DotSelect(receiver, genMethodIdent(methodName)), args)
  }

  def genApply(receiver: Tree, methodName: MethodName, args: Tree*)(
      implicit pos: Position): Tree = {
    genApply(receiver, methodName, args.toList)
  }

  def genMethodIdent(methodIdent: irt.MethodIdent): MaybeDelayedIdent =
    genMethodIdent(methodIdent.name)(methodIdent.pos)

  def genMethodIdentForDef(methodIdent: irt.MethodIdent,
      originalName: OriginalName): MaybeDelayedIdent = {
    genMethodIdentForDef(methodIdent.name, originalName)(methodIdent.pos)
  }

  def genMethodIdent(methodName: MethodName)(implicit pos: Position): MaybeDelayedIdent = {
    nameCompressor match {
      case None             => Ident(genName(methodName))
      case Some(compressor) => DelayedIdent(compressor.genResolverFor(methodName))
    }
  }

  def genMethodIdentForDef(methodName: MethodName, originalName: OriginalName)(
      implicit pos: Position): MaybeDelayedIdent = {
    nameCompressor match {
      case None =>
        val jsName = genName(methodName)
        val jsOrigName = genOriginalName(methodName, originalName, jsName)
        Ident(jsName, jsOrigName)
      case Some(compressor) =>
        DelayedIdent(compressor.genResolverFor(methodName), originalName.orElse(methodName))
    }
  }

  def genArrayClassPropApply(receiver: Tree, prop: ArrayClassProperty, args: Tree*)(
      implicit pos: Position): Tree = {
    genArrayClassPropApply(receiver, prop, args.toList)
  }

  def genArrayClassPropApply(receiver: Tree, prop: ArrayClassProperty, args: List[Tree])(
      implicit pos: Position): Tree = {
    Apply(genArrayClassPropSelect(receiver, prop), args)
  }

  def genArrayClassPropSelect(qualifier: Tree, prop: ArrayClassProperty)(
      implicit pos: Position): Tree = {
    DotSelect(qualifier, genArrayClassProperty(prop))
  }

  def genArrayClassProperty(prop: ArrayClassProperty)(implicit pos: Position): MaybeDelayedIdent = {
    nameCompressor match {
      case None             => Ident(prop.nonMinifiedName)
      case Some(compressor) => DelayedIdent(compressor.genResolverFor(prop))
    }
  }

  def genArrayClassPropertyForDef(prop: ArrayClassProperty)(implicit pos: Position): MaybeDelayedIdent = {
    nameCompressor match {
      case None             => Ident(prop.nonMinifiedName)
      case Some(compressor) => DelayedIdent(compressor.genResolverFor(prop), prop.originalName)
    }
  }

  def genAncestorIdent(ancestor: ClassName)(implicit pos: Position): MaybeDelayedIdent = {
    nameCompressor match {
      case None             => Ident(genName(ancestor))
      case Some(compressor) => DelayedIdent(compressor.genResolverForAncestor(ancestor))
    }
  }

  def genJSPrivateSelect(receiver: Tree, field: irt.FieldIdent)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    val fieldName = {
      implicit val pos = field.pos
      globalVar(VarField.r, field.name)
    }

    BracketSelect(receiver, fieldName)
  }

  def genIsInstanceOf(expr: Tree, tpe: Type)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    import TreeDSL._

    tpe match {
      case ClassType(className) =>
        if (HijackedClasses.contains(className)) {
          genIsInstanceOfHijackedClass(expr, className)
        } else if (className == ObjectClass) {
          expr === Null()
        } else if (!globalKnowledge.isAncestorOfHijackedClass(className) &&
            !globalKnowledge.isInterface(className)) {
          genIsInstanceOfClass(expr, className)
        } else {
          Apply(globalVar(VarField.is, className), List(expr))
        }

      case ArrayType(arrayTypeRef) =>
        arrayTypeRef match {
          case ArrayTypeRef(_:PrimRef | ClassRef(ObjectClass), 1) =>
            expr instanceof genArrayConstrOf(arrayTypeRef)
          case ArrayTypeRef(base, depth) =>
            Apply(typeRefVar(VarField.isArrayOf, base), List(expr, IntLiteral(depth)))
        }

      case UndefType   => expr === Undefined()
      case BooleanType => typeof(expr) === "boolean"
      case CharType    => expr instanceof globalVar(VarField.Char, CoreVar)
      case ByteType    => genCallHelper(VarField.isByte, expr)
      case ShortType   => genCallHelper(VarField.isShort, expr)
      case IntType     => genCallHelper(VarField.isInt, expr)
      case LongType    => genIsLong(expr)
      case FloatType   => genIsFloat(expr)
      case DoubleType  => typeof(expr) === "number"
      case StringType  => typeof(expr) === "string"
      case AnyType     => expr !== Null()

      case NoType | NullType | NothingType | _:RecordType =>
        throw new AssertionError(s"Unexpected type $tpe in genIsInstanceOf")
    }
  }

  def genIsInstanceOfClass(expr: Tree, className: ClassName)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    import TreeDSL._

    if (!globalKnowledge.hasInstances(className)) {
      /* We need to constant-fold the instance test, to avoid emitting
       * `x instanceof $c_TheClass`, because `$c_TheClass` won't be
       * declared at all. Otherwise, we'd get a `ReferenceError`.
       */
      BooleanLiteral(false)
    } else {
      expr instanceof globalVar(VarField.c, className)
    }
  }

  def genIsInstanceOfHijackedClass(expr: Tree, className: ClassName)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    import TreeDSL._

    className match {
      case BoxedUnitClass      => expr === Undefined()
      case BoxedBooleanClass   => typeof(expr) === "boolean"
      case BoxedCharacterClass => expr instanceof globalVar(VarField.Char, CoreVar)
      case BoxedByteClass      => genCallHelper(VarField.isByte, expr)
      case BoxedShortClass     => genCallHelper(VarField.isShort, expr)
      case BoxedIntegerClass   => genCallHelper(VarField.isInt, expr)
      case BoxedLongClass      => genIsLong(expr)
      case BoxedFloatClass     => genIsFloat(expr)
      case BoxedDoubleClass    => typeof(expr) === "number"
      case BoxedStringClass    => typeof(expr) === "string"
    }
  }

  private def genIsLong(expr: Tree)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    import TreeDSL._

    if (useBigIntForLongs) genCallHelper(VarField.isLong, expr)
    else expr instanceof globalVar(VarField.c, LongImpl.RuntimeLongClass)
  }

  private def genIsFloat(expr: Tree)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    import TreeDSL._

    if (semantics.strictFloats) genCallHelper(VarField.isFloat, expr)
    else typeof(expr) === "number"
  }

  def genAsInstanceOf(expr: Tree, tpe: Type)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      tracking: GlobalRefTracking, pos: Position): WithGlobals[Tree] = {
    import TreeDSL._

    // Local short-hand of WithGlobals(...)
    def wg(tree: Tree): WithGlobals[Tree] = WithGlobals(tree)

    if (semantics.asInstanceOfs == CheckedBehavior.Unchecked) {
      tpe match {
        case _:ClassType | _:ArrayType | AnyType =>
          wg(expr)

        case UndefType                     => wg(Block(expr, Undefined()))
        case BooleanType                   => wg(!(!expr))
        case CharType                      => wg(genCallHelper(VarField.uC, expr))
        case ByteType | ShortType| IntType => wg(expr | 0)
        case LongType                      => wg(genCallHelper(VarField.uJ, expr))
        case DoubleType                    => wg(UnaryOp(irt.JSUnaryOp.+, expr))
        case StringType                    => wg(expr || StringLiteral(""))

        case FloatType =>
          if (semantics.strictFloats) genCallPolyfillableBuiltin(FroundBuiltin, expr)
          else wg(UnaryOp(irt.JSUnaryOp.+, expr))

        case NoType | NullType | NothingType | _:RecordType =>
          throw new AssertionError(s"Unexpected type $tpe in genAsInstanceOf")
      }
    } else {
      val resultTree = tpe match {
        case ClassType(ObjectClass) =>
          expr
        case ClassType(className) =>
          Apply(globalVar(VarField.as, className), List(expr))

        case ArrayType(ArrayTypeRef(base, depth)) =>
          Apply(typeRefVar(VarField.asArrayOf, base), List(expr, IntLiteral(depth)))

        case UndefType   => genCallHelper(VarField.uV, expr)
        case BooleanType => genCallHelper(VarField.uZ, expr)
        case CharType    => genCallHelper(VarField.uC, expr)
        case ByteType    => genCallHelper(VarField.uB, expr)
        case ShortType   => genCallHelper(VarField.uS, expr)
        case IntType     => genCallHelper(VarField.uI, expr)
        case LongType    => genCallHelper(VarField.uJ, expr)
        case FloatType   => genCallHelper(VarField.uF, expr)
        case DoubleType  => genCallHelper(VarField.uD, expr)
        case StringType  => genCallHelper(VarField.uT, expr)
        case AnyType     => expr

        case NoType | NullType | NothingType | _:RecordType =>
          throw new AssertionError(s"Unexpected type $tpe in genAsInstanceOf")
      }

      wg(resultTree)
    }
  }

  /** Orders a subset of hijacked classes by priority for a series of type
   *  tests.
   *
   *  If `j.l.Double` is in the list, then run-time subclasses of `Double` are
   *  excluded (i.e., `Byte`, `Short`, `Integer` and `Float`).
   *
   *  If we do not use bigints to implement Longs, `j.l.Long` is excluded.
   *
   *  The result is ordered in an "efficient" way, putting `typeof`-based tests
   *  first when possible, and otherwise ordering by a gut-feeling of
   *  "likelihood".
   */
  def subsetOfHijackedClassesOrderedForTypeTests(
      hijackedClasses: Set[ClassName]): List[ClassName] = {
    val baseList = {
      if (hijackedClasses.contains(BoxedDoubleClass))
        nonSmallNumberHijackedClassesOrderedForTypeTests
      else
        allHijackedClassesOrderedForTypeTests
    }

    baseList.filter(hijackedClasses)
  }

  /** List of hijacked classes ordered by priority for a series of type tests,
   *  excluding run-time subclasses of Double.
   *
   *  Those with `typeof`-based tests come first because they are cheaper.
   */
  private val nonSmallNumberHijackedClassesOrderedForTypeTests = List(
    BoxedStringClass,
    BoxedDoubleClass,
    BoxedBooleanClass,
    BoxedUnitClass,
    BoxedLongClass,
    BoxedCharacterClass
  )

  /** List of all the hijacked classes ordered by priority for a series of type
   *  tests.
   */
  private val allHijackedClassesOrderedForTypeTests = List(
    BoxedByteClass,
    BoxedShortClass,
    BoxedIntegerClass,
    BoxedFloatClass
  ) ::: nonSmallNumberHijackedClassesOrderedForTypeTests

  def genCallHelper(helperName: VarField, args: Tree*)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    Apply(globalVar(helperName, CoreVar), args.toList)
  }

  def genCallPolyfillableBuiltin(builtin: PolyfillableBuiltin, args: Tree*)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      tracking: GlobalRefTracking, pos: Position): WithGlobals[Tree] = {
    if (esFeatures.esVersion >= builtin.availableInESVersion) {
      builtin match {
        case builtin: GlobalVarBuiltin =>
          for (global <- globalRef(builtin.globalVar)) yield
            Apply(global, args.toList)
        case builtin: NamespacedBuiltin =>
          for (namespace <- globalRef(builtin.namespaceGlobalVar)) yield
            Apply(genIdentBracketSelect(namespace, builtin.builtinName), args.toList)
      }
    } else {
      WithGlobals(genCallHelper(builtin.polyfillField, args: _*))
    }
  }

  def genLoadModule(moduleClass: ClassName)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    import TreeDSL._
    Apply(globalVar(VarField.m, moduleClass), Nil)
  }

  def genScalaClassNew(className: ClassName, ctor: MethodName, args: Tree*)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    val encodedClassVar = globalVar(VarField.c, className)
    val argsList = args.toList
    if (globalKnowledge.hasInlineableInit(className)) {
      New(encodedClassVar, argsList)
    } else {
      Apply(globalVar(VarField.ct, (className, ctor)), New(encodedClassVar, Nil) :: argsList)
    }
  }

  def genJSClassConstructor(className: ClassName)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      tracking: GlobalRefTracking, pos: Position): WithGlobals[Tree] = {

    genJSClassConstructor(className,
        globalKnowledge.getJSNativeLoadSpec(className))
  }

  def genJSClassConstructor(className: ClassName,
      spec: Option[irt.JSNativeLoadSpec])(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      tracking: GlobalRefTracking, pos: Position): WithGlobals[Tree] = {
    spec match {
      case None =>
        // This is a non-native JS class
        WithGlobals(genNonNativeJSClassConstructor(className))

      case Some(spec) =>
        genLoadJSFromSpec(spec)
    }
  }

  def genNonNativeJSClassConstructor(className: ClassName)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    Apply(globalVar(VarField.a, className), Nil)
  }

  def genLoadJSFromSpec(spec: irt.JSNativeLoadSpec)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      tracking: GlobalRefTracking, pos: Position): WithGlobals[Tree] = {

    def pathSelection(from: Tree, path: List[String]): Tree = {
      path.foldLeft(from) {
        (prev, part) => genBracketSelect(prev, StringLiteral(part))
      }
    }

    spec match {
      case irt.JSNativeLoadSpec.Global(globalRefName, path) =>
        for (globalVarRef <- globalRef(globalRefName)) yield
          pathSelection(globalVarRef, path)

      case irt.JSNativeLoadSpec.Import(module, path) =>
        val moduleValue = VarRef(externalModuleFieldIdent(module))
        path match {
          case "default" :: rest if moduleKind == ModuleKind.CommonJSModule =>
            val defaultField = genCallHelper(VarField.moduleDefault, moduleValue)
            WithGlobals(pathSelection(defaultField, rest))
          case _ =>
            WithGlobals(pathSelection(moduleValue, path))
        }

      case irt.JSNativeLoadSpec.ImportWithGlobalFallback(importSpec, globalSpec) =>
        moduleKind match {
          case ModuleKind.NoModule =>
            genLoadJSFromSpec(globalSpec)
          case ModuleKind.ESModule | ModuleKind.CommonJSModule =>
            genLoadJSFromSpec(importSpec)
        }
    }
  }

  def genNewArray(arrayTypeRef: ArrayTypeRef, lengths: List[Tree])(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    lengths match {
      case Nil =>
        throw new IllegalArgumentException(
            "Cannot create a new array with 0 dimensions")
      case length :: Nil =>
        New(genArrayConstrOf(arrayTypeRef), length :: Nil)
      case _ =>
        genCallHelper(VarField.newArrayObject, genClassDataOf(arrayTypeRef),
            ArrayConstr(lengths))
    }
  }

  def genArrayValue(arrayTypeRef: ArrayTypeRef, elems: List[Tree])(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      tracking: GlobalRefTracking, pos: Position): WithGlobals[Tree] = {
    genNativeArrayWrapper(arrayTypeRef, ArrayConstr(elems))
  }

  def genNativeArrayWrapper(arrayTypeRef: ArrayTypeRef, nativeArray: Tree)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      tracking: GlobalRefTracking, pos: Position): WithGlobals[Tree] = {
    val argWithGlobals = arrayTypeRef match {
      case ArrayTypeRef(elemTypeRef, 1) =>
        getArrayUnderlyingTypedArrayClassRef(elemTypeRef) match {
          case Some(typedArrayWithGlobals) =>
            for (typedArray <- typedArrayWithGlobals) yield
              New(typedArray, nativeArray :: Nil)
          case _ =>
            WithGlobals(nativeArray)
        }
      case _ =>
        WithGlobals(nativeArray)
    }

    for (arg <- argWithGlobals) yield
      New(genArrayConstrOf(arrayTypeRef), arg :: Nil)
  }

  def genArrayConstrOf(arrayTypeRef: ArrayTypeRef)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    import TreeDSL._

    arrayTypeRef match {
      case ArrayTypeRef(primRef: PrimRef, 1) =>
        globalVar(VarField.ac, primRef)
      case ArrayTypeRef(ClassRef(ObjectClass), 1) =>
        globalVar(VarField.ac, ObjectClass)
      case _ =>
        genClassDataOf(arrayTypeRef) DOT cpn.constr
    }
  }

  def genClassOf(typeRef: TypeRef)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    Apply(DotSelect(genClassDataOf(typeRef), Ident(cpn.getClassOf)), Nil)
  }

  def genClassOf(className: ClassName)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    genClassOf(ClassRef(className))
  }

  def genClassDataOf(typeRef: TypeRef)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    typeRef match {
      case typeRef: NonArrayTypeRef =>
        typeRefVar(VarField.d, typeRef)

      case ArrayTypeRef(base, dims) =>
        val baseData = genClassDataOf(base)
        (1 to dims).foldLeft[Tree](baseData) { (prev, _) =>
          Apply(DotSelect(prev, Ident(cpn.getArrayOf)), Nil)
        }
    }
  }

  def genClassDataOf(className: ClassName)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    genClassDataOf(ClassRef(className))
  }

  def genCheckNotNull(obj: Tree)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    if (semantics.nullPointers == CheckedBehavior.Unchecked)
      obj
    else
      genCallHelper(VarField.n, obj)
  }
}
