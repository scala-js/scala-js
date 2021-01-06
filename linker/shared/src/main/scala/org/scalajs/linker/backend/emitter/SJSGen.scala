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

/** Scala.js specific tree generators that are used across the board.
 *
 *  This class is fully stateless.
 *
 *  Also carries around lower-level generators.
 */
private[emitter] final class SJSGen(
    val jsGen: JSGen,
    val nameGen: NameGen,
    val varGen: VarGen
) {

  import jsGen._
  import config._
  import nameGen._
  import varGen._

  val useBigIntForLongs = esFeatures.allowBigIntsForLongs

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
      globalVar("L0", CoreVar)
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
    globalVar("bC0", CoreVar)
  }

  def genLongModuleApply(methodName: MethodName, args: Tree*)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    import TreeDSL._
    Apply(
        genLoadModule(LongImpl.RuntimeLongModuleClass) DOT genName(methodName),
        args.toList)
  }

  def usesUnderlyingTypedArray(elemTypeRef: NonArrayTypeRef): Boolean =
    getArrayUnderlyingTypedArrayClassRef(elemTypeRef)(Position.NoPosition).nonEmpty

  def getArrayUnderlyingTypedArrayClassRef(elemTypeRef: NonArrayTypeRef)(
      implicit pos: Position): Option[WithGlobals[VarRef]] = {
    elemTypeRef match {
      case _ if !esFeatures.useECMAScript2015 => None
      case primRef: PrimRef                   => typedArrayRef(primRef)
      case _                                  => None
    }
  }

  def typedArrayRef(primRef: PrimRef)(
      implicit pos: Position): Option[WithGlobals[VarRef]] = {
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

  def genSelect(receiver: Tree, className: ClassName, field: irt.FieldIdent)(
      implicit pos: Position): Tree = {
    DotSelect(receiver, Ident(genFieldJSName(className, field))(field.pos))
  }

  def genSelect(receiver: Tree, className: ClassName, field: irt.FieldIdent,
      originalName: OriginalName)(
      implicit pos: Position): Tree = {
    val jsName = genFieldJSName(className, field)
    val jsOrigName = genOriginalName(field.name, originalName, jsName)
    DotSelect(receiver, Ident(jsName, jsOrigName)(field.pos))
  }

  private def genFieldJSName(className: ClassName, field: irt.FieldIdent): String =
    genName(className) + "__f_" + genName(field.name)

  def genJSPrivateSelect(receiver: Tree, className: ClassName,
      field: irt.FieldIdent)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    val fieldName = {
      implicit val pos = field.pos
      globalVar("r", (className, field.name))
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
          Apply(globalVar("is", className), List(expr))
        }

      case ArrayType(arrayTypeRef) =>
        arrayTypeRef match {
          case ArrayTypeRef(_:PrimRef | ClassRef(ObjectClass), 1) =>
            expr instanceof genArrayConstrOf(arrayTypeRef)
          case ArrayTypeRef(base, depth) =>
            Apply(typeRefVar("isArrayOf", base), List(expr, IntLiteral(depth)))
        }

      case UndefType   => expr === Undefined()
      case BooleanType => typeof(expr) === "boolean"
      case CharType    => expr instanceof globalVar("Char", CoreVar)
      case ByteType    => genCallHelper("isByte", expr)
      case ShortType   => genCallHelper("isShort", expr)
      case IntType     => genCallHelper("isInt", expr)
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
      expr instanceof globalVar("c", className)
    }
  }

  def genIsInstanceOfHijackedClass(expr: Tree, className: ClassName)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    import TreeDSL._

    className match {
      case BoxedUnitClass      => expr === Undefined()
      case BoxedBooleanClass   => typeof(expr) === "boolean"
      case BoxedCharacterClass => expr instanceof globalVar("Char", CoreVar)
      case BoxedByteClass      => genCallHelper("isByte", expr)
      case BoxedShortClass     => genCallHelper("isShort", expr)
      case BoxedIntegerClass   => genCallHelper("isInt", expr)
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

    if (useBigIntForLongs) genCallHelper("isLong", expr)
    else expr instanceof globalVar("c", LongImpl.RuntimeLongClass)
  }

  private def genIsFloat(expr: Tree)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    import TreeDSL._

    if (semantics.strictFloats) genCallHelper("isFloat", expr)
    else typeof(expr) === "number"
  }

  def genAsInstanceOf(expr: Tree, tpe: Type)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    import TreeDSL._

    if (semantics.asInstanceOfs == CheckedBehavior.Unchecked) {
      tpe match {
        case _:ClassType | _:ArrayType | AnyType =>
          expr

        case UndefType                     => Block(expr, Undefined())
        case BooleanType                   => !(!expr)
        case CharType                      => genCallHelper("uC", expr)
        case ByteType | ShortType| IntType => expr | 0
        case LongType                      => genCallHelper("uJ", expr)
        case DoubleType                    => UnaryOp(irt.JSUnaryOp.+, expr)
        case StringType                    => expr || StringLiteral("")

        case FloatType =>
          if (semantics.strictFloats) genCallHelper("fround", expr)
          else UnaryOp(irt.JSUnaryOp.+, expr)

        case NoType | NullType | NothingType | _:RecordType =>
          throw new AssertionError(s"Unexpected type $tpe in genAsInstanceOf")
      }
    } else {
      tpe match {
        case ClassType(ObjectClass) =>
          expr
        case ClassType(className) =>
          Apply(globalVar("as", className), List(expr))

        case ArrayType(ArrayTypeRef(base, depth)) =>
          Apply(typeRefVar("asArrayOf", base), List(expr, IntLiteral(depth)))

        case UndefType   => genCallHelper("uV", expr)
        case BooleanType => genCallHelper("uZ", expr)
        case CharType    => genCallHelper("uC", expr)
        case ByteType    => genCallHelper("uB", expr)
        case ShortType   => genCallHelper("uS", expr)
        case IntType     => genCallHelper("uI", expr)
        case LongType    => genCallHelper("uJ", expr)
        case FloatType   => genCallHelper("uF", expr)
        case DoubleType  => genCallHelper("uD", expr)
        case StringType  => genCallHelper("uT", expr)
        case AnyType     => expr

        case NoType | NullType | NothingType | _:RecordType =>
          throw new AssertionError(s"Unexpected type $tpe in genAsInstanceOf")
      }
    }
  }

  def genCallHelper(helperName: String, args: Tree*)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    Apply(globalVar(helperName, CoreVar), args.toList)
  }

  def genLoadModule(moduleClass: ClassName)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    import TreeDSL._
    Apply(globalVar("m", moduleClass), Nil)
  }

  def genScalaClassNew(className: ClassName, ctor: MethodName, args: Tree*)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    val encodedClassVar = globalVar("c", className)
    val argsList = args.toList
    if (globalKnowledge.hasInlineableInit(className)) {
      New(encodedClassVar, argsList)
    } else {
      Apply(globalVar("ct", (className, ctor)), New(encodedClassVar, Nil) :: argsList)
    }
  }

  def genJSClassConstructor(className: ClassName,
      keepOnlyDangerousVarNames: Boolean)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): WithGlobals[Tree] = {

    genJSClassConstructor(className,
        globalKnowledge.getJSNativeLoadSpec(className),
        keepOnlyDangerousVarNames)
  }

  def genJSClassConstructor(className: ClassName,
      spec: Option[irt.JSNativeLoadSpec],
      keepOnlyDangerousVarNames: Boolean)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): WithGlobals[Tree] = {
    spec match {
      case None =>
        // This is a non-native JS class
        WithGlobals(genNonNativeJSClassConstructor(className))

      case Some(spec) =>
        genLoadJSFromSpec(spec, keepOnlyDangerousVarNames)
    }
  }

  def genNonNativeJSClassConstructor(className: ClassName)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    Apply(globalVar("a", className), Nil)
  }

  def genLoadJSFromSpec(spec: irt.JSNativeLoadSpec,
      keepOnlyDangerousVarNames: Boolean)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): WithGlobals[Tree] = {

    def pathSelection(from: Tree, path: List[String]): Tree = {
      path.foldLeft(from) {
        (prev, part) => genBracketSelect(prev, StringLiteral(part))
      }
    }

    spec match {
      case irt.JSNativeLoadSpec.Global(globalRef, path) =>
        val globalVarRef = VarRef(Ident(globalRef))
        val globalVarNames = {
          if (keepOnlyDangerousVarNames && !trackAllGlobalRefs &&
              !GlobalRefUtils.isDangerousGlobalRef(globalRef)) {
            Set.empty[String]
          } else {
            Set(globalRef)
          }
        }
        WithGlobals(pathSelection(globalVarRef, path), globalVarNames)

      case irt.JSNativeLoadSpec.Import(module, path) =>
        val moduleValue = VarRef(externalModuleFieldIdent(module))
        path match {
          case "default" :: rest if moduleKind == ModuleKind.CommonJSModule =>
            val defaultField = genCallHelper("moduleDefault", moduleValue)
            WithGlobals(pathSelection(defaultField, rest))
          case _ =>
            WithGlobals(pathSelection(moduleValue, path))
        }

      case irt.JSNativeLoadSpec.ImportWithGlobalFallback(importSpec, globalSpec) =>
        moduleKind match {
          case ModuleKind.NoModule =>
            genLoadJSFromSpec(globalSpec, keepOnlyDangerousVarNames)
          case ModuleKind.ESModule | ModuleKind.CommonJSModule =>
            genLoadJSFromSpec(importSpec, keepOnlyDangerousVarNames)
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
        genCallHelper("newArrayObject", genClassDataOf(arrayTypeRef),
            ArrayConstr(lengths))
    }
  }

  def genArrayValue(arrayTypeRef: ArrayTypeRef, elems: List[Tree])(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): WithGlobals[Tree] = {
    genNativeArrayWrapper(arrayTypeRef, ArrayConstr(elems))
  }

  def genNativeArrayWrapper(arrayTypeRef: ArrayTypeRef, nativeArray: Tree)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): WithGlobals[Tree] = {
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
        globalVar("ac", primRef)
      case ArrayTypeRef(ClassRef(ObjectClass), 1) =>
        globalVar("ac", ObjectClass)
      case _ =>
        genClassDataOf(arrayTypeRef) DOT "constr"
    }
  }

  def genClassOf(typeRef: TypeRef)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    Apply(DotSelect(genClassDataOf(typeRef), Ident("getClassOf")), Nil)
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
        typeRefVar("d", typeRef)

      case ArrayTypeRef(base, dims) =>
        val baseData = genClassDataOf(base)
        (1 to dims).foldLeft[Tree](baseData) { (prev, _) =>
          Apply(DotSelect(prev, Ident("getArrayOf")), Nil)
        }
    }
  }

  def genClassDataOf(className: ClassName)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge,
      pos: Position): Tree = {
    genClassDataOf(ClassRef(className))
  }
}
