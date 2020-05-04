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

import scala.language.implicitConversions

import org.scalajs.ir._
import org.scalajs.ir.Names._
import org.scalajs.ir.Types._
import org.scalajs.ir.{Trees => irt}

import org.scalajs.linker.interface._
import org.scalajs.linker.backend.javascript.Trees._

import EmitterNames._

/** Collection of tree generators that are used across the board.
 *  This class is fully stateless.
 *
 *  Also carries around config (semantics and esFeatures).
 */
private[emitter] final class JSGen(
    val config: Emitter.Config,
    val nameGen: NameGen,
    val varGen: VarGen
) {

  import config._
  import nameGen._
  import varGen._

  val useClasses = esFeatures.useECMAScript2015

  val useArrowFunctions = esFeatures.useECMAScript2015

  val useBigIntForLongs = esFeatures.allowBigIntsForLongs

  def genZeroOf(tpe: Type)(implicit pos: Position): Tree = {
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
      case _           => Null()
    }
  }

  def genLongZero()(implicit pos: Position): Tree = {
    if (useBigIntForLongs)
      BigIntLiteral(0L)
    else
      coreJSLibVar("L0")
  }

  def genBoxedZeroOf(tpe: Type)(implicit pos: Position): Tree =
    if (tpe == CharType) genBoxedCharZero()
    else genZeroOf(tpe)

  def genBoxedCharZero()(implicit pos: Position): Tree =
    coreJSLibVar("bC0")

  def genLongModuleApply(methodName: MethodName, args: Tree*)(
      implicit pos: Position): Tree = {
    import TreeDSL._
    Apply(
        genLoadModule(LongImpl.RuntimeLongModuleClass) DOT genName(methodName),
        args.toList)
  }

  def genConst(name: Ident, rhs: Tree)(implicit pos: Position): LocalDef =
    genLet(name, mutable = false, rhs)

  def genLet(name: Ident, mutable: Boolean, rhs: Tree)(
      implicit pos: Position): LocalDef = {
    if (esFeatures.useECMAScript2015)
      Let(name, mutable, Some(rhs))
    else
      VarDef(name, Some(rhs))
  }

  def genEmptyMutableLet(name: Ident)(implicit pos: Position): LocalDef =
    genEmptyLet(name, mutable = true)

  def genEmptyImmutableLet(name: Ident)(implicit pos: Position): LocalDef =
    genEmptyLet(name, mutable = false)

  private def genEmptyLet(name: Ident, mutable: Boolean)(
      implicit pos: Position): LocalDef = {
    if (esFeatures.useECMAScript2015)
      Let(name, mutable, rhs = None)
    else
      VarDef(name, rhs = None)
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

  def genSelectStatic(className: ClassName, item: irt.FieldIdent)(
      implicit pos: Position): Tree = {
    classVar("t", className, item.name)
  }

  def genJSPrivateSelect(receiver: Tree, className: ClassName,
      field: irt.FieldIdent)(
      implicit pos: Position): Tree = {
    BracketSelect(receiver,
        genJSPrivateFieldIdent(className, field)(field.pos))
  }

  def genJSPrivateFieldIdent(className: ClassName, field: irt.FieldIdent)(
      implicit pos: Position): Tree = {
    classVar("r", className, field.name)
  }

  def genIsInstanceOf(expr: Tree, tpe: Type)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): Tree = {
    import TreeDSL._

    tpe match {
      case ClassType(className) =>
        if (HijackedClasses.contains(className)) {
          genIsInstanceOfHijackedClass(expr, className)
        } else if (className == ObjectClass) {
          expr === Null()
        } else if (className != NumberClass && // the only non-object superclass of hijacked classes
            !globalKnowledge.isInterface(className)) {
          expr instanceof classVar("c", className)
        } else {
          Apply(classVar("is", className), List(expr))
        }

      case ArrayType(ArrayTypeRef(base, depth)) =>
        Apply(typeRefVar("isArrayOf", base), List(expr, IntLiteral(depth)))

      case UndefType   => expr === Undefined()
      case BooleanType => typeof(expr) === "boolean"
      case CharType    => expr instanceof coreJSLibVar("Char")
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

  def genIsInstanceOfHijackedClass(expr: Tree, className: ClassName)(
      implicit pos: Position): Tree = {
    import TreeDSL._

    className match {
      case BoxedUnitClass      => expr === Undefined()
      case BoxedBooleanClass   => typeof(expr) === "boolean"
      case BoxedCharacterClass => expr instanceof coreJSLibVar("Char")
      case BoxedByteClass      => genCallHelper("isByte", expr)
      case BoxedShortClass     => genCallHelper("isShort", expr)
      case BoxedIntegerClass   => genCallHelper("isInt", expr)
      case BoxedLongClass      => genIsLong(expr)
      case BoxedFloatClass     => genIsFloat(expr)
      case BoxedDoubleClass    => typeof(expr) === "number"
      case BoxedStringClass    => typeof(expr) === "string"
    }
  }

  private def genIsLong(expr: Tree)(implicit pos: Position): Tree = {
    import TreeDSL._

    if (useBigIntForLongs) genCallHelper("isLong", expr)
    else expr instanceof classVar("c", LongImpl.RuntimeLongClass)
  }

  private def genIsFloat(expr: Tree)(implicit pos: Position): Tree = {
    import TreeDSL._

    if (semantics.strictFloats) genCallHelper("isFloat", expr)
    else typeof(expr) === "number"
  }

  def genAsInstanceOf(expr: Tree, tpe: Type)(implicit pos: Position): Tree = {
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
        case ClassType(className) =>
          Apply(classVar("as", className), List(expr))

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
      implicit pos: Position): Tree = {
    Apply(coreJSLibVar(helperName), args.toList)
  }

  def genLoadModule(moduleClass: ClassName)(implicit pos: Position): Tree = {
    import TreeDSL._
    Apply(classVar("m", moduleClass), Nil)
  }

  def genScalaClassNew(className: ClassName, ctor: MethodName, args: Tree*)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): Tree = {
    val encodedClassVar = classVar("c", className)
    val argsList = args.toList
    if (globalKnowledge.hasInlineableInit(className)) {
      New(encodedClassVar, argsList)
    } else {
      Apply(classVar("ct", className, ctor), New(encodedClassVar, Nil) :: argsList)
    }
  }

  def genJSClassConstructor(className: ClassName,
      keepOnlyDangerousVarNames: Boolean)(
      implicit globalKnowledge: GlobalKnowledge,
      pos: Position): WithGlobals[Tree] = {

    genJSClassConstructor(className,
        globalKnowledge.getJSNativeLoadSpec(className),
        keepOnlyDangerousVarNames)
  }

  def genJSClassConstructor(className: ClassName,
      spec: Option[irt.JSNativeLoadSpec],
      keepOnlyDangerousVarNames: Boolean)(
      implicit pos: Position): WithGlobals[Tree] = {
    spec match {
      case None =>
        // This is a non-native JS class
        WithGlobals(genNonNativeJSClassConstructor(className))

      case Some(spec) =>
        genLoadJSFromSpec(spec, keepOnlyDangerousVarNames)
    }
  }

  def genNonNativeJSClassConstructor(className: ClassName)(
      implicit pos: Position): Tree = {
    Apply(classVar("a", className), Nil)
  }

  def genLoadJSFromSpec(spec: irt.JSNativeLoadSpec,
      keepOnlyDangerousVarNames: Boolean)(
      implicit pos: Position): WithGlobals[Tree] = {

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
        val moduleValue = VarRef(envModuleFieldIdent(module))
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

  def genArrayValue(arrayTypeRef: ArrayTypeRef, elems: List[Tree])(
      implicit pos: Position): Tree = {
    genCallHelper("makeNativeArrayWrapper", genClassDataOf(arrayTypeRef),
        ArrayConstr(elems))
  }

  def genClassOf(typeRef: TypeRef)(implicit pos: Position): Tree =
    Apply(DotSelect(genClassDataOf(typeRef), Ident("getClassOf")), Nil)

  def genClassOf(className: ClassName)(implicit pos: Position): Tree =
    genClassOf(ClassRef(className))

  def genClassDataOf(typeRef: TypeRef)(implicit pos: Position): Tree = {
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

  def genClassDataOf(className: ClassName)(implicit pos: Position): Tree =
    genClassDataOf(ClassRef(className))

  def envModuleFieldIdent(module: String)(implicit pos: Position): Ident =
    fileLevelVarIdent("i", genModuleName(module), OriginalName(module))

  def genPropSelect(qual: Tree, item: PropertyName)(
      implicit pos: Position): Tree = {
    item match {
      case item: Ident         => DotSelect(qual, item)
      case item: StringLiteral => genBracketSelect(qual, item)
      case ComputedName(tree)  => genBracketSelect(qual, tree)
    }
  }

  def genBracketSelect(qual: Tree, item: Tree)(implicit pos: Position): Tree = {
    item match {
      case StringLiteral(name) if optimizeBracketSelects &&
          Ident.isValidJSIdentifierName(name) && name != "eval" =>
        /* We exclude "eval" because we do not want to rely too much on the
         * strict mode peculiarities of eval(), so that we can keep running
         * on VMs that do not support strict mode.
         */
        DotSelect(qual, Ident(name))
      case _ =>
        BracketSelect(qual, item)
    }
  }

  def genIdentBracketSelect(qual: Tree, item: String)(
      implicit pos: Position): Tree = {
    require(item != "eval")
    if (optimizeBracketSelects)
      DotSelect(qual, Ident(item))
    else
      BracketSelect(qual, StringLiteral(item))
  }

  def genArrowFunction(args: List[ParamDef], body: Tree)(
      implicit pos: Position): Function = {
    Function(useArrowFunctions, args, body)
  }
}
