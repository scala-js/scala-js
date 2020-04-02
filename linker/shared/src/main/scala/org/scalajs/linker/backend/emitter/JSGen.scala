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

import scala.annotation.tailrec

import org.scalajs.ir._
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
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
private[emitter] final class JSGen(val semantics: Semantics,
    val esFeatures: ESFeatures, val moduleKind: ModuleKind,
    val nameGen: NameGen,
    internalOptions: InternalOptions,
    mentionedDangerousGlobalRefs: Set[String]) {

  import nameGen._

  val useClasses = esFeatures.useECMAScript2015

  val useArrowFunctions = esFeatures.useECMAScript2015

  val useBigIntForLongs = esFeatures.allowBigIntsForLongs

  val trackAllGlobalRefs = internalOptions.trackAllGlobalRefs

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

  def genJSPrivateSelect(receiver: Tree, className: ClassName,
      field: irt.FieldIdent)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): WithInfo[Tree] = {
    for {
      ident <- genJSPrivateFieldIdent(className, field)(globalKnowledge, field.pos)
    } yield BracketSelect(receiver, ident)
  }

  def genJSPrivateFieldIdent(className: ClassName, field: irt.FieldIdent)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): WithInfo[Tree] = {
    envVar("r", className, field.name)
  }

  def genIsInstanceOf(expr: Tree, tpe: Type)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): WithInfo[Tree] = {
    import TreeDSL._

    implicit def withInfo(t: Tree): WithInfo[Tree] = WithInfo(t)

    tpe match {
      case ClassType(className) =>
        if (HijackedClasses.contains(className)) {
          genIsInstanceOfHijackedClass(expr, className)
        } else if (className == ObjectClass) {
          expr === Null()
        } else if (className != NumberClass && // the only non-object superclass of hijacked classes
            !globalKnowledge.isInterface(className)) {
          envVar("c", className).map(expr instanceof _)
        } else {
          envVar("is", className).map(Apply(_, List(expr)))
        }

      case ArrayType(ArrayTypeRef(base, depth)) =>
        envVar("isArrayOf", base).map(Apply(_, List(expr, IntLiteral(depth))))

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

    if (useBigIntForLongs) {
      genCallHelper("isLong", expr)
    } else {
      /* HACK: RuntimeLong is not always local but is always available as it is
       * always in the root module.
       */
      expr instanceof localEnvVar("c", LongImpl.RuntimeLongClass)
    }
  }

  private def genIsFloat(expr: Tree)(implicit pos: Position): Tree = {
    import TreeDSL._

    if (semantics.strictFloats) genCallHelper("isFloat", expr)
    else typeof(expr) === "number"
  }

  def genAsInstanceOf(expr: Tree, tpe: Type)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): WithInfo[Tree] = {
    import TreeDSL._

    if (semantics.asInstanceOfs == CheckedBehavior.Unchecked) {
      val tree = tpe match {
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

      WithInfo(tree)
    } else {
      tpe match {
        case ClassType(className) =>
          envVar("as", className).map(Apply(_, List(expr)))

        case ArrayType(ArrayTypeRef(base, depth)) =>
          envVar("asArrayOf", base).map(Apply(_, List(expr, IntLiteral(depth))))

        case UndefType   => WithInfo(genCallHelper("uV", expr))
        case BooleanType => WithInfo(genCallHelper("uZ", expr))
        case CharType    => WithInfo(genCallHelper("uC", expr))
        case ByteType    => WithInfo(genCallHelper("uB", expr))
        case ShortType   => WithInfo(genCallHelper("uS", expr))
        case IntType     => WithInfo(genCallHelper("uI", expr))
        case LongType    => WithInfo(genCallHelper("uJ", expr))
        case FloatType   => WithInfo(genCallHelper("uF", expr))
        case DoubleType  => WithInfo(genCallHelper("uD", expr))
        case StringType  => WithInfo(genCallHelper("uT", expr))
        case AnyType     => WithInfo(expr)

        case NoType | NullType | NothingType | _:RecordType =>
          throw new AssertionError(s"Unexpected type $tpe in genAsInstanceOf")
      }
    }
  }

  def genCallHelper(helperName: String, args: Tree*)(
      implicit pos: Position): Tree = {
    Apply(coreJSLibVar(helperName), args.toList)
  }

  def genLoadModule(moduleClass: ClassName)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): WithInfo[Tree] = {
    envVar("m", moduleClass).map(Apply(_, Nil))
  }

  def genScalaClassNew(className: ClassName, ctor: MethodName, args: Tree*)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): WithInfo[Tree] = {
    val encodedClassVar = envVar("c", className)
    val argsList = args.toList
    if (globalKnowledge.hasInlineableInit(className)) {
      encodedClassVar.map(New(_, argsList))
    } else {
      for {
        classVar <- encodedClassVar
        ctor <- envVar("ct", className, ctor)
      } yield {
        Apply(ctor, New(classVar, Nil) :: argsList)
      }
    }
  }

  def genJSClassConstructor(className: ClassName,
      keepOnlyDangerousVarNames: Boolean)(
      implicit globalKnowledge: GlobalKnowledge,
      pos: Position): WithInfo[Tree] = {

    genJSClassConstructor(className,
        globalKnowledge.getJSNativeLoadSpec(className),
        keepOnlyDangerousVarNames)
  }

  def genJSClassConstructor(className: ClassName,
      spec: Option[irt.JSNativeLoadSpec],
      keepOnlyDangerousVarNames: Boolean)(
      implicit globalKnowledge: GlobalKnowledge,
      pos: Position): WithInfo[Tree] = {
    spec match {
      case None =>
        // This is a non-native JS class
        genNonNativeJSClassConstructor(className)

      case Some(spec) =>
        genLoadJSFromSpec(spec, keepOnlyDangerousVarNames)
    }
  }

  def genNonNativeJSClassConstructor(className: ClassName)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): WithInfo[Tree] = {
    envVar("a", className).map(Apply(_, Nil))
  }

  def genLoadJSFromSpec(spec: irt.JSNativeLoadSpec,
      keepOnlyDangerousVarNames: Boolean)(
      implicit pos: Position): WithInfo[Tree] = {

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
        WithInfo(pathSelection(globalVarRef, path), globalVarNames, Set.empty)

      case irt.JSNativeLoadSpec.Import(module, path) =>
        val moduleValue = fileLevelModuleField(module)
        path match {
          case "default" :: rest if moduleKind == ModuleKind.CommonJSModule =>
            val defaultField = genCallHelper("moduleDefault", moduleValue)
            WithInfo(pathSelection(defaultField, rest))
          case _ =>
            WithInfo(pathSelection(moduleValue, path))
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
      implicit globalKnowledge: GlobalKnowledge, pos: Position): WithInfo[Tree] = {
    genClassDataOf(arrayTypeRef).map(data =>
        genCallHelper("makeNativeArrayWrapper", data, ArrayConstr(elems)))
  }

  def genClassOf(typeRef: TypeRef)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): WithInfo[Tree] = {
    genClassDataOf(typeRef).map(data =>
        Apply(DotSelect(data, Ident("getClassOf")), Nil))
  }

  def genClassOf(className: ClassName)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): WithInfo[Tree] = {
    genClassOf(ClassRef(className))
  }

  def genClassDataOf(typeRef: TypeRef)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): WithInfo[Tree] = {
    typeRef match {
      case typeRef: NonArrayTypeRef =>
        envVar("d", typeRef)
      case ArrayTypeRef(base, dims) =>
        for (baseData <- genClassDataOf(base)) yield {
          (1 to dims).foldLeft[Tree](baseData) { (prev, _) =>
            Apply(DotSelect(prev, Ident("getArrayOf")), Nil)
          }
        }
    }
  }

  def genClassDataOf(className: ClassName)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): WithInfo[Tree] = {
    genClassDataOf(ClassRef(className))
  }

  // Compiler generated identifiers --------------------------------------------

  /* We distinguish three types of compiler generated identifiers:
   *
   * - envVar: Things pertaining to a given class. Accessible from every chunk
   *   of the Scala.js output by importing its module.
   * - coreJSLibVar: Things defined by the CoreJSLib. Accessible from every
   *   chunk by importing the root module.
   * - fileLevelVar: Things that are local to an individual file.
   */

  // Per class envVars.

  def localEnvVarIdent(field: String, className: ClassName)(implicit pos: Position): Ident =
    fileLevelVarIdent(field, genName(className))

  def localEnvVar(field: String, className: ClassName)(implicit pos: Position): Tree =
    VarRef(localEnvVarIdent(field, className))

  def envVar(field: String, className: ClassName)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): WithInfo[Tree] = {
    withInfoForClass(VarRef(localEnvVarIdent(field, className)), className)
  }

  // Per field envVars.

  def localEnvVarIdent(field: String, className: ClassName, fieldName: FieldName,
      origName: OriginalName)(implicit pos: Position): Ident = {
    fileLevelVarIdent(field, genName(className) + "__" + genName(fieldName),
        origName)
  }

  def envVar(field: String, className: ClassName, fieldName: FieldName)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): WithInfo[Tree] = {
    envVar(field, className, fieldName, NoOriginalName)
  }

  def envVar(field: String, className: ClassName, fieldName: FieldName,
      origName: OriginalName)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): WithInfo[Tree] = {
    withInfoForClass(VarRef(localEnvVarIdent(field, className, fieldName, origName)), className)
  }

  // Per method envVars.

  def localEnvVarIdent(field: String, className: ClassName, methodName: MethodName,
      origName: OriginalName)(
      implicit pos: Position): Ident = {
    fileLevelVarIdent(field, genName(className) + "__" + genName(methodName),
        origName)
  }

  def envVar(field: String, className: ClassName, methodName: MethodName)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): WithInfo[Tree] = {
    envVar(field, className, methodName, NoOriginalName)
  }

  def envVar(field: String, className: ClassName, methodName: MethodName,
      origName: OriginalName)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): WithInfo[Tree] = {
    withInfoForClass(VarRef(localEnvVarIdent(field, className, methodName, origName)), className)
  }

  // Per typeRef access.

  def envVar(field: String, typeRef: NonArrayTypeRef)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): WithInfo[Tree] = {
    typeRef match {
      case primRef: PrimRef =>
        WithInfo(VarRef(coreJSLibVarIdent(field, primRef)))

      case ClassRef(className) =>
        envVar(field, className)
    }
  }

  def coreJSLibVar(field: String)(implicit pos: Position): Tree =
    fileLevelVar(field)

  def coreJSLibVarIdent(field: String)(implicit pos: Position): Ident =
    fileLevelVarIdent(field)

  def coreJSLibVarIdent(field: String, primRef: PrimRef)(
      implicit pos: Position): Ident = {
    // The mapping in this function is an implementation detail of the emitter
    val subField = primRef.tpe match {
      case NoType      => "V"
      case BooleanType => "Z"
      case CharType    => "C"
      case ByteType    => "B"
      case ShortType   => "S"
      case IntType     => "I"
      case LongType    => "J"
      case FloatType   => "F"
      case DoubleType  => "D"
      case NullType    => "N"
      case NothingType => "E"
    }
    fileLevelVarIdent(field, subField)
  }

  def fileLevelModuleField(module: String)(implicit pos: Position): VarRef = {
    /* This is written so that the happy path, when `module` contains only
     * valid characters, is fast.
     */

    def isValidChar(c: Char): Boolean =
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

    def containsOnlyValidChars(): Boolean = {
      // scalastyle:off return
      val len = module.length
      var i = 0
      while (i != len) {
        if (!isValidChar(module.charAt(i)))
          return false
        i += 1
      }
      true
      // scalastyle:on return
    }

    def buildValidName(): String = {
      val result = new java.lang.StringBuilder("")
      val len = module.length
      var i = 0
      while (i != len) {
        val c = module.charAt(i)
        if (isValidChar(c))
          result.append(c)
        else
          result.append("$%04x".format(c.toInt))
        i += 1
      }
      result.toString()
    }

    val subField =
      if (containsOnlyValidChars()) module
      else buildValidName()

    fileLevelVar("i", subField, OriginalName(module))
  }

  def fileLevelVar(field: String, subField: String,
      origName: OriginalName = NoOriginalName)(
      implicit pos: Position): VarRef = {
    VarRef(fileLevelVarIdent(field, subField, origName))
  }

  def fileLevelVar(field: String)(implicit pos: Position): VarRef =
    VarRef(fileLevelVarIdent(field))

  def fileLevelVarIdent(field: String, subField: String,
      origName: OriginalName = NoOriginalName)(
      implicit pos: Position): Ident = {
    Ident(avoidClashWithGlobalRef("$" + field + "_" + subField), origName)
  }

  def fileLevelVarIdent(field: String)(implicit pos: Position): Ident =
    fileLevelVarIdent(field, NoOriginalName)

  def fileLevelVarIdent(field: String, origName: OriginalName)(
      implicit pos: Position): Ident = {
    Ident(avoidClashWithGlobalRef("$" + field), origName)
  }

  private def avoidClashWithGlobalRef(codegenVarName: String): String = {
    /* This is not cached because it should virtually never happen.
     * slowPath() is only called if we use a dangerous global ref, which should
     * already be very rare. And if do a second iteration in the loop only if
     * we refer to the global variables `$foo` *and* `$$foo`. At this point the
     * likelihood is so close to 0 that caching would be more expensive than
     * not caching.
     */
    @tailrec
    def slowPath(lastNameTried: String): String = {
      val nextNameToTry = "$" + lastNameTried
      if (mentionedDangerousGlobalRefs.contains(nextNameToTry))
        slowPath(nextNameToTry)
      else
        nextNameToTry
    }

    /* Hopefully this is JIT'ed away as `false` because
     * `mentionedDangerousGlobalRefs` is in fact `Set.EmptySet`.
     */
    if (mentionedDangerousGlobalRefs.contains(codegenVarName))
      slowPath(codegenVarName)
    else
      codegenVarName
  }

  private def withInfoForClass[A](v: A, className: ClassName)(
      implicit globalKnowledge: GlobalKnowledge): WithInfo[A] = {
    val module = globalKnowledge.getContainingModuleName(className).toSet
    WithInfo(v, Set.empty, module)
  }

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
      case StringLiteral(name) if internalOptions.optimizeBracketSelects &&
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
    if (internalOptions.optimizeBracketSelects)
      DotSelect(qual, Ident(item))
    else
      BracketSelect(qual, StringLiteral(item))
  }

  def genArrowFunction(args: List[ParamDef], body: Tree)(
      implicit pos: Position): Function = {
    Function(useArrowFunctions, args, body)
  }
}
