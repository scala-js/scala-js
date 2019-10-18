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

import org.scalajs.ir
import ir._
import ir.Definitions._
import ir.Types._
import ir.{Trees => irt}

import org.scalajs.linker.interface._
import org.scalajs.linker.backend.javascript.Trees._

import EmitterDefinitions._

/** Collection of tree generators that are used accross the board.
 *  This class is fully stateless.
 *
 *  Also carries around config (semantics and esFeatures).
 */
private[emitter] final class JSGen(val semantics: Semantics,
    val esFeatures: ESFeatures, val moduleKind: ModuleKind,
    internalOptions: InternalOptions,
    mentionedDangerousGlobalRefs: Set[String]) {

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
      envField("L0")
  }

  def genLongModuleApply(methodName: MethodName, args: Tree*)(
      implicit pos: Position): Tree = {
    import TreeDSL._
    Apply(
        genLoadModule(LongImpl.RuntimeLongModuleClass) DOT methodName,
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

  def genSelect(receiver: Tree, cls: ClassRef, field: irt.FieldIdent)(
      implicit pos: Position): Tree = {
    genSelect(receiver, cls.className, field)
  }

  def genSelect(receiver: Tree, cls: ClassName, field: irt.FieldIdent)(
      implicit pos: Position): Tree = {
    DotSelect(receiver, genFieldIdent(cls, field)(field.pos))
  }

  private def genFieldIdent(cls: ClassName, field: irt.FieldIdent)(
      implicit pos: Position): Ident = {
    Ident(cls + "__f_" + field.name, field.originalName)
  }

  def genSelectStatic(className: ClassName, item: irt.FieldIdent)(
      implicit pos: Position): VarRef = {
    envField("t", className + "__" + item.name)
  }

  /* The similarity with `genSelect` is accidental. It will probably evolve in
   * the future, to emit selections of symbols that are private to the
   * Scala.js-generated code.
   */
  def genJSPrivateSelect(receiver: Tree, cls: ClassRef, field: irt.FieldIdent)(
      implicit pos: Position): Tree = {
    DotSelect(receiver, genJSPrivateFieldIdent(cls.className, field)(field.pos))
  }

  // The similarity with `genFieldIdent is accidental. See above.
  def genJSPrivateFieldIdent(cls: ClassName, field: irt.FieldIdent)(
      implicit pos: Position): Ident = {
    Ident(cls + "__f_" + field.name, field.originalName)
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
          expr instanceof encodeClassVar(className)
        } else {
          Apply(envField("is", className), List(expr))
        }

      case ArrayType(ArrayTypeRef(base, depth)) =>
        Apply(envField("isArrayOf", arrayBaseFieldName(base)),
            List(expr, IntLiteral(depth)))

      case UndefType   => expr === Undefined()
      case BooleanType => typeof(expr) === "boolean"
      case CharType    => expr instanceof envField("Char")
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
      case BoxedCharacterClass => expr instanceof envField("Char")
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
    else expr instanceof encodeClassVar(LongImpl.RuntimeLongClass)
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
          Apply(envField("as", className), List(expr))

        case ArrayType(ArrayTypeRef(base, depth)) =>
          Apply(envField("asArrayOf", arrayBaseFieldName(base)),
              List(expr, IntLiteral(depth)))

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
    Apply(envField(helperName), args.toList)
  }

  def encodeClassVar(className: ClassName)(implicit pos: Position): VarRef =
    envField("c", className)

  def genLoadModule(moduleClass: ClassName)(implicit pos: Position): Tree = {
    import TreeDSL._
    Apply(envField("m", moduleClass), Nil)
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
    Apply(envField("a", className), Nil)
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
        val globalVarRef = VarRef(Ident(globalRef, Some(globalRef)))
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
        val moduleValue = envModuleField(module)
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

  def genClassDataOf(typeRef: TypeRef)(implicit pos: Position): Tree = {
    typeRef match {
      case typeRef: PrimRef =>
        genClassDataOf(fieldNameOfPrimRef(typeRef))
      case ClassRef(className) =>
        genClassDataOf(className)
      case ArrayTypeRef(base, dims) =>
        val baseData = envField("d", arrayBaseFieldName(base))
        (1 to dims).foldLeft[Tree](baseData) { (prev, _) =>
          Apply(DotSelect(prev, Ident("getArrayOf")), Nil)
        }
    }
  }

  private def arrayBaseFieldName(base: NonArrayTypeRef): String = base match {
    case base: PrimRef       => fieldNameOfPrimRef(base)
    case ClassRef(className) => className
  }

  def fieldNameOfPrimRef(primRef: PrimRef): String = primRef.tpe match {
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

  def genClassOf(className: String)(implicit pos: Position): Tree =
    Apply(DotSelect(genClassDataOf(className), Ident("getClassOf")), Nil)

  def genClassDataOf(className: String)(implicit pos: Position): Tree =
    envField("d", className)

  def envModuleField(module: String)(implicit pos: Position): VarRef = {
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
      val result = new java.lang.StringBuilder("$i_")
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

    val varName =
      if (containsOnlyValidChars()) "$i_" + module
      else buildValidName()

    VarRef(Ident(avoidClashWithGlobalRef(varName), Some(module)))
  }

  def envField(field: String, subField: String, origName: Option[String] = None)(
      implicit pos: Position): VarRef = {
    VarRef(envFieldIdent(field, subField, origName))
  }

  def envFieldIdent(field: String, subField: String,
      origName: Option[String] = None)(
      implicit pos: Position): Ident = {
    Ident(avoidClashWithGlobalRef("$" + field + "_" + subField), origName)
  }

  def envField(field: String)(implicit pos: Position): VarRef =
    VarRef(envFieldIdent(field))

  def envFieldIdent(field: String)(implicit pos: Position): Ident =
    Ident(avoidClashWithGlobalRef("$" + field))

  def avoidClashWithGlobalRef(envFieldName: String): String = {
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
    if (mentionedDangerousGlobalRefs.contains(envFieldName))
      slowPath(envFieldName)
    else
      envFieldName
  }

  /** Keeps only the global refs that need to be tracked.
   *
   *  By default, only dangerous global refs need to be tracked outside of
   *  functions, to power `mentionedDangerousGlobalRefs` and therefore
   *  `avoidClashWithGlobalRef`. In that case, the set is hopefully already
   *  emptied at this point for the large majority of methods, if not all.
   *
   *  However, when integrating with GCC, we must tell it a list of all the
   *  global variables that are accessed in an externs file. In that case, we
   *  need to track all global variables across functions and classes. This is
   *  slower, but running GCC will take most of the time anyway in that case.
   */
  def keepOnlyTrackedGlobalRefs(globalRefs: Set[String]): Set[String] =
    if (trackAllGlobalRefs) globalRefs
    else GlobalRefUtils.keepOnlyDangerousGlobalRefs(globalRefs)

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
          irt.isValidJSIdentifier(name) && name != "eval" =>
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
