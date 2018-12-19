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
import ir.Types._
import ir.{Trees => irt}

import org.scalajs.linker._
import org.scalajs.linker.backend.javascript.Trees._

/** Collection of tree generators that are used accross the board.
 *  This class is fully stateless.
 *
 *  Also carries around config (semantics and esFeatures).
 */
private[emitter] final class JSGen(val semantics: Semantics,
    val esFeatures: ESFeatures, val moduleKind: ModuleKind,
    internalOptions: InternalOptions,
    mentionedDangerousGlobalRefs: Set[String]) {

  import JSGen._

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
      envField("L0")
  }

  def genLongModuleApply(methodName: String, args: Tree*)(
      implicit pos: Position): Tree = {
    import TreeDSL._
    Apply(
        genLoadModule(LongImpl.RuntimeLongModuleClass) DOT methodName,
        args.toList)
  }

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

  def genSelectStatic(className: String, item: irt.Ident)(
      implicit pos: Position): VarRef = {
    envField("t", className + "__" + item.name)
  }

  def genIsInstanceOf(expr: Tree, typeRef: TypeRef)(
      implicit pos: Position): Tree =
    genIsAsInstanceOf(expr, typeRef, test = true)

  def genAsInstanceOf(expr: Tree, typeRef: TypeRef)(
      implicit pos: Position): Tree =
    genIsAsInstanceOf(expr, typeRef, test = false)

  private def genIsAsInstanceOf(expr: Tree, typeRef: TypeRef, test: Boolean)(
      implicit pos: Position): Tree = {
    import Definitions._
    import TreeDSL._

    typeRef match {
      case ClassRef(className0) =>
        val className =
          if (className0 == BoxedLongClass && !useBigIntForLongs) LongImpl.RuntimeLongClass
          else className0

        if (HijackedClasses.contains(className) && className != BoxedStringClass) {
          if (test) {
            className match {
              case BoxedUnitClass      => expr === Undefined()
              case BoxedBooleanClass   => typeof(expr) === "boolean"
              case BoxedCharacterClass => genCallHelper("isChar", expr)
              case BoxedByteClass      => genCallHelper("isByte", expr)
              case BoxedShortClass     => genCallHelper("isShort", expr)
              case BoxedIntegerClass   => genCallHelper("isInt", expr)
              case BoxedLongClass      => genCallHelper("isLong", expr)
              case BoxedFloatClass     => genCallHelper("isFloat", expr)
              case BoxedDoubleClass    => typeof(expr) === "number"
            }
          } else {
            className match {
              case BoxedUnitClass      => genCallHelper("asUnit", expr)
              case BoxedBooleanClass   => genCallHelper("asBoolean", expr)
              case BoxedCharacterClass => genCallHelper("asChar", expr)
              case BoxedByteClass      => genCallHelper("asByte", expr)
              case BoxedShortClass     => genCallHelper("asShort", expr)
              case BoxedIntegerClass   => genCallHelper("asInt", expr)
              case BoxedLongClass      => genCallHelper("asLong", expr)
              case BoxedFloatClass     => genCallHelper("asFloat", expr)
              case BoxedDoubleClass    => genCallHelper("asDouble", expr)
            }
          }
        } else {
          Apply(
              envField(if (test) "is" else "as", className),
              List(expr))
        }

      case ArrayTypeRef(base, depth) =>
        Apply(
            envField(if (test) "isArrayOf" else "asArrayOf", base),
            List(expr, IntLiteral(depth)))
    }
  }

  def genCallHelper(helperName: String, args: Tree*)(
      implicit pos: Position): Tree = {
    Apply(envField(helperName), args.toList)
  }

  def encodeClassVar(className: String)(implicit pos: Position): VarRef =
    envField("c", className)

  def genLoadModule(moduleClass: String)(implicit pos: Position): Tree = {
    import TreeDSL._
    Apply(envField("m", moduleClass), Nil)
  }

  def genJSClassConstructor(className: String,
      keepOnlyDangerousVarNames: Boolean)(
      implicit globalKnowledge: GlobalKnowledge,
      pos: Position): WithGlobals[Tree] = {

    genJSClassConstructor(className,
        globalKnowledge.getJSNativeLoadSpec(className),
        keepOnlyDangerousVarNames)
  }

  def genJSClassConstructor(className: String,
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

  def genNonNativeJSClassConstructor(className: String)(
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
          if (keepOnlyDangerousVarNames && !GlobalRefUtils.isDangerousGlobalRef(globalRef))
            Set.empty[String]
          else
            Set(globalRef)
        }
        WithGlobals(pathSelection(globalVarRef, path), globalVarNames)

      case irt.JSNativeLoadSpec.Import(module, path) =>
        val moduleValue = envModuleField(module)
        path match {
          case DefaultExportName :: rest if moduleKind == ModuleKind.CommonJSModule =>
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

  def genArrayValue(tpe: ArrayType, elems: List[Tree])(
      implicit pos: Position): Tree = {
    genCallHelper("makeNativeArrayWrapper", genClassDataOf(tpe.arrayTypeRef),
        ArrayConstr(elems))
  }

  def genClassDataOf(typeRef: TypeRef)(implicit pos: Position): Tree = {
    typeRef match {
      case ClassRef(className) =>
        envField("d", className)
      case ArrayTypeRef(base, dims) =>
        (1 to dims).foldLeft[Tree](envField("d", base)) { (prev, _) =>
          Apply(DotSelect(prev, Ident("getArrayOf")), Nil)
        }
    }
  }

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
    VarRef(Ident(avoidClashWithGlobalRef("$" + field)))

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
    if (internalOptions.trackAllGlobalRefs) globalRefs
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
          irt.isValidIdentifier(name) && name != "eval" =>
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

private object JSGen {
  private final val ScalaJSEnvironmentName = "ScalaJS"
  private final val DefaultExportName = "default"
}
