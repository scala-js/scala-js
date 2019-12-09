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

import scala.collection.mutable

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
    internalOptions: InternalOptions,
    mentionedDangerousGlobalRefs: Set[String]) {

  import JSGen._

  val useClasses = esFeatures.useECMAScript2015

  val useArrowFunctions = esFeatures.useECMAScript2015

  val useBigIntForLongs = esFeatures.allowBigIntsForLongs

  val trackAllGlobalRefs = internalOptions.trackAllGlobalRefs

  private val genLocalNameCache = {
    /* Fill the cache with reserved JS identifiers so that we do not have to
     * deal with avoiding them in `genName`. We append '\u00f8' to their
     * translation, which is the first non-ASCII valid JS identifier start, and
     * does not collide with any other encoding performed by `genName`.
     */
    val cache = mutable.Map.empty[LocalName, String]
    for (reserved <- ReservedJSIdentifierNames)
      cache.put(LocalName(reserved), reserved + "\u00f8")
    cache
  }

  private val genLabelNameCache = {
    // Same as genLocalNameCache
    val cache = mutable.Map.empty[LabelName, String]
    for (reserved <- ReservedJSIdentifierNames)
      cache.put(LabelName(reserved), reserved + "\u00f8")
    cache
  }

  private val genFieldNameCache =
    mutable.Map.empty[FieldName, String]

  private val genMethodNameCache =
    mutable.Map.empty[MethodName, String]

  private val genClassNameCache = {
    /* Fill the cache with the compressed form of java.lang.Object,
     * java.lang.String and org.scalajs.linker.runtime.RuntimeLong, so that we
     * do not have to take care of them in genName(ClassName).
     */
    val cache = mutable.Map.empty[ClassName, String]
    cache.put(ObjectClass, "O")
    cache.put(BoxedStringClass, "T")
    cache.put(LongImpl.RuntimeLongClass, "RTLong")
    cache.put(LongImpl.RuntimeLongModuleClass, "RTLong$")
    cache
  }

  private def genNameGeneric[N <: Name](name: N,
      cache: mutable.Map[N, String]): String = {

    cache.getOrElseUpdate(name, {
      val encoded = name.encoded
      val len = encoded.length
      val result = new Array[Char](len)
      result(0) = startByteToChar(encoded(0) & 0xff)
      var i = 1
      while (i != len) {
        val b = encoded(i) & 0xff
        if (b == '_' && encoded(i - 1) == '_')
          result(i) = FullwidthSpacingUnderscore
        else
          result(i) = partByteToChar(b)
        i += 1
      }
      new String(result)
    })
  }

  def genName(name: LocalName): String = {
    genLocalNameCache.getOrElseUpdate(name, {
      val encoded = name.encoded
      val len = encoded.length
      val result = new Array[Char](len)
      result(0) = localStartByteToChar(encoded(0) & 0xff)
      var i = 1
      while (i != len) {
        val b = encoded(i) & 0xff
        if (b == '_' && encoded(i - 1) == '_')
          result(i) = FullwidthSpacingUnderscore
        else
          result(i) = partByteToChar(b)
        i += 1
      }
      new String(result)
    })
  }

  def genName(name: LabelName): String = genNameGeneric(name, genLabelNameCache)
  def genName(name: FieldName): String = genNameGeneric(name, genFieldNameCache)

  def genName(name: MethodName): String = {
    genMethodNameCache.getOrElseUpdate(name, {
      val builder = new java.lang.StringBuilder()

      // For constructors and static initializers, we only need the param type refs
      val onlyParamTypeRefs = name.isConstructor || name.isStaticInitializer

      // First encode the simple name
      if (!onlyParamTypeRefs) {
        val encoded = name.simpleName.encoded
        builder.append(startByteToChar(encoded(0) & 0xff))
        val len = encoded.length
        var i = 1
        while (i != len) {
          val b = encoded(i) & 0xff
          // Avoid '__' in the output as that must be the end of the simple name
          if (b == '_' && encoded(i - 1) == '_')
            builder.append(FullwidthSpacingUnderscore)
          else
            builder.append(partByteToChar(b))
          i += 1
        }
        builder.append('_').append('_')
      }

      def appendTypeRef(typeRef: TypeRef): Unit = {
        typeRef match {
          case PrimRef(tpe) =>
            tpe match {
              case NoType      => builder.append('V')
              case BooleanType => builder.append('Z')
              case CharType    => builder.append('C')
              case ByteType    => builder.append('B')
              case ShortType   => builder.append('S')
              case IntType     => builder.append('I')
              case LongType    => builder.append('J')
              case FloatType   => builder.append('F')
              case DoubleType  => builder.append('D')
              case NullType    => builder.append('N')
              case NothingType => builder.append('E')
            }
          case ClassRef(className) =>
            builder.append(genName(className))
          case ArrayTypeRef(base, dimensions) =>
            var i = 0
            while (i != dimensions) {
              builder.append('A')
              i += 1
            }
            appendTypeRef(base)
        }
      }

      for (typeRef <- name.paramTypeRefs) {
        appendTypeRef(typeRef)
        builder.append('_').append('_')
      }

      if (!onlyParamTypeRefs && !name.isReflectiveProxy)
        appendTypeRef(name.resultTypeRef)

      builder.toString()
    })
  }

  def genName(name: ClassName): String = {
    genClassNameCache.getOrElseUpdate(name, {
      val encoded = name.encoded
      val len = encoded.length
      val builder = new java.lang.StringBuilder(len + 1)

      // Handle compressed prefixes
      var i = compressedPrefixes.find(pair => encodedNameStartsWith(encoded, pair._1, 0)) match {
        case None =>
          builder.append('L')
          0
        case Some(pair) =>
          builder.append(pair._2)
          pair._1.length
      }

      // Encode the rest
      while (i != len) {
        builder.append(classByteToChar(encoded(i) & 0xff))
        i += 1
      }

      builder.toString()
    })
  }

  def genOriginalName(name: Name, originalName: OriginalName,
      jsName: String): OriginalName = {
    genOriginalName(name.encoded, originalName, jsName)
  }

  def genOriginalName(name: MethodName, originalName: OriginalName,
      jsName: String): OriginalName = {
    genOriginalName(name.simpleName, originalName, jsName)
  }

  private def genOriginalName(name: UTF8String, originalName: OriginalName,
      jsName: String): OriginalName = {

    def sameName: Boolean = {
      /* This method compares a UTF-8 string and a (UTF-16) string
       * element-wise, thus comparing bytes with chars, in order to avoid any
       * recoding. We can do this here because:
       *
       * - for ASCII characters, the byte and char values are the same
       * - for non-ASCII characters, the byte value is always negative while
       *   the char values are always positive, so the comparison is always
       *   false
       * - the always-false result for non-ASCII characters is correct in the
       *   case of `JSGen`, because all non-ASCII code points in `Name`s are
       *   encoded in `genName()` byte-by-byte into Chars that have lost all
       *   connection to what they meant, so any non-ASCII character will
       *   require an original name to be generated.
       */

      // scalastyle:off return
      if (name.length != jsName.length())
        return false
      var i = 0
      while (i != name.length) {
        if (name(i).toInt != jsName.charAt(i).toInt)
          return false
        i += 1
      }
      true
      // scalastyle:on return
    }

    if (originalName.isDefined) originalName
    else if (sameName) NoOriginalName
    else OriginalName(name)
  }

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
      codegenVar("L0")
  }

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
      implicit pos: Position): VarRef = {
    codegenVar("t", className, item.name)
  }

  def genJSPrivateSelect(receiver: Tree, className: ClassName,
      field: irt.FieldIdent)(
      implicit pos: Position): Tree = {
    BracketSelect(receiver,
        genJSPrivateFieldIdent(className, field)(field.pos))
  }

  def genJSPrivateFieldIdent(className: ClassName, field: irt.FieldIdent)(
      implicit pos: Position): Tree = {
    codegenVar("r", className, field.name)
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
          Apply(codegenVar("is", className), List(expr))
        }

      case ArrayType(ArrayTypeRef(base, depth)) =>
        Apply(codegenVar("isArrayOf", base), List(expr, IntLiteral(depth)))

      case UndefType   => expr === Undefined()
      case BooleanType => typeof(expr) === "boolean"
      case CharType    => expr instanceof codegenVar("Char")
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
      case BoxedCharacterClass => expr instanceof codegenVar("Char")
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
          Apply(codegenVar("as", className), List(expr))

        case ArrayType(ArrayTypeRef(base, depth)) =>
          Apply(codegenVar("asArrayOf", base), List(expr, IntLiteral(depth)))

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
    Apply(codegenVar(helperName), args.toList)
  }

  def encodeClassVar(className: ClassName)(implicit pos: Position): VarRef =
    codegenVar("c", className)

  def genLoadModule(moduleClass: ClassName)(implicit pos: Position): Tree = {
    import TreeDSL._
    Apply(codegenVar("m", moduleClass), Nil)
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
    Apply(codegenVar("a", className), Nil)
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

  def genClassOf(className: ClassName)(implicit pos: Position): Tree =
    genClassOf(ClassRef(className))

  def genClassDataOf(typeRef: TypeRef)(implicit pos: Position): Tree = {
    typeRef match {
      case typeRef: NonArrayTypeRef =>
        codegenVar("d", typeRef)
      case ArrayTypeRef(base, dims) =>
        val baseData = genClassDataOf(base)
        (1 to dims).foldLeft[Tree](baseData) { (prev, _) =>
          Apply(DotSelect(prev, Ident("getArrayOf")), Nil)
        }
    }
  }

  def genClassDataOf(className: ClassName)(implicit pos: Position): Tree =
    genClassDataOf(ClassRef(className))

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

    VarRef(Ident(avoidClashWithGlobalRef(varName), OriginalName(module)))
  }

  def codegenVar(field: String, typeRef: NonArrayTypeRef)(
      implicit pos: Position): VarRef = {
    VarRef(codegenVarIdent(field, typeRef))
  }

  def codegenVar(field: String, className: ClassName)(implicit pos: Position): VarRef =
    codegenVar(field, genName(className))

  def codegenVar(field: String, className: ClassName, fieldName: FieldName)(
      implicit pos: Position): VarRef = {
    codegenVar(field, className, fieldName, NoOriginalName)
  }

  def codegenVar(field: String, className: ClassName, fieldName: FieldName,
      origName: OriginalName)(
      implicit pos: Position): VarRef = {
    codegenVar(field, genName(className) + "__" + genName(fieldName), origName)
  }

  def codegenVar(field: String, className: ClassName, methodName: MethodName)(
      implicit pos: Position): VarRef = {
    codegenVar(field, className, methodName, NoOriginalName)
  }

  def codegenVar(field: String, className: ClassName, methodName: MethodName,
      origName: OriginalName)(
      implicit pos: Position): VarRef = {
    codegenVar(field, genName(className) + "__" + genName(methodName), origName)
  }

  def codegenVar(field: String, subField: String,
      origName: OriginalName = NoOriginalName)(
      implicit pos: Position): VarRef = {
    VarRef(codegenVarIdent(field, subField, origName))
  }

  def codegenVarIdent(field: String, typeRef: NonArrayTypeRef)(
      implicit pos: Position): Ident = {
    // The mapping in this function is an implementation detail of the emitter
    val subField = typeRef match {
      case PrimRef(tpe) =>
        tpe match {
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
      case ClassRef(className) =>
        genName(className)
    }
    codegenVarIdent(field, subField)
  }

  def codegenVarIdent(field: String, subField: String,
      origName: OriginalName = NoOriginalName)(
      implicit pos: Position): Ident = {
    Ident(avoidClashWithGlobalRef("$" + field + "_" + subField), origName)
  }

  def codegenVar(field: String)(implicit pos: Position): VarRef =
    VarRef(codegenVarIdent(field))

  def codegenVarIdent(field: String)(implicit pos: Position): Ident =
    codegenVarIdent(field, NoOriginalName)

  def codegenVarIdent(field: String, origName: OriginalName)(
      implicit pos: Position): Ident = {
    Ident(avoidClashWithGlobalRef("$" + field), origName)
  }

  def avoidClashWithGlobalRef(codegenVarName: String): String = {
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

private[emitter] object JSGen {

  private final val FullwidthSpacingUnderscore = '\uff3f'
  private final val GreekSmallLetterDelta = '\u03b4'

  private val startByteToChar: Array[Char] = {
    /* The code points 256 through (512 - 1) are all valid start characters for
     * JavaScript identifiers. We encode each invalid byte as one of those
     * characters. Valid ASCII start chars are encoded as themselves
     */
    val table = Array.tabulate(256)(i => (256 + i).toChar)
    for (b <- 'A'.toInt to 'Z'.toInt)
      table(b) = b.toChar
    for (b <- 'a'.toInt to 'z'.toInt)
      table(b) = b.toChar
    table('$'.toInt) = '$'
    table('_'.toInt) = '_'
    table
  }

  private val partByteToChar: Array[Char] = {
    /* Once not at the start anymore, ASCII digits are also encoded as
     * themselves.
     */
    val table = startByteToChar.clone()
    for (b <- '0'.toInt to '9'.toInt)
      table(b) = b.toChar
    table
  }

  private val localStartByteToChar: Array[Char] = {
    /* Local variables must not start with a '$', otherwise they might clash
     * with compiler-generated variables such as codegenVars or the `$thiz` of
     * default methods (#2972).
     * We rewrite a starting '$' as '\u03b4' (greek small letter delta), which
     * is an arbitrary choice based on the sound ('dollar' starts with the
     * sound of delta).
     */
    val table = startByteToChar.clone()
    table('$'.toInt) = GreekSmallLetterDelta
    table
  }

  private val classByteToChar: Array[Char] = {
    /* For class names, '.' are rewritten as '_' and '_' as '\uff3f'. We can
     * use the 'part' table even for start characters because class names are
     * always prefixed by something in our encoding.
     */
    val table = partByteToChar.clone()
    table('.'.toInt) = '_'
    table('_'.toInt) = FullwidthSpacingUnderscore
    table
  }

  /** Set of identifier names that cannot or should not be used for variable
   *  names.
   *
   *  This set includes and is limited to:
   *
   *  - All ECMAScript 2015 keywords;
   *  - Identifier names that are treated as keywords in ECMAScript 2015
   *    Strict Mode;
   *  - The identifiers `arguments` and `eval`, because they cannot be used for
   *    local variable names in ECMAScript 2015 Strict Mode;
   *  - The identifier `undefined`, because that's way too confusing if it does
   *    not actually mean `void 0`, and who knows what JS engine performance
   *    cliffs we can trigger with that.
   */
  private final val ReservedJSIdentifierNames: Set[String] = Set(
      "arguments", "break", "case", "catch", "class", "const", "continue",
      "debugger", "default", "delete", "do", "else", "enum", "eval", "export",
      "extends", "false", "finally", "for", "function", "if", "implements",
      "import", "in", "instanceof", "interface", "let", "new", "null",
      "package", "private", "protected", "public", "return", "static", "super",
      "switch", "this", "throw", "true", "try", "typeof", "undefined", "var",
      "void", "while", "with", "yield"
  )

  private val compressedPrefixes: List[(UTF8String, String)] = {
    List(
        "java.lang." -> "jl_",
        "java.util." -> "ju_",
        "scala.collection.immutable." -> "sci_",
        "scala.collection.mutable." -> "scm_",
        "scala.collection.generic." -> "scg_",
        "scala.collection." -> "sc_",
        "scala.runtime." -> "sr_",
        "scala.scalajs.runtime." -> "sjsr_",
        "scala.scalajs." -> "sjs_",
        "scala.Function" -> "F",
        "scala.Tuple" -> "T",
        "scala." -> "s_"
    ).map { pair =>
      UTF8String(pair._1) -> pair._2
    }
  }

  private def encodedNameStartsWith(encoded: UTF8String, prefix: UTF8String,
      start: Int): Boolean = {
    // scalastyle:off return
    val prefixLen = prefix.length
    if (start + prefixLen > encoded.length)
      return false
    var i = 0
    while (i != prefixLen) {
      if (encoded(start + i) != prefix(i))
        return false
      i += 1
    }
    true
    // scalastyle:on return
  }

}
