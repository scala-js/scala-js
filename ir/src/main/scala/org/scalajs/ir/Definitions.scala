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

package org.scalajs.ir

import scala.annotation.switch

import Trees.isValidJSIdentifier
import Types._

object Definitions {
  type LocalName <: String

  def LocalName(name: String): LocalName = {
    require(isValidJSIdentifier(name), "Invalid local name: " + name)
    name.asInstanceOf[LocalName]
  }

  type LabelName <: String

  def LabelName(name: String): LabelName = {
    require(isValidJSIdentifier(name), "Invalid label name: " + name)
    name.asInstanceOf[LabelName]
  }

  type FieldName <: String

  def FieldName(name: String): FieldName = {
    require(isValidJSIdentifier(name), "Invalid field name: " + name)
    name.asInstanceOf[FieldName]
  }

  type MethodName <: String

  def MethodName(name: String): MethodName = {
    require(isValidJSIdentifier(name), "Invalid method name: " + name)
    name.asInstanceOf[MethodName]
  }

  type ClassName <: String

  def ClassName(name: String): ClassName = {
    /* Because of pos/t9392, which generates a class whose full name is
     * `<local Client>$C`, we cannot actually validate class names. It has
     * worked so far because that test does not go through the linker. It will
     * properly work again once we delay mangling to the emitter, which is why
     * we avoid the validation rather than disabling the test.
     */
    // require(isValidJSIdentifier(name), "Invalid class name: " + name)
    name.asInstanceOf[ClassName]
  }

  /** `java.lang.Object`, the root of the class hierarchy. */
  val ObjectClass: ClassName = ClassName("O")

  // Hijacked classes
  val BoxedUnitClass: ClassName = ClassName("jl_Void")
  val BoxedBooleanClass: ClassName = ClassName("jl_Boolean")
  val BoxedCharacterClass: ClassName = ClassName("jl_Character")
  val BoxedByteClass: ClassName = ClassName("jl_Byte")
  val BoxedShortClass: ClassName = ClassName("jl_Short")
  val BoxedIntegerClass: ClassName = ClassName("jl_Integer")
  val BoxedLongClass: ClassName = ClassName("jl_Long")
  val BoxedFloatClass: ClassName = ClassName("jl_Float")
  val BoxedDoubleClass: ClassName = ClassName("jl_Double")
  val BoxedStringClass: ClassName = ClassName("T")

  /** The set of all hijacked classes. */
  val HijackedClasses: Set[ClassName] = Set(
      BoxedUnitClass,
      BoxedBooleanClass,
      BoxedCharacterClass,
      BoxedByteClass,
      BoxedShortClass,
      BoxedIntegerClass,
      BoxedLongClass,
      BoxedFloatClass,
      BoxedDoubleClass,
      BoxedStringClass
  )

  /** The class of things returned by `ClassOf` and `GetClass`. */
  val ClassClass: ClassName = ClassName("jl_Class")

  /** Name of a constructor without argument.
   *
   *  This is notably the signature of constructors of module classes.
   */
  final val NoArgConstructorName: MethodName = MethodName("init___")

  /** Name of the static initializer method. */
  final val StaticInitializerName: MethodName = MethodName("clinit___")

  /** Encodes a class name. */
  def encodeClassName(fullName: String): ClassName = {
    val base = fullName.replace("_", "$und").replace(".", "_")
    compressedClasses.getOrElse(base, {
      compressedPrefixes.collectFirst {
        case (prefix, compressed) if base.startsWith(prefix) =>
          ClassName(compressed + base.substring(prefix.length))
      }.getOrElse {
        ClassName("L" + base)
      }
    })
  }

  // !!! Duplicate logic: this code must be in sync with runtime.StackTrace

  /** Decodes a class name encoded with [[encodeClassName]]. */
  def decodeClassName(encodedName: ClassName): String = {
    val base = decompressedClasses.getOrElse(encodedName, {
      decompressedPrefixes.collectFirst {
        case (prefix, decompressed) if encodedName.startsWith(prefix) =>
          decompressed + encodedName.substring(prefix.length)
      } getOrElse {
        assert(!encodedName.isEmpty && encodedName.charAt(0) == 'L',
            s"Cannot decode invalid encoded name '$encodedName'")
        encodedName.substring(1)
      }
    })
    base.replace("_", ".").replace("$und", "_")
  }

  private val compressedClasses: Map[String, ClassName] = Map(
      "java_lang_Object" -> ObjectClass,
      "java_lang_String" -> BoxedStringClass
  ) ++ (
      for (index <- 2 to 22)
        yield s"scala_Tuple$index" -> ClassName("T" + index)
  ) ++ (
      for (index <- 0 to 22)
        yield s"scala_Function$index" -> ClassName("F" + index)
  )

  private val decompressedClasses: Map[ClassName, String] =
    compressedClasses map { case (a, b) => (b, a) }

  private val compressedPrefixes = Seq(
      "scala_scalajs_runtime_" -> "sjsr_",
      "scala_scalajs_" -> "sjs_",
      "scala_collection_immutable_" -> "sci_",
      "scala_collection_mutable_" -> "scm_",
      "scala_collection_generic_" -> "scg_",
      "scala_collection_" -> "sc_",
      "scala_runtime_" -> "sr_",
      "scala_" -> "s_",
      "java_lang_" -> "jl_",
      "java_util_" -> "ju_"
  )

  private val decompressedPrefixes: Seq[(String, String)] =
    compressedPrefixes map { case (a, b) => (b, a) }

  /** Encodes a method name from its full signature. */
  def encodeMethodName(baseName: String, paramTypes: List[TypeRef],
      resultType: Option[TypeRef]): MethodName = {

    val paramTypesString = paramTypes.map(encodeTypeRef).mkString("__")

    if (baseName == "<clinit>") {
      assert(paramTypes.isEmpty && resultType.isEmpty)
      StaticInitializerName
    } else if (baseName == "<init>") {
      assert(resultType.isEmpty)
      paramTypes.map(encodeTypeRef).mkString("init___", "__", "")
        .asInstanceOf[MethodName]
    } else {
      val resultTypeString = resultType.fold("")(encodeTypeRef)
      (paramTypes.map(encodeTypeRef) :+ resultTypeString)
        .mkString(baseName + "__", "__", "")
        .asInstanceOf[MethodName]
    }
  }

  /** Decodes a method name into its full signature. */
  def decodeMethodName(
      encodedName: MethodName): (String, List[TypeRef], Option[TypeRef]) = {
    val (simpleName, privateAndSigString) = if (isConstructorName(encodedName)) {
      val privateAndSigString =
        if (encodedName == "init___") ""
        else encodedName.stripPrefix("init___") + "__"
      ("<init>", privateAndSigString)
    } else if (encodedName == StaticInitializerName) {
      ("<clinit>", "")
    } else {
      val pos = encodedName.indexOf("__")
      (encodedName.substring(0, pos), encodedName.substring(pos + 2))
    }

    // -1 preserves trailing empty strings
    val paramsAndResultStrings = privateAndSigString.split("__", -1).toSeq
    val paramStrings :+ resultString = paramsAndResultStrings

    val paramTypes = paramStrings.map(decodeTypeRef).toList
    val resultType =
      if (resultString == "") None // constructor or reflective proxy
      else Some(decodeTypeRef(resultString))

    (simpleName, paramTypes, resultType)
  }

  /** Encodes a [[Types.TypeRef]] to be used for example in an encoded method
   *  signature.
   */
  def encodeTypeRef(typeRef: TypeRef): String = {
    typeRef match {
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
        className
      case ArrayTypeRef(base, dimensions) =>
        "A" * dimensions + encodeTypeRef(base)
    }
  }

  /** Decodes a [[Types.TypeRef]], such as in an encoded method signature.
   */
  def decodeTypeRef(encodedName: String): TypeRef = {
    val arrayDepth = encodedName.indexWhere(_ != 'A')
    val base = {
      if (arrayDepth == encodedName.length() - 1) {
        (encodedName.charAt(arrayDepth): @switch) match {
          case 'V' => VoidRef
          case 'Z' => BooleanRef
          case 'C' => CharRef
          case 'B' => ByteRef
          case 'S' => ShortRef
          case 'I' => IntRef
          case 'J' => LongRef
          case 'F' => FloatRef
          case 'D' => DoubleRef
          case 'N' => NullRef
          case 'E' => NothingRef
          case _   => ClassRef(ClassName(encodedName.substring(arrayDepth)))
        }
      } else {
        ClassRef(ClassName(encodedName.substring(arrayDepth)))
      }
    }
    if (arrayDepth == 0)
      base
    else
      ArrayTypeRef(base, arrayDepth)
  }

  /* Common predicates on encoded names */

  def isConstructorName(name: MethodName): Boolean =
    name.startsWith("init___")

  def isReflProxyName(name: MethodName): Boolean = {
    name.endsWith("__") &&
    !isConstructorName(name) &&
    name != StaticInitializerName
  }

}
